//===- bolt/tools/merge-fdata/merge-fdata.cpp -----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Tool for merging profile in fdata format:
//
//   $ merge-fdata 1.fdata 2.fdata 3.fdata > merged.fdata
//
//===----------------------------------------------------------------------===//

#include "bolt/Profile/ProfileYAMLMapping.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/ThreadPool.h"
#include <algorithm>
#include <fstream>
#include <mutex>
#include <unordered_map>

using namespace llvm;
using namespace llvm::yaml::bolt;

namespace opts {

static cl::OptionCategory MergeFdataCategory("merge-fdata options");

enum SortType : char {
  ST_NONE,
  ST_EXEC_COUNT,      /// Sort based on function execution count.
  ST_TOTAL_BRANCHES,  /// Sort based on all branches in the function.
};

static cl::list<std::string>
InputDataFilenames(
  cl::Positional,
  cl::CommaSeparated,
  cl::desc("<fdata1> [<fdata2>]..."),
  cl::OneOrMore,
  cl::cat(MergeFdataCategory));

static cl::opt<SortType>
PrintFunctionList("print",
  cl::desc("print the list of objects with count to stderr"),
  cl::init(ST_NONE),
  cl::values(clEnumValN(ST_NONE,
      "none",
      "do not print objects/functions"),
    clEnumValN(ST_EXEC_COUNT,
      "exec",
      "print functions sorted by execution count"),
    clEnumValN(ST_TOTAL_BRANCHES,
      "branches",
      "print functions sorted by total branch count")),
  cl::cat(MergeFdataCategory));

static cl::opt<bool>
SuppressMergedDataOutput("q",
  cl::desc("do not print merged data to stdout"),
  cl::init(false),
  cl::Optional,
  cl::cat(MergeFdataCategory));

static cl::opt<std::string>
OutputFilePath("o",
  cl::value_desc("file"),
  cl::desc("Write output to <file>"),
  cl::cat(MergeFdataCategory));

} // namespace opts

namespace {

static StringRef ToolName;

static void report_error(StringRef Message, std::error_code EC) {
  assert(EC);
  errs() << ToolName << ": '" << Message << "': " << EC.message() << ".\n";
  exit(1);
}

static void report_error(Twine Message, StringRef CustomError) {
  errs() << ToolName << ": '" << Message << "': " << CustomError << ".\n";
  exit(1);
}

static raw_fd_ostream &output() {
  if (opts::OutputFilePath.empty() || opts::OutputFilePath == "-")
    return outs();
  else {
    std::error_code EC;
    static raw_fd_ostream Output(opts::OutputFilePath, EC);
    if (EC)
      report_error(opts::OutputFilePath, EC);
    return Output;
  }
}

void mergeProfileHeaders(BinaryProfileHeader &MergedHeader,
                         const BinaryProfileHeader &Header) {
  if (MergedHeader.FileName.empty())
    MergedHeader.FileName = Header.FileName;

  if (!MergedHeader.FileName.empty() &&
      MergedHeader.FileName != Header.FileName)
    errs() << "WARNING: merging profile from a binary for " << Header.FileName
           << " into a profile for binary " << MergedHeader.FileName << '\n';

  if (MergedHeader.Id.empty())
    MergedHeader.Id = Header.Id;

  if (!MergedHeader.Id.empty() && (MergedHeader.Id != Header.Id))
    errs() << "WARNING: build-ids in merged profiles do not match\n";

  // Cannot merge samples profile with LBR profile.
  if (!MergedHeader.Flags)
    MergedHeader.Flags = Header.Flags;

  constexpr auto Mask = llvm::bolt::BinaryFunction::PF_BRANCH |
                        llvm::bolt::BinaryFunction::PF_BASIC;
  if ((MergedHeader.Flags & Mask) != (Header.Flags & Mask)) {
    errs() << "ERROR: cannot merge LBR profile with non-LBR profile\n";
    exit(1);
  }
  MergedHeader.Flags = MergedHeader.Flags | Header.Flags;

  if (!Header.Origin.empty()) {
    if (MergedHeader.Origin.empty())
      MergedHeader.Origin = Header.Origin;
    else if (MergedHeader.Origin != Header.Origin)
      MergedHeader.Origin += "; " + Header.Origin;
  }

  if (MergedHeader.EventNames.empty())
    MergedHeader.EventNames = Header.EventNames;

  if (MergedHeader.EventNames != Header.EventNames) {
    errs() << "WARNING: merging profiles with different sampling events\n";
    MergedHeader.EventNames += "," + Header.EventNames;
  }

  if (MergedHeader.HashFunction != Header.HashFunction)
    report_error("merge conflict",
                 "cannot merge profiles with different hash functions");
}

void mergeBasicBlockProfile(BinaryBasicBlockProfile &MergedBB,
                            BinaryBasicBlockProfile &&BB,
                            const BinaryFunctionProfile &BF) {
  // Verify that the blocks match.
  if (BB.NumInstructions != MergedBB.NumInstructions)
    report_error(BF.Name + " : BB #" + Twine(BB.Index),
                 "number of instructions in block mismatch");
  if (BB.Hash != MergedBB.Hash)
    report_error(BF.Name + " : BB #" + Twine(BB.Index),
                 "basic block hash mismatch");

  // Update the execution count.
  MergedBB.ExecCount += BB.ExecCount;

  // Update the event count.
  MergedBB.EventCount += BB.EventCount;

  // Merge calls sites.
  std::unordered_map<uint32_t, CallSiteInfo *> CSByOffset;
  for (CallSiteInfo &CS : BB.CallSites)
    CSByOffset.emplace(std::make_pair(CS.Offset, &CS));

  for (CallSiteInfo &MergedCS : MergedBB.CallSites) {
    auto CSI = CSByOffset.find(MergedCS.Offset);
    if (CSI == CSByOffset.end())
      continue;
    yaml::bolt::CallSiteInfo &CS = *CSI->second;
    if (CS != MergedCS)
      continue;

    MergedCS.Count += CS.Count;
    MergedCS.Mispreds += CS.Mispreds;

    CSByOffset.erase(CSI);
  }

  // Append the rest of call sites.
  for (std::pair<const uint32_t, CallSiteInfo *> CSI : CSByOffset)
    MergedBB.CallSites.emplace_back(std::move(*CSI.second));

  // Merge successor info.
  std::vector<SuccessorInfo *> SIByIndex(BF.NumBasicBlocks);
  for (SuccessorInfo &SI : BB.Successors) {
    if (SI.Index >= BF.NumBasicBlocks)
      report_error(BF.Name, "bad successor index");
    SIByIndex[SI.Index] = &SI;
  }
  for (SuccessorInfo &MergedSI : MergedBB.Successors) {
    if (!SIByIndex[MergedSI.Index])
      continue;
    SuccessorInfo &SI = *SIByIndex[MergedSI.Index];

    MergedSI.Count += SI.Count;
    MergedSI.Mispreds += SI.Mispreds;

    SIByIndex[MergedSI.Index] = nullptr;
  }
  for (SuccessorInfo *SI : SIByIndex)
    if (SI)
      MergedBB.Successors.emplace_back(std::move(*SI));
}

void mergeFunctionProfile(BinaryFunctionProfile &MergedBF,
                          BinaryFunctionProfile &&BF) {
  // Validate that we are merging the correct function.
  if (BF.NumBasicBlocks != MergedBF.NumBasicBlocks)
    report_error(BF.Name, "number of basic blocks mismatch");
  if (BF.Id != MergedBF.Id)
    report_error(BF.Name, "ID mismatch");
  if (BF.Hash != MergedBF.Hash)
    report_error(BF.Name, "hash mismatch");

  // Update the execution count.
  MergedBF.ExecCount += BF.ExecCount;

  // Merge basic blocks profile.
  std::vector<BinaryBasicBlockProfile *> BlockByIndex(BF.NumBasicBlocks);
  for (BinaryBasicBlockProfile &BB : BF.Blocks) {
    if (BB.Index >= BF.NumBasicBlocks)
      report_error(BF.Name + " : BB #" + Twine(BB.Index),
                   "bad basic block index");
    BlockByIndex[BB.Index] = &BB;
  }
  for (BinaryBasicBlockProfile &MergedBB : MergedBF.Blocks) {
    if (!BlockByIndex[MergedBB.Index])
      continue;
    BinaryBasicBlockProfile &BB = *BlockByIndex[MergedBB.Index];

    mergeBasicBlockProfile(MergedBB, std::move(BB), MergedBF);

    // Ignore this block in the future.
    BlockByIndex[MergedBB.Index] = nullptr;
  }

  // Append blocks unique to BF (i.e. those that are not in MergedBF).
  for (BinaryBasicBlockProfile *BB : BlockByIndex)
    if (BB)
      MergedBF.Blocks.emplace_back(std::move(*BB));
}

bool isYAML(const StringRef Filename) {
  ErrorOr<std::unique_ptr<MemoryBuffer>> MB =
      MemoryBuffer::getFileOrSTDIN(Filename);
  if (std::error_code EC = MB.getError())
    report_error(Filename, EC);
  StringRef Buffer = MB.get()->getBuffer();
  if (Buffer.starts_with("---\n"))
    return true;
  return false;
}

void mergeLegacyProfiles(const SmallVectorImpl<std::string> &Filenames) {
  errs() << "Using legacy profile format.\n";
  std::optional<bool> BoltedCollection;
  std::optional<bool> NoLBRCollection;
  std::mutex BoltedCollectionMutex;
  struct CounterTy {
    uint64_t Exec{0};
    uint64_t Mispred{0};
    CounterTy &operator+=(const CounterTy &O) {
      Exec += O.Exec;
      Mispred += O.Mispred;
      return *this;
    }
    CounterTy operator+(const CounterTy &O) { return *this += O; }
  };
  typedef StringMap<CounterTy> ProfileTy;

  auto ParseProfile = [&](const std::string &Filename, auto &Profiles) {
    const llvm::thread::id tid = llvm::this_thread::get_id();

    if (isYAML(Filename))
      report_error(Filename, "cannot mix YAML and legacy formats");

    std::ifstream FdataFile(Filename, std::ios::in);
    std::string FdataLine;
    std::getline(FdataFile, FdataLine);

    auto checkMode = [&](const std::string &Key, std::optional<bool> &Flag) {
      const bool KeyIsSet = FdataLine.rfind(Key, 0) == 0;

      if (!Flag.has_value())
        Flag = KeyIsSet;
      else if (*Flag != KeyIsSet)
        report_error(Filename, "cannot mix profile with and without " + Key);
      if (KeyIsSet)
        // Advance line
        std::getline(FdataFile, FdataLine);
    };

    ProfileTy *Profile;
    {
      std::lock_guard<std::mutex> Lock(BoltedCollectionMutex);
      // Check if the string "boltedcollection" is in the first line
      checkMode("boltedcollection", BoltedCollection);
      // Check if the string "no_lbr" is in the first line
      // (or second line if BoltedCollection is true)
      checkMode("no_lbr", NoLBRCollection);
      Profile = &Profiles[tid];
    }

    do {
      StringRef Line(FdataLine);
      CounterTy Count;
      auto [Signature, ExecCount] = Line.rsplit(' ');
      if (ExecCount.getAsInteger(10, Count.Exec))
        report_error(Filename, "Malformed / corrupted execution count");
      // Only LBR profile has misprediction field
      if (!NoLBRCollection.value_or(false)) {
        auto [SignatureLBR, MispredCount] = Signature.rsplit(' ');
        Signature = SignatureLBR;
        if (MispredCount.getAsInteger(10, Count.Mispred))
          report_error(Filename, "Malformed / corrupted misprediction count");
      }

      Count += Profile->lookup(Signature);
      Profile->insert_or_assign(Signature, Count);
    } while (std::getline(FdataFile, FdataLine));
  };

  // The final reduction has non-trivial cost, make sure each thread has at
  // least 4 tasks.
  ThreadPoolStrategy S = optimal_concurrency(
      std::max(Filenames.size() / 4, static_cast<size_t>(1)));
  DefaultThreadPool Pool(S);
  DenseMap<llvm::thread::id, ProfileTy> ParsedProfiles(
      Pool.getMaxConcurrency());
  for (const auto &Filename : Filenames)
    Pool.async(ParseProfile, std::cref(Filename), std::ref(ParsedProfiles));
  Pool.wait();

  ProfileTy MergedProfile;
  for (const auto &[Thread, Profile] : ParsedProfiles)
    for (const auto &[Key, Value] : Profile) {
      CounterTy Count = MergedProfile.lookup(Key) + Value;
      MergedProfile.insert_or_assign(Key, Count);
    }

  if (BoltedCollection.value_or(false))
    output() << "boltedcollection\n";
  if (NoLBRCollection.value_or(false))
    output() << "no_lbr\n";
  for (const auto &[Key, Value] : MergedProfile) {
    output() << Key << " ";
    if (!NoLBRCollection.value_or(false))
      output() << Value.Mispred << " ";
    output() << Value.Exec << "\n";
  }

  errs() << "Profile from " << Filenames.size() << " files merged.\n";
}

} // anonymous namespace

int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);

  llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  cl::HideUnrelatedOptions(opts::MergeFdataCategory);

  cl::ParseCommandLineOptions(argc, argv,
                              "merge multiple fdata into a single file");

  ToolName = argv[0];

  // Recursively expand input directories into input file lists.
  SmallVector<std::string> Inputs;
  for (std::string &InputDataFilename : opts::InputDataFilenames) {
    if (!llvm::sys::fs::exists(InputDataFilename))
      report_error(InputDataFilename,
                   std::make_error_code(std::errc::no_such_file_or_directory));
    if (llvm::sys::fs::is_regular_file(InputDataFilename))
      Inputs.emplace_back(InputDataFilename);
    else if (llvm::sys::fs::is_directory(InputDataFilename)) {
      std::error_code EC;
      for (llvm::sys::fs::recursive_directory_iterator F(InputDataFilename, EC),
           E;
           F != E && !EC; F.increment(EC))
        if (llvm::sys::fs::is_regular_file(F->path()))
          Inputs.emplace_back(F->path());
      if (EC)
        report_error(InputDataFilename, EC);
    }
  }

  if (!isYAML(Inputs.front())) {
    mergeLegacyProfiles(Inputs);
    return 0;
  }

  // Merged header.
  BinaryProfileHeader MergedHeader;
  MergedHeader.Version = 1;

  // Merged information for all functions.
  StringMap<BinaryFunctionProfile> MergedBFs;

  bool FirstHeader = true;
  for (std::string &InputDataFilename : Inputs) {
    ErrorOr<std::unique_ptr<MemoryBuffer>> MB =
        MemoryBuffer::getFileOrSTDIN(InputDataFilename);
    if (std::error_code EC = MB.getError())
      report_error(InputDataFilename, EC);
    yaml::Input YamlInput(MB.get()->getBuffer());
    YamlInput.setAllowUnknownKeys(true);

    errs() << "Merging data from " << InputDataFilename << "...\n";

    BinaryProfile BP;
    YamlInput >> BP;
    if (YamlInput.error())
      report_error(InputDataFilename, YamlInput.error());

    // Sanity check.
    if (BP.Header.Version != 1) {
      errs() << "Unable to merge data from profile using version "
             << BP.Header.Version << '\n';
      exit(1);
    }

    // Merge the header.
    if (FirstHeader) {
      MergedHeader = BP.Header;
      FirstHeader = false;
    } else {
      mergeProfileHeaders(MergedHeader, BP.Header);
    }

    // Do the function merge.
    for (BinaryFunctionProfile &BF : BP.Functions) {
      if (!MergedBFs.count(BF.Name)) {
        MergedBFs.insert(std::make_pair(BF.Name, BF));
        continue;
      }

      BinaryFunctionProfile &MergedBF = MergedBFs.find(BF.Name)->second;
      mergeFunctionProfile(MergedBF, std::move(BF));
    }
  }

  if (!opts::SuppressMergedDataOutput) {
    yaml::Output YamlOut(output());

    BinaryProfile MergedProfile;
    MergedProfile.Header = MergedHeader;
    MergedProfile.Functions.resize(MergedBFs.size());
    llvm::copy(llvm::make_second_range(MergedBFs),
               MergedProfile.Functions.begin());

    // For consistency, sort functions by their IDs.
    llvm::sort(MergedProfile.Functions,
               [](const BinaryFunctionProfile &A,
                  const BinaryFunctionProfile &B) { return A.Id < B.Id; });

    YamlOut << MergedProfile;
  }

  errs() << "Data for " << MergedBFs.size()
         << " unique objects successfully merged.\n";

  if (opts::PrintFunctionList != opts::ST_NONE) {
    // List of function names with execution count.
    std::vector<std::pair<uint64_t, StringRef>> FunctionList(MergedBFs.size());
    using CountFuncType = std::function<std::pair<uint64_t, StringRef>(
        const StringMapEntry<BinaryFunctionProfile> &)>;
    CountFuncType ExecCountFunc =
        [](const StringMapEntry<BinaryFunctionProfile> &V) {
          return std::make_pair(V.second.ExecCount, StringRef(V.second.Name));
        };
    CountFuncType BranchCountFunc =
        [](const StringMapEntry<BinaryFunctionProfile> &V) {
          // Return total branch count.
          uint64_t BranchCount = 0;
          for (const BinaryBasicBlockProfile &BI : V.second.Blocks)
            for (const SuccessorInfo &SI : BI.Successors)
              BranchCount += SI.Count;
          return std::make_pair(BranchCount, StringRef(V.second.Name));
        };

    CountFuncType CountFunc = (opts::PrintFunctionList == opts::ST_EXEC_COUNT)
                                  ? ExecCountFunc
                                  : BranchCountFunc;
    llvm::transform(MergedBFs, FunctionList.begin(), CountFunc);
    llvm::stable_sort(reverse(FunctionList));
    errs() << "Functions sorted by "
           << (opts::PrintFunctionList == opts::ST_EXEC_COUNT ? "execution"
                                                              : "total branch")
           << " count:\n";
    for (std::pair<uint64_t, StringRef> &FI : FunctionList)
      errs() << FI.second << " : " << FI.first << '\n';
  }

  return 0;
}
