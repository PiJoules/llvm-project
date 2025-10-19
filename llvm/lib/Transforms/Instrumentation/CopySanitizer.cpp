//===- CopySanitizer.cpp - memory access error detector --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Instrumentation/CopySanitizer.h"
#include "llvm/IR/Comdat.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

namespace llvm {

namespace {

const char kCsanModuleCtorName[] = "csan.module_ctor";
const char kCsanInitName[] = "__csan_init";

class CopySanitizer {
public:
  CopySanitizer(Module &M) : M(M), C(M.getContext()) {
    std::tie(CsanCtorFunction, std::ignore) =
        getOrCreateSanitizerCtorAndInitFunctions(
            M, kCsanModuleCtorName, kCsanInitName,
            /*InitArgTypes=*/{},
            /*InitArgs=*/{},
            // This callback is invoked when the functions are created the first
            // time. Hook them into the global ctors list in that case:
            [&](Function *Ctor, FunctionCallee) {
              Comdat *CtorComdat = M.getOrInsertComdat(kCsanModuleCtorName);
              Ctor->setComdat(CtorComdat);
              appendToGlobalCtors(M, Ctor, 0, Ctor);
            });

    PtrTy = PointerType::getUnqual(C);
    IntptrTy = Type::getIntNTy(C, M.getDataLayout().getPointerSizeInBits());

    CsanAlloc =
        M.getOrInsertFunction("__csan_alloc", PtrTy, IntptrTy, IntptrTy);
    CsanFree = M.getOrInsertFunction("__csan_free", Type::getVoidTy(C), PtrTy);
    CsanMarkWritten = M.getOrInsertFunction(
        "__csan_mark_written", Type::getVoidTy(C), PtrTy, IntptrTy);
    CsanMarkRead = M.getOrInsertFunction("__csan_mark_read", Type::getVoidTy(C),
                                         PtrTy, IntptrTy);
  }

  void sanitizeFunction(Function &F, FunctionAnalysisManager &FAM) {
    if (&F == CsanCtorFunction)
      return;

    if (F.hasFnAttribute(Attribute::Naked))
      return;

    if (F.empty())
      return;

    SmallVector<AllocaInst *, 16> Allocas;
    SmallVector<ReturnInst *, 16> Rets;
    SmallVector<MemIntrinsic *, 16> MemIntrinsics;
    SmallVector<LoadInst *, 16> Loads;
    SmallVector<StoreInst *, 16> Stores;

    for (auto &Inst : instructions(F)) {
      if (auto *AI = dyn_cast<AllocaInst>(&Inst)) {
        Allocas.push_back(AI);
      } else if (auto *RI = dyn_cast<ReturnInst>(&Inst)) {
        Rets.push_back(RI);
      } else if (auto *MI = dyn_cast<MemIntrinsic>(&Inst)) {
        MemIntrinsics.push_back(MI);
      } else if (auto *LI = dyn_cast<LoadInst>(&Inst)) {
        Loads.push_back(LI);
      } else if (auto *SI = dyn_cast<StoreInst>(&Inst)) {
        Stores.push_back(SI);
      }
    }

    // Replace all allocas with runtime calls.
    SmallVector<CallInst *, 16> NewAllocas;
    for (AllocaInst *AI : Allocas) {
      size_t ByteSize = *AI->getAllocationSize(AI->getDataLayout());

      IRBuilder<> IRB(AI);
      auto *CI = IRB.CreateCall(
          CsanAlloc, {ConstantInt::get(IntptrTy, ByteSize),
                      ConstantInt::get(IntptrTy, AI->getAlign().value())});
      CI->takeName(AI);
      NewAllocas.push_back(CI);

      AI->replaceAllUsesWith(CI);
      AI->eraseFromParent();

      // Remove lifetime markers now that these are no longer allocas.
      //
      // TODO: Allocations can be freed early once the lifetime ends.
      for (User *U : make_early_inc_range(CI->users())) {
        auto *I = cast<Instruction>(U);
        if (I->isLifetimeStartOrEnd())
          I->eraseFromParent();
      }
    }

    // Destroy all allocations before exiting scope.
    for (ReturnInst *RI : Rets) {
      // This places inserted instructions before the ReturnInst.
      IRBuilder<> IRB(RI);
      for (CallInst *CI : NewAllocas) {
        IRB.CreateCall(CsanFree, {CI});
      }
    }

    // Record memcpys/memmoves.
    for (MemIntrinsic *MI : MemIntrinsics) {
      IRBuilder<> IRB(MI);
      if (auto *MT = dyn_cast<MemTransferInst>(MI)) {
        IRB.CreateCall(
            CsanMarkRead,
            {MT->getSource(),
             IRB.CreateIntCast(MT->getLength(), IntptrTy, /*isSigned=*/false)});
      }
      IRB.CreateCall(
          CsanMarkWritten,
          {MI->getDest(),
           IRB.CreateIntCast(MI->getLength(), IntptrTy, /*isSigned=*/false)});
    }

    for (LoadInst *LI : Loads) {
      IRBuilder<> IRB(LI);
      auto Size = M.getDataLayout().getTypeAllocSize(LI->getType());
      IRB.CreateCall(CsanMarkRead, {LI->getPointerOperand(),
                                    ConstantInt::get(IntptrTy, Size)});
    }

    for (StoreInst *SI : Stores) {
      IRBuilder<> IRB(SI);
      auto Size =
          M.getDataLayout().getTypeAllocSize(SI->getValueOperand()->getType());
      IRB.CreateCall(CsanMarkWritten, {SI->getPointerOperand(),
                                       ConstantInt::get(IntptrTy, Size)});
    }
  }

private:
  Module &M;
  LLVMContext &C;

  PointerType *PtrTy;
  IntegerType *IntptrTy;

  Function *CsanCtorFunction;
  FunctionCallee CsanAlloc, CsanFree, CsanMarkWritten, CsanMarkRead;
};

} // namespace

PreservedAnalyses CopySanitizerPass::run(Module &M,
                                         ModuleAnalysisManager &MAM) {
  CopySanitizer csan(M);

  auto &FAM = MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  for (Function &F : M) {
    csan.sanitizeFunction(F, FAM);
  }

  return PreservedAnalyses::none();
}

} // namespace llvm
