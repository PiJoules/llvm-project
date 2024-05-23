//===- RelLookupTableConverterPass - Rel Table Conv -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements relative lookup table converter that converts
// lookup tables to relative lookup tables to make them PIC-friendly.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/RelLookupTableConverter.h"
#include "llvm/Analysis/ConstantFolding.h"
#include "llvm/Analysis/SimplifyQuery.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

using namespace llvm;

static bool HasNonVisibleUses(const Value *V) {
  for (const User *user : V->users()) {
    if (const auto *gep = dyn_cast<GetElementPtrInst>(user)) {
      for (const User *user : gep->users()) {
        if (auto *load = dyn_cast<LoadInst>(user)) {
          if (load->getType()->isPointerTy())
            continue;  // Valid
        }
        // Anything but a load.
        return true;
      }
    } else {
      // Conservative approach - all other uses could lead to an escape of this
      // global. This can be refined in the future.
      return true;
    }
  }

  return false;
}

static void MaybeReplaceWithRelativeOffset(Module &M, GlobalVariable &GV) {
  // The global should look something like this:
  //
  //   @symbols = dso_local constant [3 x ptr] [ptr @.str, ptr @.str.1, ptr @.str.2], align 16
  //
  // This definition must be the one we know will persist at link/runtime.
  if (!GV.hasExactDefinition()) {
    return;
  }

  // We can't see usage in any other module, so we wouldn't be able to replace usage of this global there.
  // But if we had LTO, then we can bump this up to just `isInterposable` and allow hidden visibility.
  if (!GV.hasLocalLinkage())
    return;

  // Definitely don't operate on stuff like llvm.compiler.used.
  if (GV.getName().starts_with("llvm."))
    return;

  const Constant *Initializer = GV.getInitializer();
  if (!Initializer->getType()->isAggregateType())  {
    return;
  }

  // We must be able to see all uses of it.
  if (HasNonVisibleUses(&GV)) {
    //llvm::errs() << "HasNonVisibleUses\n";
    return;
  }

  //llvm::errs() << "Checking operands\n";
  const size_t NumOperands = Initializer->getNumOperands();

  // If there's no operands, the initializer is likely an aggregate of raw constant data.
  if (NumOperands == 0)
    return;

  // All references in it must be non-interposabe.
  // Only do the replacement if all of the contigu
  for (size_t i = 0; i < NumOperands; ++i) {
    Constant *Op = cast<Constant>(Initializer->getOperand(i));

    // The element must be to another global.
    auto *OperandGV = dyn_cast<GlobalValue>(Op);
    if (!OperandGV) {
      return;
    }

    bool DSOLocal = OperandGV->isDSOLocal() || OperandGV->isImplicitDSOLocal();
    if (!DSOLocal)
      return;

    APInt Offset;
    GlobalValue *GV2;
    if (!IsConstantOffsetFromGlobal(Op, GV2, Offset, M.getDataLayout()))
      return;
  }

  Type *IntPtrTy = M.getDataLayout().getIntPtrType(M.getContext());
  Type *OffsetTy = Type::getInt32Ty(M.getContext());

  // Construct the new type.
  Type *ReplacementTy = [&]() -> Type *{
    if (Initializer->getType()->isStructTy()) {
      std::vector<Type *> ElemTys(NumOperands, OffsetTy);
      return llvm::StructType::create(ElemTys);
    } else if (Initializer->getType()->isArrayTy()) {
      return llvm::ArrayType::get(OffsetTy, NumOperands);
    }
    llvm_unreachable("An aggregate type should be one of a struct or array");
  }();

  GlobalVariable *Replacement = new GlobalVariable(M, ReplacementTy, /*isConstant=*/true,
      GV.getLinkage(), /*Initializer=*/nullptr);
  Replacement->takeName(&GV);
  Replacement->setUnnamedAddr(GV.getUnnamedAddr());
  Replacement->setVisibility(GV.getVisibility());
  Replacement->setAlignment(llvm::Align(4));  // Unconditional 4-byte alignment

  Constant *ReplacementInit = [&]() -> Constant *{
    std::vector<Constant *> members;
    members.reserve(NumOperands);
    for (size_t i = 0; i < NumOperands; ++i) {
      Constant *OriginalMember = cast<Constant>(Initializer->getOperand(i));

      // Take the offset.
      Constant *Base = llvm::ConstantExpr::getPtrToInt(Replacement, IntPtrTy);
      Constant *Target = llvm::ConstantExpr::getPtrToInt(OriginalMember, IntPtrTy);
      Constant *Sub = llvm::ConstantExpr::getSub(Target, Base);
      Constant *RelOffset = llvm::ConstantExpr::getTrunc(Sub, OffsetTy);

      members.push_back(RelOffset);
    }

    if (Initializer->getType()->isStructTy()) {
      return llvm::ConstantStruct::getAnon(members);
    } else if (Initializer->getType()->isArrayTy()) {
      return llvm::ConstantArray::get(cast<ArrayType>(ReplacementTy), members);
    }
    llvm_unreachable("An aggregate type should be one of a struct or array");
  }();

  Replacement->setInitializer(ReplacementInit);

  // Rn, we only account for geps, loads, and stores.
  for (User *user : GV.users()) {
    if (auto *gep = dyn_cast<GetElementPtrInst>(user)) {
      for (User *user : gep->users()) {
        if (auto *load = dyn_cast<LoadInst>(user)) {
          assert(gep->getOperand(0) == &GV);

          BasicBlock *BB = gep->getParent();
          IRBuilder<> Builder(BB);
          Builder.SetInsertPoint(gep);

          // 1. The global itself
          // 2. The first index (which should be zero)
          // 3. The actual offset from the start of the global.
          Value *Offset;
          if (gep->getNumOperands() == 3) {
            // Convert to offset in bytes.
            Offset = gep->getOperand(2);
            Offset = Builder.CreateShl(Offset, ConstantInt::get(Offset->getType(), 2));
          } else if (gep->getNumOperands() == 2) {
            assert(gep->getSourceElementType()->isIntegerTy(8) && "Unhandled source element type");;
            Offset = gep->getOperand(1);
          } else {
            GV.dump();
            gep->dump();
            gep->getParent()->getParent()->dump();
            assert(0 && "unhandled gep operand count");
          }

          Function *LoadRelIntrinsic = llvm::Intrinsic::getDeclaration(
              &M, Intrinsic::load_relative, {Offset->getType()});
          
          Builder.SetInsertPoint(load);
          Value *RelLoad = Builder.CreateCall(LoadRelIntrinsic, {&GV, Offset},
                                               "reltable.intrinsic");
          if (load->getType() != RelLoad->getType()) {
            llvm::errs() << "Differing types\n";
            gep->getOperand(0)->dump();
            gep->dump();
            load->dump();
            RelLoad->dump();
          }
          load->replaceAllUsesWith(RelLoad);
        } else {
          llvm_unreachable("Unhandled use for GV");
        }
      }
    } else {
      llvm_unreachable("Unhandled use for GV");
    }
  }


  llvm::errs() << "Replaced " << Replacement->getName() << ": "; GV.dump();
  GV.replaceAllUsesWith(Replacement);
  GV.eraseFromParent();
}

static bool shouldConvertToRelLookupTable(Module &M, GlobalVariable &GV) {
  // If lookup table has more than one user,
  // do not generate a relative lookup table.
  // This is to simplify the analysis that needs to be done for this pass.
  // TODO: Add support for lookup tables with multiple uses.
  // For ex, this can happen when a function that uses a lookup table gets
  // inlined into multiple call sites.
  if (!GV.hasInitializer() ||
      !GV.isConstant() ||
      !GV.hasOneUse())
    return false;

  GetElementPtrInst *GEP =
      dyn_cast<GetElementPtrInst>(GV.use_begin()->getUser());
  if (!GEP || !GEP->hasOneUse() ||
      GV.getValueType() != GEP->getSourceElementType())
    return false;

  LoadInst *Load = dyn_cast<LoadInst>(GEP->use_begin()->getUser());
  if (!Load || !Load->hasOneUse() ||
      Load->getType() != GEP->getResultElementType())
    return false;

  // If the original lookup table does not have local linkage and is
  // not dso_local, do not generate a relative lookup table.
  // This optimization creates a relative lookup table that consists of
  // offsets between the start of the lookup table and its elements.
  // To be able to generate these offsets, relative lookup table and
  // its elements should have internal linkage and be dso_local, which means
  // that they should resolve to symbols within the same linkage unit.
  if (!GV.hasLocalLinkage() ||
      !GV.isDSOLocal() ||
      !GV.isImplicitDSOLocal())
    return false;

  ConstantArray *Array = dyn_cast<ConstantArray>(GV.getInitializer());
  if (!Array)
    return false;

  // If values are not 64-bit pointers, do not generate a relative lookup table.
  const DataLayout &DL = M.getDataLayout();
  Type *ElemType = Array->getType()->getElementType();
  if (!ElemType->isPointerTy() || DL.getPointerTypeSizeInBits(ElemType) != 64)
    return false;

  for (const Use &Op : Array->operands()) {
    Constant *ConstOp = cast<Constant>(&Op);
    GlobalValue *GVOp;
    APInt Offset;

    // If an operand is not a constant offset from a lookup table,
    // do not generate a relative lookup table.
    if (!IsConstantOffsetFromGlobal(ConstOp, GVOp, Offset, DL))
      return false;

    // If operand is mutable, do not generate a relative lookup table.
    auto *GlovalVarOp = dyn_cast<GlobalVariable>(GVOp);
    if (!GlovalVarOp || !GlovalVarOp->isConstant())
      return false;

    if (!GlovalVarOp->hasLocalLinkage() ||
        !GlovalVarOp->isDSOLocal() ||
        !GlovalVarOp->isImplicitDSOLocal())
      return false;
  }

  return true;
}

static GlobalVariable *createRelLookupTable(Function &Func,
                                            GlobalVariable &LookupTable) {
  Module &M = *Func.getParent();
  ConstantArray *LookupTableArr =
      cast<ConstantArray>(LookupTable.getInitializer());
  unsigned NumElts = LookupTableArr->getType()->getNumElements();
  ArrayType *IntArrayTy =
      ArrayType::get(Type::getInt32Ty(M.getContext()), NumElts);

  GlobalVariable *RelLookupTable = new GlobalVariable(
    M, IntArrayTy, LookupTable.isConstant(), LookupTable.getLinkage(),
    nullptr, "reltable." + Func.getName(), &LookupTable,
    LookupTable.getThreadLocalMode(), LookupTable.getAddressSpace(),
    LookupTable.isExternallyInitialized());

  uint64_t Idx = 0;
  SmallVector<Constant *, 64> RelLookupTableContents(NumElts);

  for (Use &Operand : LookupTableArr->operands()) {
    Constant *Element = cast<Constant>(Operand);
    Type *IntPtrTy = M.getDataLayout().getIntPtrType(M.getContext());
    Constant *Base = llvm::ConstantExpr::getPtrToInt(RelLookupTable, IntPtrTy);
    Constant *Target = llvm::ConstantExpr::getPtrToInt(Element, IntPtrTy);
    Constant *Sub = llvm::ConstantExpr::getSub(Target, Base);
    Constant *RelOffset =
        llvm::ConstantExpr::getTrunc(Sub, Type::getInt32Ty(M.getContext()));
    RelLookupTableContents[Idx++] = RelOffset;
  }

  Constant *Initializer =
      ConstantArray::get(IntArrayTy, RelLookupTableContents);
  RelLookupTable->setInitializer(Initializer);
  RelLookupTable->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
  RelLookupTable->setAlignment(llvm::Align(4));
  return RelLookupTable;
}

static void convertToRelLookupTable(GlobalVariable &LookupTable) {
  GetElementPtrInst *GEP =
      cast<GetElementPtrInst>(LookupTable.use_begin()->getUser());
  LoadInst *Load = cast<LoadInst>(GEP->use_begin()->getUser());

  Module &M = *LookupTable.getParent();
  BasicBlock *BB = GEP->getParent();
  IRBuilder<> Builder(BB);
  Function &Func = *BB->getParent();

  // Generate an array that consists of relative offsets.
  GlobalVariable *RelLookupTable = createRelLookupTable(Func, LookupTable);

  // Place new instruction sequence before GEP.
  Builder.SetInsertPoint(GEP);
  Value *Index = GEP->getOperand(2);
  IntegerType *IntTy = cast<IntegerType>(Index->getType());
  Value *Offset =
      Builder.CreateShl(Index, ConstantInt::get(IntTy, 2), "reltable.shift");

  // Insert the call to load.relative intrinsic before LOAD.
  // GEP might not be immediately followed by a LOAD, like it can be hoisted
  // outside the loop or another instruction might be inserted them in between.
  Builder.SetInsertPoint(Load);
  Function *LoadRelIntrinsic = llvm::Intrinsic::getDeclaration(
      &M, Intrinsic::load_relative, {Index->getType()});

  // Create a call to load.relative intrinsic that computes the target address
  // by adding base address (lookup table address) and relative offset.
  Value *Result = Builder.CreateCall(LoadRelIntrinsic, {RelLookupTable, Offset},
                                     "reltable.intrinsic");

  // Replace load instruction with the new generated instruction sequence.
  Load->replaceAllUsesWith(Result);
  // Remove Load and GEP instructions.
  Load->eraseFromParent();
  GEP->eraseFromParent();
}

// Convert lookup tables to relative lookup tables in the module.
static bool convertToRelativeLookupTables(
    Module &M, function_ref<TargetTransformInfo &(Function &)> GetTTI) {
  for (Function &F : M) {
    if (F.isDeclaration())
      continue;

    // Check if we have a target that supports relative lookup tables.
    if (!GetTTI(F).shouldBuildRelLookupTables())
      return false;

    // We assume that the result is independent of the checked function.
    break;
  }

  bool Changed = false;

  for (GlobalVariable &GV : llvm::make_early_inc_range(M.globals())) {
    if (!shouldConvertToRelLookupTable(M, GV)) {
      MaybeReplaceWithRelativeOffset(M, GV);
      continue;
    }

    convertToRelLookupTable(GV);

    // Remove the original lookup table.
    GV.eraseFromParent();

    Changed = true;
  }

  return Changed;
}

PreservedAnalyses RelLookupTableConverterPass::run(Module &M,
                                                   ModuleAnalysisManager &AM) {
  FunctionAnalysisManager &FAM =
      AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();

  auto GetTTI = [&](Function &F) -> TargetTransformInfo & {
    return FAM.getResult<TargetIRAnalysis>(F);
  };

  if (!convertToRelativeLookupTables(M, GetTTI))
    return PreservedAnalyses::all();

  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
