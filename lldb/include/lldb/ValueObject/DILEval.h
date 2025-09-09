//===-- DILEval.h -----------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLDB_VALUEOBJECT_DILEVAL_H
#define LLDB_VALUEOBJECT_DILEVAL_H

#include "lldb/ValueObject/DILAST.h"
#include "lldb/ValueObject/DILParser.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"
#include <memory>
#include <vector>

namespace lldb_private::dil {

/// Given the name of an identifier (variable name, member name, type name,
/// etc.), find the ValueObject for that name (if it exists), excluding global
/// variables, and create and return an IdentifierInfo object containing all
/// the relevant information about that object (for DIL parsing and
/// evaluating).
lldb::ValueObjectSP LookupIdentifier(llvm::StringRef name_ref,
                                     std::shared_ptr<StackFrame> frame_sp,
                                     lldb::DynamicValueType use_dynamic);

/// Given the name of an identifier, check to see if it matches the name of a
/// global variable. If so, find the ValueObject for that global variable, and
/// create and return an IdentifierInfo object containing all the relevant
/// informatin about it.
lldb::ValueObjectSP LookupGlobalIdentifier(llvm::StringRef name_ref,
                                           std::shared_ptr<StackFrame> frame_sp,
                                           lldb::TargetSP target_sp,
                                           lldb::DynamicValueType use_dynamic);

class Interpreter : Visitor {
public:
  Interpreter(lldb::TargetSP target, llvm::StringRef expr,
              std::shared_ptr<StackFrame> frame_sp,
              lldb::DynamicValueType use_dynamic, bool use_synthetic,
              bool fragile_ivar, bool check_ptr_vs_member);

  llvm::Expected<lldb::ValueObjectSP> Evaluate(const ASTNode *node);

private:
  llvm::Expected<lldb::ValueObjectSP>
  EvaluateAndDereference(const ASTNode *node);
  llvm::Expected<lldb::ValueObjectSP>
  Visit(const IdentifierNode *node) override;
  llvm::Expected<lldb::ValueObjectSP> Visit(const MemberOfNode *node) override;
  llvm::Expected<lldb::ValueObjectSP> Visit(const UnaryOpNode *node) override;
  llvm::Expected<lldb::ValueObjectSP> Visit(const BinaryOpNode *node) override;
  llvm::Expected<lldb::ValueObjectSP>
  Visit(const ArraySubscriptNode *node) override;
  llvm::Expected<lldb::ValueObjectSP>
  Visit(const BitFieldExtractionNode *node) override;
  llvm::Expected<lldb::ValueObjectSP>
  Visit(const IntegerLiteralNode *node) override;
  llvm::Expected<lldb::ValueObjectSP>
  Visit(const FloatLiteralNode *node) override;

  lldb::ValueObjectSP
  ConvertValueObjectToTypeSystem(lldb::ValueObjectSP valobj,
                                 lldb::TypeSystemSP type_system);
  llvm::Expected<lldb::ValueObjectSP>
  UnaryConversion(lldb::ValueObjectSP valobj);
  llvm::Expected<CompilerType> ArithmeticConversion(lldb::ValueObjectSP &lhs,
                                                    lldb::ValueObjectSP &rhs);
  llvm::Expected<lldb::ValueObjectSP> PointerAdd(lldb::ValueObjectSP ptr,
                                                 int64_t offset);
  llvm::Expected<lldb::ValueObjectSP>
  EvaluateArithmeticOp(BinaryOpKind kind, lldb::ValueObjectSP lhs,
                       lldb::ValueObjectSP rhs, CompilerType result_type,
                       uint32_t location);
  llvm::Expected<lldb::ValueObjectSP>
  EvaluateBinaryAddition(lldb::ValueObjectSP lhs, lldb::ValueObjectSP rhs,
                         uint32_t location);
  llvm::Expected<lldb::ValueObjectSP>
  EvaluateBinarySubtraction(lldb::ValueObjectSP lhs, lldb::ValueObjectSP rhs,
                            uint32_t location);
  llvm::Expected<CompilerType>
  PickIntegerType(lldb::TypeSystemSP type_system,
                  std::shared_ptr<ExecutionContextScope> ctx,
                  const IntegerLiteralNode *literal);

  // Used by the interpreter to create objects, perform casts, etc.
  lldb::TargetSP m_target;
  llvm::StringRef m_expr;
  lldb::ValueObjectSP m_scope;
  std::shared_ptr<StackFrame> m_exe_ctx_scope;
  lldb::DynamicValueType m_use_dynamic;
  bool m_use_synthetic;
  bool m_fragile_ivar;
  bool m_check_ptr_vs_member;
};

} // namespace lldb_private::dil

#endif // LLDB_VALUEOBJECT_DILEVAL_H
