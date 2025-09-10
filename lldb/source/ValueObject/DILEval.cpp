//===-- DILEval.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "lldb/ValueObject/DILEval.h"
#include "lldb/Core/Module.h"
#include "lldb/Symbol/CompileUnit.h"
#include "lldb/Symbol/TypeSystem.h"
#include "lldb/Symbol/VariableList.h"
#include "lldb/Target/RegisterContext.h"
#include "lldb/ValueObject/DILAST.h"
#include "lldb/ValueObject/ValueObject.h"
#include "lldb/ValueObject/ValueObjectRegister.h"
#include "lldb/ValueObject/ValueObjectVariable.h"
#include "llvm/Support/FormatAdapters.h"
#include <memory>

namespace lldb_private::dil {

static llvm::Expected<lldb::TypeSystemSP>
GetTypeSystemFromCU(std::shared_ptr<ExecutionContextScope> ctx) {
  auto stack_frame = ctx->CalculateStackFrame();
  if (!stack_frame)
    return llvm::createStringError("no stack frame in this context");
  SymbolContext symbol_context =
      stack_frame->GetSymbolContext(lldb::eSymbolContextCompUnit);
  lldb::LanguageType language = symbol_context.comp_unit->GetLanguage();

  symbol_context = stack_frame->GetSymbolContext(lldb::eSymbolContextModule);
  return symbol_context.module_sp->GetTypeSystemForLanguage(language);
}

static CompilerType GetBasicType(lldb::TypeSystemSP type_system,
                                 lldb::BasicType basic_type) {
  if (type_system)
    return type_system.get()->GetBasicTypeFromAST(basic_type);

  return CompilerType();
}

static lldb::ValueObjectSP
ArrayToPointerConversion(lldb::ValueObjectSP valobj,
                         std::shared_ptr<ExecutionContextScope> ctx) {
  assert(valobj->IsArrayType() &&
         "an argument to array-to-pointer conversion must be an array");

  uint64_t addr = valobj->GetLoadAddress();
  ExecutionContext exe_ctx;
  ctx->CalculateExecutionContext(exe_ctx);
  return ValueObject::CreateValueObjectFromAddress(
      "result", addr, exe_ctx,
      valobj->GetCompilerType().GetArrayElementType(ctx.get()).GetPointerType(),
      /* do_deref */ false);
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::UnaryConversion(lldb::ValueObjectSP valobj) {
  // Perform usual conversions for unary operators. At the moment this includes
  // array-to-pointer and the integral promotion for eligible types.
  llvm::Expected<lldb::TypeSystemSP> type_system =
      GetTypeSystemFromCU(m_exe_ctx_scope);
  if (!type_system)
    return type_system.takeError();
  CompilerType in_type = valobj->GetCompilerType();
  CompilerType result_type;
  if (valobj->IsBitfield()) {
    // Promote bitfields. If `int` can represent the bitfield value, it is
    // converted to `int`. Otherwise, if `unsigned int` can represent it, it
    // is converted to `unsigned int`. Otherwise, it is treated as its
    // underlying type.
    uint32_t bitfield_size = valobj->GetBitfieldBitSize();
    // Some bitfields have undefined size (e.g. result of ternary operation).
    // The AST's `bitfield_size` of those is 0, and no promotion takes place.
    if (bitfield_size > 0 && in_type.IsInteger()) {
      CompilerType int_type = GetBasicType(*type_system, lldb::eBasicTypeInt);
      CompilerType uint_type =
          GetBasicType(*type_system, lldb::eBasicTypeUnsignedInt);
      llvm::Expected<uint64_t> int_bit_size =
          int_type.GetBitSize(m_exe_ctx_scope.get());
      if (!int_bit_size)
        return int_bit_size.takeError();
      llvm::Expected<uint64_t> uint_bit_size =
          uint_type.GetBitSize(m_exe_ctx_scope.get());
      if (!uint_bit_size)
        return int_bit_size.takeError();
      if (bitfield_size < *int_bit_size ||
          (in_type.IsSigned() && bitfield_size == *int_bit_size))
        return valobj->CastToBasicType(int_type);
      if (bitfield_size <= *uint_bit_size)
        return valobj->CastToBasicType(uint_type);
      // Re-create as a const value with the same underlying type
      Scalar scalar;
      bool resolved = valobj->ResolveValue(scalar);
      if (!resolved)
        return llvm::createStringError("invalid scalar value");
      return ValueObject::CreateValueObjectFromScalar(m_target, scalar, in_type,
                                                      "result");
    }
  }

  if (in_type.IsArrayType())
    valobj = ArrayToPointerConversion(valobj, m_exe_ctx_scope);

  if (valobj->GetCompilerType().IsInteger() ||
      valobj->GetCompilerType().IsUnscopedEnumerationType()) {
    llvm::Expected<CompilerType> promoted_type =
        type_system.get()->DoIntegralPromotion(valobj->GetCompilerType(),
                                               m_exe_ctx_scope.get());
    if (!promoted_type)
      return promoted_type.takeError();
    if (!promoted_type->CompareTypes(valobj->GetCompilerType()))
      return valobj->CastToBasicType(*promoted_type);
  }

  return valobj;
}

static size_t ConversionRank(CompilerType type) {
  // Get integer conversion rank
  // https://eel.is/c++draft/conv.rank
  switch (type.GetCanonicalType().GetBasicTypeEnumeration()) {
  case lldb::eBasicTypeBool:
    return 1;
  case lldb::eBasicTypeChar:
  case lldb::eBasicTypeSignedChar:
  case lldb::eBasicTypeUnsignedChar:
    return 2;
  case lldb::eBasicTypeShort:
  case lldb::eBasicTypeUnsignedShort:
    return 3;
  case lldb::eBasicTypeInt:
  case lldb::eBasicTypeUnsignedInt:
    return 4;
  case lldb::eBasicTypeLong:
  case lldb::eBasicTypeUnsignedLong:
    return 5;
  case lldb::eBasicTypeLongLong:
  case lldb::eBasicTypeUnsignedLongLong:
    return 6;

  // TODO: The ranks of char16_t, char32_t, and wchar_t are equal to the
  // ranks of their underlying types.
  case lldb::eBasicTypeWChar:
  case lldb::eBasicTypeSignedWChar:
  case lldb::eBasicTypeUnsignedWChar:
    return 3;
  case lldb::eBasicTypeChar16:
    return 3;
  case lldb::eBasicTypeChar32:
    return 4;

  default:
    break;
  }
  return 0;
}

llvm::Expected<CompilerType>
Interpreter::ArithmeticConversion(lldb::ValueObjectSP &lhs,
                                  lldb::ValueObjectSP &rhs) {
  // Apply unary conversion (e.g. intergal promotion) for both operands.
  auto lhs_or_err = UnaryConversion(lhs);
  if (!lhs_or_err)
    return lhs_or_err.takeError();
  lhs = *lhs_or_err;
  auto rhs_or_err = UnaryConversion(rhs);
  if (!rhs_or_err)
    return rhs_or_err.takeError();
  rhs = *rhs_or_err;

  CompilerType lhs_type = lhs->GetCompilerType();
  CompilerType rhs_type = rhs->GetCompilerType();

  if (lhs_type.CompareTypes(rhs_type))
    return lhs_type;

  // If either of the operands is not arithmetic (e.g. pointer), we're done.
  if (!lhs_type.IsScalarType() || !rhs_type.IsScalarType())
    return CompilerType();

  // Handle conversions for floating types (float, double).
  if (lhs_type.IsFloat() || rhs_type.IsFloat()) {
    // If both are floats, convert the smaller operand to the bigger.
    if (lhs_type.IsFloat() && rhs_type.IsFloat()) {
      int order = lhs_type.GetBasicTypeEnumeration() -
                  rhs_type.GetBasicTypeEnumeration();
      if (order > 0)
        return lhs_type;
      return rhs_type;
    }

    if (lhs_type.IsFloat() && rhs_type.IsInteger())
      return lhs_type;
    return rhs_type;
  }

  if (lhs_type.IsInteger() && rhs_type.IsInteger()) {
    using Rank = std::tuple<size_t, bool>;
    Rank l_rank = {ConversionRank(lhs_type), !lhs_type.IsSigned()};
    Rank r_rank = {ConversionRank(rhs_type), !rhs_type.IsSigned()};

    if (l_rank < r_rank)
      return rhs_type;
    if (l_rank > r_rank)
      return lhs_type;
  }
  return rhs_type;
}

static lldb::VariableSP DILFindVariable(ConstString name,
                                        VariableList &variable_list) {
  lldb::VariableSP exact_match;
  std::vector<lldb::VariableSP> possible_matches;

  for (lldb::VariableSP var_sp : variable_list) {
    llvm::StringRef str_ref_name = var_sp->GetName().GetStringRef();

    str_ref_name.consume_front("::");
    // Check for the exact same match
    if (str_ref_name == name.GetStringRef())
      return var_sp;

    // Check for possible matches by base name
    if (var_sp->NameMatches(name))
      possible_matches.push_back(var_sp);
  }

  // If there's a non-exact match, take it.
  if (possible_matches.size() > 0)
    return possible_matches[0];

  return nullptr;
}

lldb::ValueObjectSP LookupGlobalIdentifier(
    llvm::StringRef name_ref, std::shared_ptr<StackFrame> stack_frame,
    lldb::TargetSP target_sp, lldb::DynamicValueType use_dynamic) {
  // Get a global variables list without the locals from the current frame
  SymbolContext symbol_context =
      stack_frame->GetSymbolContext(lldb::eSymbolContextCompUnit);
  lldb::VariableListSP variable_list;
  if (symbol_context.comp_unit)
    variable_list = symbol_context.comp_unit->GetVariableList(true);

  name_ref.consume_front("::");
  lldb::ValueObjectSP value_sp;
  if (variable_list) {
    lldb::VariableSP var_sp =
        DILFindVariable(ConstString(name_ref), *variable_list);
    if (var_sp)
      value_sp =
          stack_frame->GetValueObjectForFrameVariable(var_sp, use_dynamic);
  }

  if (value_sp)
    return value_sp;

  // Check for match in modules global variables.
  VariableList modules_var_list;
  target_sp->GetImages().FindGlobalVariables(
      ConstString(name_ref), std::numeric_limits<uint32_t>::max(),
      modules_var_list);

  if (!modules_var_list.Empty()) {
    lldb::VariableSP var_sp =
        DILFindVariable(ConstString(name_ref), modules_var_list);
    if (var_sp)
      value_sp = ValueObjectVariable::Create(stack_frame.get(), var_sp);

    if (value_sp)
      return value_sp;
  }
  return nullptr;
}

lldb::ValueObjectSP LookupIdentifier(llvm::StringRef name_ref,
                                     std::shared_ptr<StackFrame> stack_frame,
                                     lldb::DynamicValueType use_dynamic) {
  // Support $rax as a special syntax for accessing registers.
  // Will return an invalid value in case the requested register doesn't exist.
  if (name_ref.consume_front("$")) {
    lldb::RegisterContextSP reg_ctx(stack_frame->GetRegisterContext());
    if (!reg_ctx)
      return nullptr;

    if (const RegisterInfo *reg_info = reg_ctx->GetRegisterInfoByName(name_ref))
      return ValueObjectRegister::Create(stack_frame.get(), reg_ctx, reg_info);

    return nullptr;
  }

  if (!name_ref.contains("::")) {
    // Lookup in the current frame.
    // Try looking for a local variable in current scope.
    lldb::VariableListSP variable_list(
        stack_frame->GetInScopeVariableList(false));

    lldb::ValueObjectSP value_sp;
    if (variable_list) {
      lldb::VariableSP var_sp =
          variable_list->FindVariable(ConstString(name_ref));
      if (var_sp)
        value_sp =
            stack_frame->GetValueObjectForFrameVariable(var_sp, use_dynamic);
    }

    if (value_sp)
      return value_sp;

    // Try looking for an instance variable (class member).
    SymbolContext sc = stack_frame->GetSymbolContext(
        lldb::eSymbolContextFunction | lldb::eSymbolContextBlock);
    llvm::StringRef ivar_name = sc.GetInstanceVariableName();
    value_sp = stack_frame->FindVariable(ConstString(ivar_name));
    if (value_sp)
      value_sp = value_sp->GetChildMemberWithName(name_ref);

    if (value_sp)
      return value_sp;
  }
  return nullptr;
}

Interpreter::Interpreter(lldb::TargetSP target, llvm::StringRef expr,
                         std::shared_ptr<StackFrame> frame_sp,
                         lldb::DynamicValueType use_dynamic, bool use_synthetic,
                         bool fragile_ivar, bool check_ptr_vs_member)
    : m_target(std::move(target)), m_expr(expr), m_exe_ctx_scope(frame_sp),
      m_use_dynamic(use_dynamic), m_use_synthetic(use_synthetic),
      m_fragile_ivar(fragile_ivar), m_check_ptr_vs_member(check_ptr_vs_member) {
}

llvm::Expected<lldb::ValueObjectSP> Interpreter::Evaluate(const ASTNode *node) {
  // Evaluate an AST.
  auto value_or_error = node->Accept(this);
  // Return the computed value-or-error. The caller is responsible for
  // checking if an error occured during the evaluation.
  return value_or_error;
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::EvaluateAndDereference(const ASTNode *node) {
  auto valobj_or_err = Evaluate(node);
  if (!valobj_or_err)
    return valobj_or_err;
  lldb::ValueObjectSP valobj = *valobj_or_err;

  Status error;
  if (valobj->GetCompilerType().IsReferenceType()) {
    valobj = valobj->Dereference(error);
    if (error.Fail())
      return error.ToError();
  }
  return valobj;
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const IdentifierNode *node) {
  lldb::DynamicValueType use_dynamic = m_use_dynamic;

  lldb::ValueObjectSP identifier =
      LookupIdentifier(node->GetName(), m_exe_ctx_scope, use_dynamic);

  if (!identifier)
    identifier = LookupGlobalIdentifier(node->GetName(), m_exe_ctx_scope,
                                        m_target, use_dynamic);
  if (!identifier) {
    std::string errMsg =
        llvm::formatv("use of undeclared identifier '{0}'", node->GetName());
    return llvm::make_error<DILDiagnosticError>(
        m_expr, errMsg, node->GetLocation(), node->GetName().size());
  }

  return identifier;
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const UnaryOpNode *node) {
  Status error;
  auto op_or_err = Evaluate(node->GetOperand());
  if (!op_or_err)
    return op_or_err;

  lldb::ValueObjectSP operand = *op_or_err;

  switch (node->GetKind()) {
  case UnaryOpKind::Deref: {
    lldb::ValueObjectSP dynamic_op = operand->GetDynamicValue(m_use_dynamic);
    if (dynamic_op)
      operand = dynamic_op;

    lldb::ValueObjectSP child_sp = operand->Dereference(error);
    if (!child_sp && m_use_synthetic) {
      if (lldb::ValueObjectSP synth_obj_sp = operand->GetSyntheticValue()) {
        error.Clear();
        child_sp = synth_obj_sp->Dereference(error);
      }
    }
    if (error.Fail())
      return llvm::make_error<DILDiagnosticError>(m_expr, error.AsCString(),
                                                  node->GetLocation());

    return child_sp;
  }
  case UnaryOpKind::AddrOf: {
    Status error;
    lldb::ValueObjectSP value = operand->AddressOf(error);
    if (error.Fail())
      return llvm::make_error<DILDiagnosticError>(m_expr, error.AsCString(),
                                                  node->GetLocation());

    return value;
  }
  case UnaryOpKind::Minus: {
    llvm::Expected<lldb::ValueObjectSP> conv_op = UnaryConversion(operand);
    if (!conv_op)
      return conv_op;
    operand = *conv_op;
    CompilerType operand_type = operand->GetCompilerType();
    if (!operand_type.IsScalarType()) {
      std::string errMsg =
          llvm::formatv("invalid argument type '{0}' to unary expression",
                        operand_type.GetTypeName());
      return llvm::make_error<DILDiagnosticError>(m_expr, errMsg,
                                                  node->GetLocation());
    }
    Scalar scalar;
    bool resolved = operand->ResolveValue(scalar);
    if (!resolved)
      break;

    bool negated = scalar.UnaryNegate();
    if (negated)
      return ValueObject::CreateValueObjectFromScalar(
          m_target, scalar, operand->GetCompilerType(), "result");
    break;
  }
  case UnaryOpKind::Plus: {
    llvm::Expected<lldb::ValueObjectSP> conv_op = UnaryConversion(operand);
    if (!conv_op)
      return conv_op;
    operand = *conv_op;
    CompilerType operand_type = operand->GetCompilerType();
    if (!operand_type.IsScalarType() &&
        // Unary plus is allowed for pointers.
        !operand_type.IsPointerType()) {
      std::string errMsg =
          llvm::formatv("invalid argument type '{0}' to unary expression",
                        operand_type.GetTypeName());
      return llvm::make_error<DILDiagnosticError>(m_expr, errMsg,
                                                  node->GetLocation());
    }
    return operand;
  }
  }
  return llvm::make_error<DILDiagnosticError>(m_expr, "invalid unary operation",
                                              node->GetLocation());
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::PointerAdd(lldb::ValueObjectSP ptr, int64_t offset) {
  llvm::Expected<uint64_t> byte_size =
      ptr->GetCompilerType().GetPointeeType().GetByteSize(
          ptr->GetTargetSP().get());
  if (!byte_size)
    return byte_size.takeError();
  uintptr_t addr = ptr->GetValueAsUnsigned(0) + offset * (*byte_size);

  ExecutionContext exe_ctx(m_target.get(), false);
  Scalar s(addr);
  return ValueObject::CreateValueObjectFromScalar(
      m_target, s, ptr->GetCompilerType(), "result");
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::EvaluateArithmeticOp(BinaryOpKind kind, lldb::ValueObjectSP lhs,
                                  lldb::ValueObjectSP rhs,
                                  CompilerType result_type, uint32_t location) {
  auto type_system = lhs->GetCompilerType().GetTypeSystem().GetSharedPointer();
  Scalar l, r;
  bool l_resolved = lhs->ResolveValue(l);
  bool r_resolved = rhs->ResolveValue(r);

  if (!l_resolved || !r_resolved)
    return llvm::make_error<DILDiagnosticError>(m_expr, "invalid scalar value",
                                                location);

  switch (kind) {
  case BinaryOpKind::Add: {
    Scalar result = l + r;
    return ValueObject::CreateValueObjectFromScalar(m_target, result,
                                                    result_type, "result");
  }
  case BinaryOpKind::Sub: {
    Scalar result = l - r;
    return ValueObject::CreateValueObjectFromScalar(m_target, result,
                                                    result_type, "result");
  }
  }
  return llvm::make_error<DILDiagnosticError>(
      m_expr, "invalid arithmetic operation", location);
}

llvm::Expected<lldb::ValueObjectSP> Interpreter::EvaluateBinaryAddition(
    lldb::ValueObjectSP lhs, lldb::ValueObjectSP rhs, uint32_t location) {
  // Operation '+' works for:
  //
  //  {scalar,unscoped_enum} <-> {scalar,unscoped_enum}
  //  {integer,unscoped_enum} <-> pointer
  //  pointer <-> {integer,unscoped_enum}
  auto orig_lhs_type = lhs->GetCompilerType();
  auto orig_rhs_type = rhs->GetCompilerType();
  auto type_or_err = ArithmeticConversion(lhs, rhs);
  if (!type_or_err)
    return type_or_err.takeError();
  CompilerType result_type = *type_or_err;

  if (result_type.IsScalarType())
    return EvaluateArithmeticOp(BinaryOpKind::Add, lhs, rhs, result_type,
                                location);

  // Check for pointer arithmetic.
  // One of the operands must be a pointer and the other one an integer.
  lldb::ValueObjectSP ptr, offset;
  if (lhs->GetCompilerType().IsPointerType()) {
    ptr = lhs;
    offset = rhs;
  } else if (rhs->GetCompilerType().IsPointerType()) {
    ptr = rhs;
    offset = lhs;
  }

  if (!ptr || !offset->GetCompilerType().IsInteger()) {
    std::string errMsg =
        llvm::formatv("invalid operands to binary expression ('{0}' and '{1}')",
                      orig_lhs_type.GetTypeName(), orig_rhs_type.GetTypeName());
    return llvm::make_error<DILDiagnosticError>(m_expr, errMsg, location);
  }

  if (ptr->GetCompilerType().IsPointerToVoid()) {
    return llvm::make_error<DILDiagnosticError>(
        m_expr, "arithmetic on a pointer to void", location);
  }
  if (ptr->GetValueAsUnsigned(0) == 0 && offset->GetValueAsUnsigned(0) != 0) {
    return llvm::make_error<DILDiagnosticError>(
        m_expr, "arithmetic on a nullptr is undefined", location);
  }

  return PointerAdd(ptr, offset->GetValueAsUnsigned(0));
}

static llvm::Expected<CompilerType>
GetPtrDiffType(std::shared_ptr<ExecutionContextScope> ctx) {
  lldb::TargetSP target_sp = ctx->CalculateTarget();
  llvm::Triple triple(
      llvm::Twine(target_sp->GetArchitecture().GetTriple().str()));
  lldb::BasicType basic_type;
  if (triple.isOSWindows()) {
    basic_type =
        triple.isArch64Bit() ? lldb::eBasicTypeLongLong : lldb::eBasicTypeInt;
  } else {
    basic_type =
        triple.isArch64Bit() ? lldb::eBasicTypeLong : lldb::eBasicTypeInt;
  }

  llvm::Expected<lldb::TypeSystemSP> type_system = GetTypeSystemFromCU(ctx);
  if (!type_system)
    return type_system.takeError();
  return GetBasicType(*type_system, basic_type);
}

llvm::Expected<lldb::ValueObjectSP> Interpreter::EvaluateBinarySubtraction(
    lldb::ValueObjectSP lhs, lldb::ValueObjectSP rhs, uint32_t location) {
  // Operation '-' works for:
  //
  //  {scalar,unscoped_enum} <-> {scalar,unscoped_enum}
  //  pointer <-> {integer,unscoped_enum}
  //  pointer <-> pointer (if pointee types are compatible)
  auto orig_lhs_type = lhs->GetCompilerType();
  auto orig_rhs_type = rhs->GetCompilerType();
  auto type_or_err = ArithmeticConversion(lhs, rhs);
  if (!type_or_err)
    return type_or_err.takeError();
  CompilerType result_type = *type_or_err;

  if (result_type.IsScalarType())
    return EvaluateArithmeticOp(BinaryOpKind::Sub, lhs, rhs, result_type,
                                location);

  auto lhs_type = lhs->GetCompilerType();
  auto rhs_type = rhs->GetCompilerType();

  if (lhs_type.IsPointerType() && rhs_type.IsInteger()) {
    if (lhs_type.IsPointerToVoid())
      return llvm::make_error<DILDiagnosticError>(
          m_expr, "arithmetic on a pointer to void", location);
  } else if (lhs_type.IsPointerType() && rhs_type.IsPointerType()) {
    if (lhs_type.IsPointerToVoid() && rhs_type.IsPointerToVoid()) {
      return llvm::make_error<DILDiagnosticError>(
          m_expr, "arithmetic on pointers to void", location);
    }

    // Compare canonical unqualified pointer types.
    CompilerType lhs_unqualified_type =
        lhs_type.GetCanonicalType().GetFullyUnqualifiedType();
    CompilerType rhs_unqualified_type =
        rhs_type.GetCanonicalType().GetFullyUnqualifiedType();
    bool comparable = lhs_unqualified_type.CompareTypes(rhs_unqualified_type);
    if (!comparable) {
      std::string errMsg = llvm::formatv(
          "'{0}' and '{1}' are not pointers to compatible types",
          orig_lhs_type.GetTypeName(), orig_rhs_type.GetTypeName());
      return llvm::make_error<DILDiagnosticError>(m_expr, errMsg, location);
    }
  } else {
    std::string errMsg =
        llvm::formatv("invalid operands to binary expression ('{0}' and '{1}')",
                      orig_lhs_type.GetTypeName(), orig_rhs_type.GetTypeName());
    return llvm::make_error<DILDiagnosticError>(m_expr, errMsg, location);
  }

  // "pointer - integer" operation.
  if (rhs->GetCompilerType().IsInteger())
    return PointerAdd(lhs, -rhs->GetValueAsUnsigned(0));

  // "pointer - pointer" operation.
  llvm::Expected<uint64_t> lhs_byte_size =
      lhs->GetCompilerType().GetPointeeType().GetByteSize(
          lhs->GetTargetSP().get());
  if (!lhs_byte_size)
    return lhs_byte_size.takeError();
  llvm::Expected<uint64_t> rhs_byte_size =
      rhs->GetCompilerType().GetPointeeType().GetByteSize(
          rhs->GetTargetSP().get());
  if (!rhs_byte_size)
    return rhs_byte_size.takeError();
  // Since pointers have compatible types, both have the same pointee size.
  int64_t item_size = *lhs_byte_size;
  int64_t diff = static_cast<int64_t>(lhs->GetValueAsUnsigned(0) -
                                      rhs->GetValueAsUnsigned(0));
  if (diff % item_size != 0) {
    // If address difference isn't divisible by pointee size then performing
    // the operation is undefined behaviour.
    return llvm::make_error<DILDiagnosticError>(
        m_expr, "undefined pointer arithmetic", location);
  }
  diff /= item_size;

  // Pointer difference is ptrdiff_t.
  // TODO: lookup the actual type ptrdiff_t
  llvm::Expected<CompilerType> ptrdiff_t = GetPtrDiffType(m_exe_ctx_scope);
  if (!ptrdiff_t)
    return ptrdiff_t.takeError();
  ExecutionContext exe_ctx(m_target.get(), false);
  llvm::Expected<uint64_t> byte_size = ptrdiff_t->GetByteSize(m_target.get());
  if (!byte_size)
    return byte_size.takeError();
  lldb::DataExtractorSP data_sp = std::make_shared<DataExtractor>(
      reinterpret_cast<const void *>(&diff), *byte_size, exe_ctx.GetByteOrder(),
      exe_ctx.GetAddressByteSize());
  return ValueObject::CreateValueObjectFromData("result", *data_sp, exe_ctx,
                                                *ptrdiff_t);
}

lldb::ValueObjectSP
Interpreter::ConvertValueObjectToTypeSystem(lldb::ValueObjectSP valobj,
                                            lldb::TypeSystemSP type_system) {
  Scalar scalar;
  bool resolved = valobj->ResolveValue(scalar);
  if (resolved && type_system) {
    lldb::BasicType basic_type =
        valobj->GetCompilerType().GetCanonicalType().GetBasicTypeEnumeration();
    if (CompilerType compiler_type =
            type_system.get()->GetBasicTypeFromAST(basic_type)) {
      valobj->GetValue().SetCompilerType(compiler_type);
      return ValueObject::CreateValueObjectFromScalar(m_target, scalar,
                                                      compiler_type, "result");
    }
  }

  return lldb::ValueObjectSP();
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const BinaryOpNode *node) {
  auto lhs_or_err = EvaluateAndDereference(node->GetLHS());
  if (!lhs_or_err)
    return lhs_or_err;
  lldb::ValueObjectSP lhs = *lhs_or_err;
  auto rhs_or_err = EvaluateAndDereference(node->GetRHS());
  if (!rhs_or_err)
    return rhs_or_err;
  lldb::ValueObjectSP rhs = *rhs_or_err;

  lldb::TypeSystemSP lhs_system =
      lhs->GetCompilerType().GetTypeSystem().GetSharedPointer();
  lldb::TypeSystemSP rhs_system =
      rhs->GetCompilerType().GetTypeSystem().GetSharedPointer();

  if (lhs_system != rhs_system) {
    // If one of the nodes is a const literal, convert it to the
    // type system of another one
    if (node->GetLHS()->IsConstLiteral())
      lhs = ConvertValueObjectToTypeSystem(lhs, rhs_system);
    else if (node->GetRHS()->IsConstLiteral())
      rhs = ConvertValueObjectToTypeSystem(rhs, lhs_system);
    else
      return llvm::make_error<DILDiagnosticError>(
          m_expr, "incompatible type systems", node->GetLocation());
  }

  switch (node->GetKind()) {
  case BinaryOpKind::Add:
    return EvaluateBinaryAddition(lhs, rhs, node->GetLocation());
  case BinaryOpKind::Sub:
    return EvaluateBinarySubtraction(lhs, rhs, node->GetLocation());
  }

  return llvm::make_error<DILDiagnosticError>(
      m_expr, "unimplemented binary operation", node->GetLocation());
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const MemberOfNode *node) {
  auto base_or_err = Evaluate(node->GetBase());
  if (!base_or_err)
    return base_or_err;
  bool expr_is_ptr = node->GetIsArrow();
  lldb::ValueObjectSP base = *base_or_err;

  // Perform some basic type & correctness checking.
  if (node->GetIsArrow()) {
    if (!m_fragile_ivar) {
      // Make sure we aren't trying to deref an objective
      // C ivar if this is not allowed
      const uint32_t pointer_type_flags =
          base->GetCompilerType().GetTypeInfo(nullptr);
      if ((pointer_type_flags & lldb::eTypeIsObjC) &&
          (pointer_type_flags & lldb::eTypeIsPointer)) {
        // This was an objective C object pointer and it was requested we
        // skip any fragile ivars so return nothing here
        return lldb::ValueObjectSP();
      }
    }

    // If we have a non-pointer type with a synthetic value then lets check
    // if we have a synthetic dereference specified.
    if (!base->IsPointerType() && base->HasSyntheticValue()) {
      Status deref_error;
      if (lldb::ValueObjectSP synth_deref_sp =
              base->GetSyntheticValue()->Dereference(deref_error);
          synth_deref_sp && deref_error.Success()) {
        base = std::move(synth_deref_sp);
      }
      if (!base || deref_error.Fail()) {
        std::string errMsg = llvm::formatv(
            "Failed to dereference synthetic value: {0}", deref_error);
        return llvm::make_error<DILDiagnosticError>(
            m_expr, errMsg, node->GetLocation(), node->GetFieldName().size());
      }

      // Some synthetic plug-ins fail to set the error in Dereference
      if (!base) {
        std::string errMsg = "Failed to dereference synthetic value";
        return llvm::make_error<DILDiagnosticError>(
            m_expr, errMsg, node->GetLocation(), node->GetFieldName().size());
      }
      expr_is_ptr = false;
    }
  }

  if (m_check_ptr_vs_member) {
    bool base_is_ptr = base->IsPointerType();

    if (expr_is_ptr != base_is_ptr) {
      if (base_is_ptr) {
        std::string errMsg =
            llvm::formatv("member reference type {0} is a pointer; "
                          "did you mean to use '->'?",
                          base->GetCompilerType().TypeDescription());
        return llvm::make_error<DILDiagnosticError>(
            m_expr, errMsg, node->GetLocation(), node->GetFieldName().size());
      } else {
        std::string errMsg =
            llvm::formatv("member reference type {0} is not a pointer; "
                          "did you mean to use '.'?",
                          base->GetCompilerType().TypeDescription());
        return llvm::make_error<DILDiagnosticError>(
            m_expr, errMsg, node->GetLocation(), node->GetFieldName().size());
      }
    }
  }

  lldb::ValueObjectSP field_obj =
      base->GetChildMemberWithName(node->GetFieldName());
  if (!field_obj) {
    if (m_use_synthetic) {
      field_obj = base->GetSyntheticValue();
      if (field_obj)
        field_obj = field_obj->GetChildMemberWithName(node->GetFieldName());
    }

    if (!m_use_synthetic || !field_obj) {
      std::string errMsg = llvm::formatv(
          "\"{0}\" is not a member of \"({1}) {2}\"", node->GetFieldName(),
          base->GetTypeName().AsCString("<invalid type>"), base->GetName());
      return llvm::make_error<DILDiagnosticError>(
          m_expr, errMsg, node->GetLocation(), node->GetFieldName().size());
    }
  }

  if (field_obj) {
    if (m_use_dynamic != lldb::eNoDynamicValues) {
      lldb::ValueObjectSP dynamic_val_sp =
          field_obj->GetDynamicValue(m_use_dynamic);
      if (dynamic_val_sp)
        field_obj = dynamic_val_sp;
    }
    return field_obj;
  }

  CompilerType base_type = base->GetCompilerType();
  if (node->GetIsArrow() && base->IsPointerType())
    base_type = base_type.GetPointeeType();
  std::string errMsg = llvm::formatv(
      "\"{0}\" is not a member of \"({1}) {2}\"", node->GetFieldName(),
      base->GetTypeName().AsCString("<invalid type>"), base->GetName());
  return llvm::make_error<DILDiagnosticError>(
      m_expr, errMsg, node->GetLocation(), node->GetFieldName().size());
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const ArraySubscriptNode *node) {
  auto base_or_err = Evaluate(node->GetBase());
  if (!base_or_err)
    return base_or_err;
  lldb::ValueObjectSP base = *base_or_err;
  auto idx_or_err = EvaluateAndDereference(node->GetIndex());
  if (!idx_or_err)
    return idx_or_err;
  lldb::ValueObjectSP idx = *idx_or_err;

  if (!idx->GetCompilerType().IsIntegerOrUnscopedEnumerationType()) {
    return llvm::make_error<DILDiagnosticError>(
        m_expr, "array subscript is not an integer", node->GetLocation());
  }

  StreamString var_expr_path_strm;
  uint64_t child_idx = idx->GetValueAsUnsigned(0);
  lldb::ValueObjectSP child_valobj_sp;

  bool is_incomplete_array = false;
  CompilerType base_type = base->GetCompilerType().GetNonReferenceType();
  base->GetExpressionPath(var_expr_path_strm);

  if (base_type.IsPointerType()) {
    bool is_objc_pointer = true;

    if (base->GetCompilerType().GetMinimumLanguage() != lldb::eLanguageTypeObjC)
      is_objc_pointer = false;
    else if (!base->GetCompilerType().IsPointerType())
      is_objc_pointer = false;

    if (!m_use_synthetic && is_objc_pointer) {
      std::string err_msg = llvm::formatv(
          "\"({0}) {1}\" is an Objective-C pointer, and cannot be subscripted",
          base->GetTypeName().AsCString("<invalid type>"),
          var_expr_path_strm.GetData());
      return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                  node->GetLocation());
    }
    if (is_objc_pointer) {
      lldb::ValueObjectSP synthetic = base->GetSyntheticValue();
      if (!synthetic || synthetic == base) {
        std::string err_msg =
            llvm::formatv("\"({0}) {1}\" is not an array type",
                          base->GetTypeName().AsCString("<invalid type>"),
                          var_expr_path_strm.GetData());
        return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                    node->GetLocation());
      }
      if (static_cast<uint32_t>(child_idx) >=
          synthetic->GetNumChildrenIgnoringErrors()) {
        std::string err_msg = llvm::formatv(
            "array index {0} is not valid for \"({1}) {2}\"", child_idx,
            base->GetTypeName().AsCString("<invalid type>"),
            var_expr_path_strm.GetData());
        return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                    node->GetLocation());
      }
      child_valobj_sp = synthetic->GetChildAtIndex(child_idx);
      if (!child_valobj_sp) {
        std::string err_msg = llvm::formatv(
            "array index {0} is not valid for \"({1}) {2}\"", child_idx,
            base->GetTypeName().AsCString("<invalid type>"),
            var_expr_path_strm.GetData());
        return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                    node->GetLocation());
      }
      if (m_use_dynamic != lldb::eNoDynamicValues) {
        if (auto dynamic_sp = child_valobj_sp->GetDynamicValue(m_use_dynamic))
          child_valobj_sp = std::move(dynamic_sp);
      }
      return child_valobj_sp;
    }

    child_valobj_sp = base->GetSyntheticArrayMember(child_idx, true);
    if (!child_valobj_sp) {
      std::string err_msg = llvm::formatv(
          "failed to use pointer as array for index {0} for "
          "\"({1}) {2}\"",
          child_idx, base->GetTypeName().AsCString("<invalid type>"),
          var_expr_path_strm.GetData());
      if (base_type.IsPointerToVoid())
        err_msg = "subscript of pointer to incomplete type 'void'";
      return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                  node->GetLocation());
    }
  } else if (base_type.IsArrayType(nullptr, nullptr, &is_incomplete_array)) {
    child_valobj_sp = base->GetChildAtIndex(child_idx);
    if (!child_valobj_sp && (is_incomplete_array || m_use_synthetic))
      child_valobj_sp = base->GetSyntheticArrayMember(child_idx, true);
    if (!child_valobj_sp) {
      std::string err_msg = llvm::formatv(
          "array index {0} is not valid for \"({1}) {2}\"", child_idx,
          base->GetTypeName().AsCString("<invalid type>"),
          var_expr_path_strm.GetData());
      return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                  node->GetLocation());
    }
  } else if (base_type.IsScalarType()) {
    child_valobj_sp =
        base->GetSyntheticBitFieldChild(child_idx, child_idx, true);
    if (!child_valobj_sp) {
      std::string err_msg = llvm::formatv(
          "bitfield range {0}:{1} is not valid for \"({2}) {3}\"", child_idx,
          child_idx, base->GetTypeName().AsCString("<invalid type>"),
          var_expr_path_strm.GetData());
      return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                  node->GetLocation(), 1);
    }
  } else {
    lldb::ValueObjectSP synthetic = base->GetSyntheticValue();
    if (!m_use_synthetic || !synthetic || synthetic == base) {
      std::string err_msg =
          llvm::formatv("\"{0}\" is not an array type",
                        base->GetTypeName().AsCString("<invalid type>"));
      return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                  node->GetLocation(), 1);
    }
    if (static_cast<uint32_t>(child_idx) >=
        synthetic->GetNumChildrenIgnoringErrors(child_idx + 1)) {
      std::string err_msg = llvm::formatv(
          "array index {0} is not valid for \"({1}) {2}\"", child_idx,
          base->GetTypeName().AsCString("<invalid type>"),
          var_expr_path_strm.GetData());
      return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                  node->GetLocation(), 1);
    }
    child_valobj_sp = synthetic->GetChildAtIndex(child_idx);
    if (!child_valobj_sp) {
      std::string err_msg = llvm::formatv(
          "array index {0} is not valid for \"({1}) {2}\"", child_idx,
          base->GetTypeName().AsCString("<invalid type>"),
          var_expr_path_strm.GetData());
      return llvm::make_error<DILDiagnosticError>(m_expr, std::move(err_msg),
                                                  node->GetLocation(), 1);
    }
  }

  if (child_valobj_sp) {
    if (m_use_dynamic != lldb::eNoDynamicValues) {
      if (auto dynamic_sp = child_valobj_sp->GetDynamicValue(m_use_dynamic))
        child_valobj_sp = std::move(dynamic_sp);
    }
    return child_valobj_sp;
  }

  int64_t signed_child_idx = idx->GetValueAsSigned(0);
  return base->GetSyntheticArrayMember(signed_child_idx, true);
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const BitFieldExtractionNode *node) {
  auto base_or_err = EvaluateAndDereference(node->GetBase());
  if (!base_or_err)
    return base_or_err;
  lldb::ValueObjectSP base = *base_or_err;
  auto first_idx_or_err = EvaluateAndDereference(node->GetFirstIndex());
  if (!first_idx_or_err)
    return first_idx_or_err;
  lldb::ValueObjectSP first_idx = *first_idx_or_err;
  auto last_idx_or_err = EvaluateAndDereference(node->GetLastIndex());
  if (!last_idx_or_err)
    return last_idx_or_err;
  lldb::ValueObjectSP last_idx = *last_idx_or_err;

  if (!first_idx->GetCompilerType().IsIntegerOrUnscopedEnumerationType() ||
      !last_idx->GetCompilerType().IsIntegerOrUnscopedEnumerationType()) {
    return llvm::make_error<DILDiagnosticError>(
        m_expr, "bit index is not an integer", node->GetLocation());
  }

  int64_t first_index = first_idx->GetValueAsSigned(0);
  int64_t last_index = last_idx->GetValueAsSigned(0);

  // if the format given is [high-low], swap range
  if (first_index > last_index)
    std::swap(first_index, last_index);

  lldb::ValueObjectSP child_valobj_sp =
      base->GetSyntheticBitFieldChild(first_index, last_index, true);
  if (!child_valobj_sp) {
    std::string message = llvm::formatv(
        "bitfield range {0}:{1} is not valid for \"({2}) {3}\"", first_index,
        last_index, base->GetTypeName().AsCString("<invalid type>"),
        base->GetName().AsCString());
    return llvm::make_error<DILDiagnosticError>(m_expr, message,
                                                node->GetLocation());
  }
  return child_valobj_sp;
}

llvm::Expected<CompilerType>
Interpreter::PickIntegerType(lldb::TypeSystemSP type_system,
                             std::shared_ptr<ExecutionContextScope> ctx,
                             const IntegerLiteralNode *literal) {
  // Binary, Octal, Hexadecimal and literals with a U suffix are allowed to be
  // an unsigned integer.
  bool unsigned_is_allowed = literal->IsUnsigned() || literal->GetRadix() != 10;
  llvm::APInt apint = literal->GetValue();

  llvm::SmallVector<std::pair<lldb::BasicType, lldb::BasicType>, 3> candidates;
  if (literal->GetTypeSuffix() <= IntegerTypeSuffix::None)
    candidates.emplace_back(lldb::eBasicTypeInt,
                            unsigned_is_allowed ? lldb::eBasicTypeUnsignedInt
                                                : lldb::eBasicTypeInvalid);
  if (literal->GetTypeSuffix() <= IntegerTypeSuffix::Long)
    candidates.emplace_back(lldb::eBasicTypeLong,
                            unsigned_is_allowed ? lldb::eBasicTypeUnsignedLong
                                                : lldb::eBasicTypeInvalid);
  candidates.emplace_back(lldb::eBasicTypeLongLong,
                          lldb::eBasicTypeUnsignedLongLong);
  for (auto [signed_, unsigned_] : candidates) {
    CompilerType signed_type = type_system->GetBasicTypeFromAST(signed_);
    if (!signed_type)
      continue;
    llvm::Expected<uint64_t> size = signed_type.GetBitSize(ctx.get());
    if (!size)
      return size.takeError();
    if (!literal->IsUnsigned() && apint.isIntN(*size - 1))
      return signed_type;
    if (unsigned_ != lldb::eBasicTypeInvalid && apint.isIntN(*size))
      return type_system->GetBasicTypeFromAST(unsigned_);
  }

  return llvm::make_error<DILDiagnosticError>(
      m_expr,
      "integer literal is too large to be represented in any integer type",
      literal->GetLocation());
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const IntegerLiteralNode *node) {
  llvm::Expected<lldb::TypeSystemSP> type_system =
      GetTypeSystemFromCU(m_exe_ctx_scope);
  if (!type_system)
    return type_system.takeError();

  llvm::Expected<CompilerType> type =
      PickIntegerType(*type_system, m_exe_ctx_scope, node);
  if (!type)
    return type.takeError();

  Scalar scalar = node->GetValue();
  // APInt from StringRef::getAsInteger comes with just enough bitwidth to
  // hold the value. This adjusts APInt bitwidth to match the compiler type.
  llvm::Expected<uint64_t> type_bitsize =
      type->GetBitSize(m_exe_ctx_scope.get());
  if (!type_bitsize)
    return type_bitsize.takeError();
  scalar.TruncOrExtendTo(*type_bitsize, false);
  return ValueObject::CreateValueObjectFromScalar(m_target, scalar, *type,
                                                  "result");
}

llvm::Expected<lldb::ValueObjectSP>
Interpreter::Visit(const FloatLiteralNode *node) {
  llvm::Expected<lldb::TypeSystemSP> type_system =
      GetTypeSystemFromCU(m_exe_ctx_scope);
  if (!type_system)
    return type_system.takeError();

  bool isFloat =
      &node->GetValue().getSemantics() == &llvm::APFloat::IEEEsingle();
  lldb::BasicType basic_type =
      isFloat ? lldb::eBasicTypeFloat : lldb::eBasicTypeDouble;
  CompilerType type = GetBasicType(*type_system, basic_type);

  if (!type)
    return llvm::make_error<DILDiagnosticError>(
        m_expr, "unable to create a const literal", node->GetLocation());

  Scalar scalar = node->GetValue();
  return ValueObject::CreateValueObjectFromScalar(m_target, scalar, type,
                                                  "result");
}

} // namespace lldb_private::dil
