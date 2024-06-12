add_lldb_unittest(LLDBEvalTests
  lldb-eval/api.cc
  lldb-eval/ast.cc
  lldb-eval/context.cc
  lldb-eval/eval.cc
  lldb-eval/parser.cc
  lldb-eval/parser_context.cc
  lldb-eval/type.cc
  lldb-eval/value.cc
  lldb-eval/runner.cc
  eval_test.cc

  LINK_LIBS
    clangBasic
    clangLex
    lldbHostHelpers
    lldbBreakpoint
    lldbCommands
    lldbCore
    lldbDataFormatters
    lldbExpression
    lldbHost
    lldbInitialization
    lldbInterpreter
    lldbSymbol
    lldbTarget
    lldbUtility
    lldbUtilityHelpers
    liblldb
    LLVMTestingSupport

  LINK_COMPONENTS
    Support
  )

set(test_inputs
  test_binary_libc++.bin
  test_binary_libstdc++.bin
  test_binary.cc
  )
add_unittest_inputs(LLDBEvalTests "${test_inputs}")

add_lldb_tool(lldb-eval-fuzzer
  lldb-eval/api.cc
  lldb-eval/ast.cc
  lldb-eval/context.cc
  lldb-eval/eval.cc
  lldb-eval/parser.cc
  lldb-eval/parser_context.cc
  lldb-eval/type.cc
  lldb-eval/value.cc
  lldb-eval/runner.cc
  fuzzer/ast.cc
  fuzzer/constraints.cc
  fuzzer/expr_gen.cc
  fuzzer/fixed_rng.cc
  fuzzer/gen_node.cc
  fuzzer/symbol_table.cc
  fuzzer_main.cc

  CLANG_LIBS
    clangBasic
    clangLex

  LINK_LIBS
    liblldb

  LINK_COMPONENTS
    Support
  )
