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
  test_binary.bin
  test_binary.cc
  )
add_unittest_inputs(LLDBEvalTests "${test_inputs}")
