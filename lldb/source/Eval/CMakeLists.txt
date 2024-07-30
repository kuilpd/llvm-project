add_lldb_library(lldbEval NO_PLUGIN_DEPENDENCIES
  api.cc
  ast.cc
  context.cc
  eval.cc
  parser.cc
  parser_context.cc
  runner.cc
  type.cc
  value.cc

  LINK_LIBS
    clangBasic
    clangLex
    lldbCore
    lldbHost
    lldbInterpreter
    lldbSymbol
    lldbTarget
    lldbUtility

  LINK_COMPONENTS
    Support
  )
