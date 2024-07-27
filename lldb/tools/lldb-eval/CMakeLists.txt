add_lldb_tool(lldb-eval
  lldb-eval/runner.cc
  lexer.cc
  exec.cc

  CLANG_LIBS
    clangBasic
    clangLex

  LINK_LIBS
    liblldb
    lldbEval

  LINK_COMPONENTS
    Support
  )
