add_lldb_tool(lldb-eval
  lldb-eval/api.cc
  lldb-eval/ast.cc
  lldb-eval/context.cc
  lldb-eval/eval.cc
  lldb-eval/parser.cc
  lldb-eval/parser_context.cc
  lldb-eval/type.cc
  lldb-eval/value.cc
  lldb-eval/runner.cc
  lexer.cc
  exec.cc

  CLANG_LIBS
    clangBasic
    clangLex

  LINK_LIBS
    liblldb

  LINK_COMPONENTS
    Support
  )
