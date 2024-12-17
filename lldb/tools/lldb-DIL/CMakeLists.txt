add_lldb_tool(lldb-DIL
  runner.cpp
  main.cpp

  CLANG_LIBS
    clangBasic
    clangLex

  LINK_LIBS
    liblldb

  LINK_COMPONENTS
    Support
  )
