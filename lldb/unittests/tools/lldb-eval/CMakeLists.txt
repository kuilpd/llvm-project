add_subdirectory(libfuzzer_test)

add_lldb_unittest(LLDBEvalTests
  eval_test.cc

  LINK_LIBS
    liblldb
    lldbEval
    lldbUtilityHelpers
    LLVMTestingSupport
  )

add_lldb_library(lldbEvalFuzzer
  fuzzer/ast.cc
  fuzzer/constraints.cc
  fuzzer/expr_gen.cc
  fuzzer/fixed_rng.cc
  fuzzer/gen_node.cc
  fuzzer/libfuzzer_common.cc
  fuzzer/symbol_table.cc

  LINK_LIBS
    liblldb
    lldbEval
  )

add_lldb_unittest(LLDBEvalMutateTests
  mutate_test.cc

  LINK_LIBS
    liblldb
    lldbEval
    lldbEvalFuzzer
    lldbUtilityHelpers
    LLVMTestingSupport
  )

add_lldb_unittest(LLDBEvalUtilsTests
  libfuzzer_utils_test.cc

  LINK_LIBS
    liblldb
    lldbEval
    lldbEvalFuzzer
  )

add_lldb_unittest(LLDBEvalSymbolTableTests
  symbol_table_test.cc

  LINK_LIBS
    liblldb
    lldbEval
    lldbEvalFuzzer
    lldbUtilityHelpers
    LLVMTestingSupport
  )

add_lldb_unittest(LLDBEvalFuzzerTests
  fuzzer_test.cc

  LINK_LIBS
    liblldb
    lldbEval
    lldbEvalFuzzer
  )

set(test_lib
  test_binary_libc++.bin
  test_binary_libstdc++.bin
  test_binary.cc
  )
set(test_fuzzer
  fuzzer_binary.bin
  fuzzer_binary.cc
  )
add_unittest_inputs(LLDBEvalTests "${test_lib}")
add_unittest_inputs(LLDBEvalMutateTests "${test_fuzzer}")
add_unittest_inputs(LLDBEvalSymbolTableTests "${test_fuzzer}")


add_lldb_tool(lldb-eval-fuzzer
  fuzzer_main.cc

  CLANG_LIBS
    clangBasic
    clangLex

  LINK_LIBS
    liblldb
    lldbEval
    lldbEvalFuzzer
  )


