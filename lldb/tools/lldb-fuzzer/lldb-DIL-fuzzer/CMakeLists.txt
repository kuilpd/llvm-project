
set(LLVM_LINK_COMPONENTS
  FuzzMutate
  FuzzerCLI
  Support
  )

add_llvm_fuzzer(lldb-DIL-libfuzzer-test
  EXCLUDE_FROM_ALL
  lldb_DIL_libfuzzer_test.cpp
  runner.cpp
  fuzzer/ast.cc
  fuzzer/constraints.cc
  fuzzer/expr_gen.cc
  fuzzer/fixed_rng.cc
  fuzzer/gen_node.cc
  fuzzer/libfuzzer_common.cc
  fuzzer/symbol_table.cc
  )

if(TARGET lldb-DIL-libfuzzer-test)
  target_include_directories(lldb-DIL-libfuzzer-test PRIVATE ..)
  target_link_libraries(lldb-DIL-libfuzzer-test
    PRIVATE
    liblldb
    lldbFuzzerUtils
    )
endif()

# add_llvm_fuzzer(lldb-eval-vs-lldb-test
#   EXCLUDE_FROM_ALL
#   lldb_vs_lldb_eval_libfuzzer_test.cc
#   )

# if(TARGET lldb-eval-vs-lldb-test)
#   target_include_directories(lldb-eval-vs-lldb-test PRIVATE ..)
#   target_link_libraries(lldb-eval-vs-lldb-test
#     PRIVATE
#     liblldb
#     lldbEval
#     lldbEvalFuzzer
#     )
# endif()

