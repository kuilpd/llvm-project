
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

# add_llvm_fuzzer(lldb-DIL-vs-lldb-libfuzzer-test
#   EXCLUDE_FROM_ALL
#   lldb_DIL_vs_lldb_libfuzzer_test.cpp
#   runner.cpp
#   fuzzer/ast.cc
#   fuzzer/constraints.cc
#   fuzzer/expr_gen.cc
#   fuzzer/fixed_rng.cc
#   fuzzer/gen_node.cc
#   fuzzer/libfuzzer_common.cc
#   fuzzer/symbol_table.cc
#   )

# if(TARGET lldb-DIL-vs-lldb-libfuzzer-test)
#   target_include_directories(lldb-DIL-vs-lldb-libfuzzer-test PRIVATE ..)
#   target_link_libraries(lldb-DIL-vs-lldb-libfuzzer-test
#     PRIVATE
#     liblldb
#     lldbFuzzerUtils
#     )
# endif()



