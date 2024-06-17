
set(LLVM_LINK_COMPONENTS
  FuzzMutate
  FuzzerCLI
  )

add_llvm_fuzzer(lldb-eval-libfuzzer-test
  EXCLUDE_FROM_ALL
  lldb_eval_libfuzzer_test.cc
  )

if(TARGET lldb-eval-libfuzzer-test)
  target_include_directories(lldb-eval-libfuzzer-test PRIVATE ..)
  target_link_libraries(lldb-eval-libfuzzer-test
    PRIVATE
    liblldb
    lldbEval
    lldbEvalFuzzer
    )
endif()

add_llvm_fuzzer(lldb-eval-vs-lldb-test
  EXCLUDE_FROM_ALL
  lldb_vs_lldb_eval_libfuzzer_test.cc
  )

if(TARGET lldb-eval-vs-lldb-test)
  target_include_directories(lldb-eval-vs-lldb-test PRIVATE ..)
  target_link_libraries(lldb-eval-vs-lldb-test
    PRIVATE
    liblldb
    lldbEval
    lldbEvalFuzzer
    )
endif()
