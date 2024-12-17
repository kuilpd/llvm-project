#include <chrono>
#include <iostream>
#include <memory>
#include <string>

#include "lldb/lldb-enumerations.h"
#include "runner.h"
#include "lldb/API/SBExpressionOptions.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "llvm/ADT/StringRef.h"

int64_t timer(std::function<void()> func) {
  auto start = std::chrono::high_resolution_clock::now();
  func();
  auto total = std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start);

  return total.count();
}

const char* maybe_null(const char* str) {
  return str == nullptr ? "NULL" : str;
}

void Expr(lldb::SBFrame frame, const std::string& expr) {
  lldb::SBError error;
  lldb::SBValue value;

  auto elapsed = timer([&]() {
    value = frame.EvaluateExpressionViaDIL(expr.c_str(), lldb::eNoDynamicValues);
  });

  std::cerr << "== lldb-DIL == " << std::endl;
  if (error.GetError()) {
    std::cerr << error.GetCString() << std::endl;
  } else {
    // Due to various bugs result can still be NULL even though there was no
    // error reported. Printing NULL leads to segfault, so check and replace it.
    if (value.IsValid()) {
      std::cerr << "value = " << maybe_null(value.GetValue()) << std::endl;
      std::cerr << "type  = " << value.GetTypeName() << std::endl;
    } else {
      std::cerr << "Unknown error, result is invalid." << std::endl;
    }
  }

  std::cerr << "elapsed = " << elapsed << "us" << std::endl;
}

int main(int argc, char** argv) {
  lldb::SBDebugger::Initialize();
  lldb::SBDebugger debugger = lldb::SBDebugger::Create(false);
  lldb::SBProcess process;

  auto name = "/home/ikuklin/dev/test/lldb-eval/temp.cpp";
  auto break_line = 68;
  auto binary_path = "/home/ikuklin/dev/test/lldb-eval/temp";
  const char** binary_argv = const_cast<const char**>(&argv[1]);
  process = LaunchTestProgram(debugger, name, break_line, binary_path, binary_argv);

  lldb::SBFrame frame = process.GetSelectedThread().GetSelectedFrame();

  Expr(frame, "-enum_one");

  process.Destroy();
  lldb::SBDebugger::Terminate();

  return 0;
}