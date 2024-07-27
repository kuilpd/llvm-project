// Copyright 2020 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <chrono>
#include <iostream>
#include <memory>
#include <string>

#include "cpp-linenoise/linenoise.hpp"
#include "lldb/Eval/api.h"
#include "lldb-eval/runner.h"
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

void EvalExpr(lldb::SBFrame frame, const std::string& expr) {
  lldb::SBError error;
  lldb::SBValue value;

  lldb_eval::Options opts;
  opts.allow_side_effects = true;

  auto elapsed = timer([&]() {
    value = lldb_eval::EvaluateExpression(frame, expr.c_str(), opts, error);
  });

  std::cerr << "== lldb-eval == " << std::endl;
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

void EvalExprLLDB(lldb::SBFrame frame, const std::string& expr) {
  lldb::SBError error;
  lldb::SBValue value;

  lldb::SBExpressionOptions options;
  options.SetAutoApplyFixIts(false);

  auto elapsed = timer([&]() {
    value = frame.EvaluateExpression(expr.c_str(), options);
    error = value.GetError();
  });

  std::cerr << "== LLDB == " << std::endl;
  if (error.GetError()) {
    std::cerr << error.GetCString() << std::endl;
  } else {
    std::cerr << "value = " << maybe_null(value.GetValue()) << std::endl;
    std::cerr << "type  = " << value.GetTypeName() << std::endl;
  }

  std::cerr << "elapsed = " << elapsed << "us" << std::endl;
}

void RunRepl(lldb::SBFrame frame) {
  linenoise::SetMultiLine(true);
  std::string expr;

  std::cerr << "Stopped at:" << std::endl;
  std::cerr << "\t" << frame.GetFunctionName() << ":"
            << frame.GetLineEntry().GetLine() << ":"
            << frame.GetLineEntry().GetColumn() << std::endl;

  while (true) {
    bool quit = linenoise::Readline("> ", expr);
    if (quit) {
      break;
    }

    EvalExpr(frame, expr);
    EvalExprLLDB(frame, expr);
    std::cerr << "----------" << std::endl;

    linenoise::AddHistory(expr.c_str());
  }
}

static int printUsageAndExit() {
  std::cerr << "Usage: \n"
               "  lldb-eval -n SymbolName Binary [<args>]\n"
               "or \n"
               "  lldb-eval -f File:Line Binary [<args>]\n";
  return -1;
}

int main(int argc, char** argv) {

  lldb::SBDebugger::Initialize();
  lldb::SBDebugger debugger = lldb::SBDebugger::Create(false);

  if (argc < 4)
    return printUsageAndExit();

  lldb::SBProcess process;
  std::string command = argv[1];
  if (command == "-n") {
    auto name = argv[2];
    auto break_line = 0;
    auto binary_path = argv[3];
    const char** binary_argv = const_cast<const char**>(&argv[4]);
    process = lldb_eval::LaunchTestProgram(
        debugger, name, break_line, binary_path, binary_argv);
  } else if (command == "-f") {
    auto file_line = llvm::StringRef(argv[2]).split(':');
    if (file_line.second.empty())
      return printUsageAndExit();
    auto name = file_line.first.str();
    auto break_line = atoi(file_line.second.str().c_str());
    auto binary_path = argv[3];
    const char** binary_argv = const_cast<const char**>(&argv[4]);
    process = lldb_eval::LaunchTestProgram(
        debugger, name, break_line, binary_path, binary_argv);
  } else
    return printUsageAndExit();

  if (!process.IsValid())
    return -1;

  lldb::SBFrame frame = process.GetSelectedThread().GetSelectedFrame();

  bool repl_mode = true;
  std::string expr;
  if (repl_mode) {
    RunRepl(frame);
  } else {
    EvalExpr(frame, expr);
    EvalExprLLDB(frame, expr);
  }

  process.Destroy();
  lldb::SBDebugger::Terminate();

  return 0;
}
