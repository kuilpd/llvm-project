# lldb-eval in LLDB

This project is an integration of [lldb-eval](https://github.com/google/lldb-eval/) into LLVM infrastructure.

Originally `lldb-eval` was made as a separate project that uses Bazel as a building tool, and the project is no longer maintained.
It has its own API that executes expression evaluation and relies on LLDB API for retrieving all the needed debug information.

In order to maintain the project easier and simplify its usage within LLDB, this project integrates `lldb-eval` into LLDB and can be built alongside it using CMake.

This project adds:
1. Main implementation of `lldb-eval` as an LLDB library `Eval`.
2. `lldb-eval` tool for quick testing of expression evaluation speed and results compared to standard LLDB implementation.
3. Changes to `lldb-dap` and some core LLDB files that utilize `lldb-eval` to speed up expression evaluation in certain scenarios:
   * Evaluating conditional breakpoints.
   * Creating values from expression in LLDB's [Custom Data Formatters](https://github.com/vadimcn/codelldb/wiki/Custom-Data-Formatters) by using Python and CreateValueFromExpression API function.
   * Explicit expression evaluation for watched expressions in an IDE.
4. Unit tests for testing `lldb-eval` functionality.
5. Fuzzers, which can be built by adding `-DLLVM_USE_SANITIZE_COVERAGE=On` to the CMake configuration.

## Building
It can be built normally either as a part of [LLVM](https://lldb.llvm.org/resources/build.html) or a [standalone LLDB build](https://lldb.llvm.org/resources/build.html).

## Usage
1. Use `lldb-eval` tool to quickly launch a binary and stop at a breakpoint to try out expression evaluation and compare `lldb-eval` results to LLDB.
2. Use LLDB through the Debugger Adapter Protocol in an IDE. You can use the [LLDB DAP](https://marketplace.visualstudio.com/items?itemName=llvm-vs-code-extensions.lldb-dap) plugin for [Visual Studio Code](https://code.visualstudio.com/). Specify the path to `lldb-dap` binary in the plugin settings and create a launch configuration like this:
```
{
  "type": "lldb-dap",
  "request": "launch",
  "name": "Debug via LLDB DAP",
  "program": "/tmp/a.out",
}
```
Further setting for launch configurations can be found on the [plugin page](https://marketplace.visualstudio.com/items?itemName=llvm-vs-code-extensions.lldb-dap).

3. You can disable using `lldb-eval` for evaluating expression by adding `settings set target.use-eval-for-expressions false` to `.lldbinit`

## Performance
You should expect the speedup in expression evaluation to be anywhere from 10 to 60 times, depending on the complexity of the expression and the amount of debug information. Usually, the longer the expression and the larger debug information amount are, the faster the execution is relative to default LLDB implementation.
Examples:
* In a small file with a loop and a conditional breakpoint that triggers after 5000 iterations, time spend on expression evaluation goes from 12 seconds with LLDB to 1 second with `lldb-eval`.
* Unreal Engine 5 uses custom data formatters to display summaries for certain classes, which often use expression evaluation. After triggering a breakpoint in the middle of gameplay every expression evaluation goes from 100 ms with LLDB to 1.5 ms with `lldb-eval`, and every debugging step with expanded global variables does ~20 evaluations, which ends up speeding up every step by full 2 seconds.
