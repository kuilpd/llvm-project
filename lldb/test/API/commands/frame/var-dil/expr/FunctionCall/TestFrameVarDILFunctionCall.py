"""
Test DIL function and method calls.
"""

import lldb
from lldbsuite.test.lldbtest import *
from lldbsuite.test.decorators import *
from lldbsuite.test import lldbutil


class TestFrameVarDILFunctionCall(TestBase):
    NO_DEBUG_INFO_TESTCASE = True

    def test_arithmetic(self):
        self.build()
        lldbutil.run_to_source_breakpoint(
            self, "Set a breakpoint here", lldb.SBFileSpec("main.cpp")
        )
        self.runCmd("settings set target.experimental.use-DIL true")

        # Function calls
        self.expect_var_path("func0()", value="0")
        self.expect_var_path("ns::func0()", value="1")
        self.expect_var_path("func1()", value="101")
        self.expect_var_path("ns::func1()", value="101")
        self.expect_var_path("array()", value="11")
        self.expect_var_path("ns::func3()", value="103")
        self.expect(
            "frame var -- 'func3()'",
            error=True,
            substrs=["call to 'func3' is ambiguous"],
        )

        # Function calls with arguments
        self.expect_var_path("func0(1)", value="2")
        # self.expect_var_path("func0(1.0f)", value="111")
        self.expect_var_path("func0(1, 10, 100, 1000)", value="1111")
        self.expect_var_path("debase(&nsbase, 100)", value="110")

        # Static method calls
        self.expect_var_path("ns::Base::func2()", value="200")
        self.expect_var_path("Base::func2()", value="201")
        self.expect(
            "frame var -- 'func2()'",
            error=True,
            substrs=["no matching function for call to 'func2'"],
        )

        # Method calls
        self.expect_var_path("nsbase.method()", value="110")
        self.expect_var_path("r_nsderived.method()", value="210")
        self.expect_var_path("p_nsbase->method()", value="120")
        self.expect_var_path("p_nsderived->method()", value="220")
        self.expect_var_path("base.method()", value="399")
        self.expect_var_path("p_nsbase->func2()", value="200")
        self.expect_var_path("base.func2()", value="201")
        self.expect_var_path("uni.method()", value="1")
        self.expect(
            "frame var -- 'base.Base()'",
            error=True,
            substrs=["no member function named 'Base' in 'Base'"],
        )
        self.expect(
            "frame var -- 'arr_ptr->method()'",
            error=True,
            substrs=[
                "member reference base type 'const int' is not a structure or union"
            ],
        )
