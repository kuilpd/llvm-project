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
        self.expect_var_path("func0(1)", value="101")
        self.expect_var_path("func0(1, 10, 100, 1000)", value="1111")
        self.expect_var_path("func0(1.0f)", value="101.25")
        self.expect_var_path("func0(1.0f, 2, 3.0)", value="6")
        self.expect_var_path("debase(&nsbase, 100)", value="110")
        self.expect_var_path("dsum(4, 1.0, 2.0, 3.0, 4.0)", value="10")
        self.expect(
            "frame var -- 'dsum(1.0, 2.0)'",
            error=True,
            substrs=["no matching function for call to 'dsum'"],
        )
        self.expect(
            "frame var -- 'dsum(1, nsbase)'",
            error=True,
            substrs=["function call validation failed: unsupported type of arg2"],
        )

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

        # Method calls with arguments
        self.expect_var_path("base.method(1)", value="400")
        self.expect_var_path("base.method(1.0f)", value="101.25")
        self.expect_var_path("base.method(1.0f, 2, 3.0)", value="105")
        self.expect_var_path("base.method(1, 10, 100, 1000)", value="1210")
        self.expect_var_path("p_base->method(1.0f, 2, 3.0)", value="105")
        self.expect_var_path("base.member_add(4, 1, 2, 3, 4)", value="109")
        self.expect_var_path("base.get_member()", value="109")
        self.expect(
            "frame var -- 'base.member_add(1.0, 2.0)'",
            error=True,
            substrs=["no member function named 'member_add' in 'Base'"],
        )
        self.expect(
            "frame var -- 'base.member_add(1, nsbase)'",
            error=True,
            substrs=["function call validation failed: unsupported type of arg3"],
        )
