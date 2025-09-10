"""
Test DIL arithmetic.
"""

import lldb
from lldbsuite.test.lldbtest import *
from lldbsuite.test.decorators import *
from lldbsuite.test import lldbutil


class TestFrameVarDILArithmetic(TestBase):
    NO_DEBUG_INFO_TESTCASE = True

    def test_arithmetic(self):
        self.build()
        (target, process, thread, bkpt) = lldbutil.run_to_source_breakpoint(
            self, "Set a breakpoint here", lldb.SBFileSpec("main.cpp")
        )

        self.runCmd("settings set target.experimental.use-DIL true")

        # Check unary results and integral promotion
        self.expect_var_path("+0", value="0")
        self.expect_var_path("-0", value="0")
        self.expect_var_path("+1", value="1")
        self.expect_var_path("-1", value="-1")
        self.expect_var_path("-9223372036854775808", value="9223372036854775808")
        self.expect_var_path("s", value="10", type="short")
        self.expect_var_path("+s", value="10", type="int")
        self.expect_var_path("-s", value="-10", type="int")
        self.expect_var_path("+us", value="1", type="int")
        self.expect_var_path("-us", value="-1", type="int")
        self.expect_var_path("+0.0", value="0")
        self.expect_var_path("-0.0", value="-0")
        self.expect_var_path("+enum_one", value="1")
        self.expect_var_path("-enum_one", value="-1")
        self.expect_var_path("+wchar", value="1")
        self.expect_var_path("+char16", value="2")
        self.expect_var_path("+char32", value="3")
        self.expect_var_path("-bitfield.a", value="-1", type="int")
        self.expect_var_path("+bitfield.a", value="1", type="int")
        self.expect_var_path("+bitfield.b", value="2", type="int")
        self.expect_var_path("+bitfield.c", value="3", type="unsigned int")
        self.expect_var_path("+bitfield.d", value="4", type="uint64_t")

        # Check basic math and resulting types
        self.expect_var_path("1 + 2", value="3", type="int")
        # self.expect_var_path("1 + 2*3", value="7")
        self.expect_var_path("1 + (2 - 3)", value="0")
        self.expect_var_path("s + x", value="12", type="int")
        self.expect_var_path("s + l", value="15", type="long")
        self.expect_var_path("1.0 + 2.5", value="3.5", type="double")
        self.expect_var_path("1 + 2.5f", value="3.5", type="float")
        self.expect_var_path("2. + .5", value="2.5", type="double")
        self.expect_var_path("2.f + .5f", value="2.5", type="float")
        self.expect_var_path("f + d", value="3.5", type="double")
        # self.expect_var_path("0.0 / 0", value="NaN")
        # self.expect_var_path("0 / 0.0", value="NaN")
        # self.expect_var_path("1 / +0.0", value="+Inf")
        # self.expect_var_path("1 / -0.0", value="-Inf")

        # Check limits and overflows
        frame = thread.GetFrameAtIndex(0)
        int_min = frame.GetValueForVariablePath("int_min").GetValue()
        int_max = frame.GetValueForVariablePath("int_max").GetValue()
        uint_max = frame.GetValueForVariablePath("uint_max").GetValue()
        ll_max = frame.GetValueForVariablePath("ll_max").GetValue()
        ll_min = frame.GetValueForVariablePath("ll_min").GetValue()
        ull_max = frame.GetValueForVariablePath("ull_max").GetValue()
        self.expect_var_path("int_max + 1", value=int_min)
        self.expect_var_path("int_min - 1", value=int_max)
        self.expect_var_path("uint_max + 1", value="0")
        self.expect_var_path("uint_zero - 1", value=uint_max)
        self.expect_var_path("4294967295 + 1", value="4294967296")
        self.expect_var_path("ll_max + 1", value=ll_min)
        self.expect_var_path("ll_min - 1", value=ll_max)
        self.expect_var_path("ull_max + 1", value="0")
        self.expect_var_path("ull_zero - 1", value=ull_max)
        self.expect_var_path("9223372036854775807 + 1", value="-9223372036854775808")
        self.expect_var_path("18446744073709551615ULL + 1", value="0")

        # self.expect_var_path("-20 / 1U", value="4294967276")
        # self.expect_var_path("-20LL / 1U", value="-20")
        # self.expect_var_path("-20LL / 1ULL", value="18446744073709551596")

        # Check references and typedefs
        self.expect_var_path("r + 1", value="3")
        self.expect_var_path("r - 1l", value="1")
        # self.expect_var_path("r * 2u", value="4")
        # self.expect_var_path("r / 2ull", value="1")
        self.expect_var_path("my_r + 1", value="3")
        self.expect_var_path("my_r - 1", value="1")
        # self.expect_var_path("my_r * 2", value="4")
        # self.expect_var_path("my_r / 2", value="1")
        self.expect_var_path("r + my_r", value="4")
        self.expect_var_path("r - my_r", value="0")
        # self.expect_var_path("r * my_r", value="4")
        # self.expect_var_path("r / my_r", value="1")

        # Division by zero
        # self.expect_var_path("1 / 0"), IsError("Division by zero detected.")
        # self.expect_var_path("1 / uint_zero"), IsError("Division by zero detected.")
        # self.expect_var_path("1ll / 0 + 1"), IsError("Division by zero detected.")
        # self.expect_var_path("1 % 0"), IsError("Division by zero detected.")
        # self.expect_var_path("1 % uint_zero"), IsError("Division by zero detected.")
        # self.expect_var_path("1 % uint_zero + 1"), IsError("Division by zero detected.")
