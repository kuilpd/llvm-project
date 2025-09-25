"""
Test DIL bitwise operators.
"""

import lldb
from lldbsuite.test.lldbtest import *
from lldbsuite.test.decorators import *
from lldbsuite.test import lldbutil


class TestFrameVarDILBitwise(TestBase):
    NO_DEBUG_INFO_TESTCASE = True

    def test_bitwise(self):
        self.build()
        (target, process, thread, bkpt) = lldbutil.run_to_source_breakpoint(
            self, "Set a breakpoint here", lldb.SBFileSpec("main.cpp")
        )

        self.runCmd("settings set target.experimental.use-DIL true")

        self.expect_var_path("~(-1)", value="0")
        self.expect_var_path("~~0", value="0")
        self.expect_var_path("~0", value="-1")
        self.expect_var_path("~1", value="-2")
        self.expect_var_path("~0LL", value="-1")
        self.expect_var_path("~1LL", value="-2")
        self.expect_var_path("~true", value="-2")
        self.expect_var_path("~false", value="-1")
        self.expect_var_path("~var_true", value="-2")
        self.expect_var_path("~var_false", value="-1")
        self.expect_var_path("~ull_max", value="0")

        self.expect_var_path("(1 << 5)", value="32")
        self.expect_var_path("(32 >> 2)", value="8")
        self.expect_var_path("(-1 >> 10)", value="-1")
        self.expect_var_path("(-100 >> 5)", value="-4")
        self.expect_var_path("(-3 << 6)", value="-192")
        self.expect_var_path("(2000000000U << 1)", value="4000000000")
        self.expect_var_path("(-1 >> 1U)", value="-1")
        self.expect_var_path("(0xFFFFFFFFu>>31)", value="1")
        self.expect_var_path("(char)1 << 16", value="65536")
        self.expect_var_path("(signed char)-123 >> 8", value="-1")

        self.expect_var_path("0b1011 & 0xFF", value="11")
        self.expect_var_path("0b1011 & mask_ff", value="11")
        self.expect_var_path("0b1011 & 0b0111", value="3")
        self.expect_var_path("0b1011 | 0b0111", value="15")
        self.expect_var_path("-0b1011 | 0xFF", value="-1")
        self.expect_var_path("-0b1011 | 0xFFu", value="4294967295")
        self.expect_var_path("0b1011 ^ 0b0111", value="12")
        self.expect_var_path("~0b1011", value="-12")

        self.expect(
            "frame var -- '~s'",
            error=True,
            substrs=["invalid argument type 'S' to unary expression"],
        )
        self.expect(
            "frame var -- '~p'",
            error=True,
            substrs=["invalid argument type 'const char *' to unary expression"],
        )
        self.expect(
            "frame var -- 's ^ 1'",
            error=True,
            substrs=["invalid operands to binary expression ('S' and 'int')"],
        )
