"""
Test DIL pointers.
"""

import lldb
from lldbsuite.test.lldbtest import *
from lldbsuite.test.decorators import *
from lldbsuite.test import lldbutil


class TestFrameVarDILPointers(TestBase):
    NO_DEBUG_INFO_TESTCASE = True

    def test_pointers(self):
        self.build()
        lldbutil.run_to_source_breakpoint(
            self, "Set a breakpoint here", lldb.SBFileSpec("main.cpp")
        )

        self.runCmd("settings set target.experimental.use-DIL true")
        self.expect_var_path("*p_int0", value="0")
        self.expect_var_path("*cp_int5", value="5")
        self.expect_var_path("*rcp_int0", type="const int *")
        self.expect_var_path("*offset_p",  value="5")
        self.expect_var_path("*offset_pref", type="int *")
        self.expect_var_path("**pp_int0", value="0")
        self.expect_var_path("&**pp_int0", type="int *")
        self.expect_var_path("*td_int_ptr0", value="0")
        self.expect_var_path("*array", value="0")
        self.expect(
            "frame var '&*p_null'",
            error=True,
            substrs=["doesn't have a valid address"],
        )
        self.expect(
            "frame var '&*p_void'",
            error=True,
            substrs=["dereference failed: (void *) p_void"],
        )
