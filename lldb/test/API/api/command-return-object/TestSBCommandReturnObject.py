"""Test the lldb public C++ api for returning SBCommandReturnObject."""

import subprocess

from lldbsuite.test.decorators import *
from lldbsuite.test.lldbtest import *
from lldbsuite.test import lldbutil


class TestSBCommandReturnObject(TestBase):
    NO_DEBUG_INFO_TESTCASE = True

    @skipIfNoSBHeaders
    @expectedFailureAll(
        oslist=["windows"], archs=["i[3-6]86", "x86_64"], bugnumber="llvm.org/pr43570"
    )
    @skipIfHostIncompatibleWithTarget
    def test_sb_command_return_object(self):
        self.driver_exe = self.getBuildArtifact("command-return-object")
        self.buildDriver("main.cpp", self.driver_exe)
        self.addTearDownHook(lambda: os.remove(self.driver_exe))

        # check_call will raise a CalledProcessError if the executable doesn't
        # return exit code 0 to indicate success.  We can let this exception go
        # - the test harness will recognize it as a test failure.
        subprocess.check_call([self.driver_exe, self.driver_exe])

    def test_get_command(self):
        res = lldb.SBCommandReturnObject()
        self.assertEqual(res.GetCommand(), "")

        ci = self.dbg.GetCommandInterpreter()
        ci.HandleCommand("help help", res)
        self.assertTrue(res.Succeeded())
        self.assertEqual(res.GetCommand(), "help help")

        value_list = res.GetValues(lldb.eNoDynamicValues)
        self.assertEqual(value_list.GetSize(), 0)

    def test_get_value(self):
        res = lldb.SBCommandReturnObject()
        ci = self.dbg.GetCommandInterpreter()
        ci.HandleCommand("p 1 + 1", res)
        self.assertTrue(res.Succeeded())

        value_list = res.GetValues(lldb.eNoDynamicValues)
        self.assertEqual(value_list.GetSize(), 1)
        self.assertEqual(value_list.GetValueAtIndex(0).GetValue(), "2")
