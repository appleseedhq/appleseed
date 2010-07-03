#!/usr/bin/python

import BoostBuild

t = BoostBuild.Tester()


# First check some startup.
t.set_tree("direct-request-test")
t.run_build_system(extra_args="define=MACROS")
t.expect_addition("bin/$toolset/debug/" 
                  * (BoostBuild.List("a.obj b.obj b.dll a.exe")))


# When building a debug version, the 'define' still applies.
t.rm("bin")
t.run_build_system(extra_args="debug define=MACROS")
t.expect_addition("bin/$toolset/debug/" 
                  * (BoostBuild.List("a.obj b.obj b.dll a.exe")))


# When building release version, the 'define' should not apply: we will have
# direct build request 'release <define>MACROS' and a real build property
# 'debug'.
t.copy("jamfile2.jam", "jamfile.jam")
t.copy("b_inverse.cpp", "b.cpp")
t.rm("bin")
t.run_build_system(extra_args="release define=MACROS")


# Regression test: direct build request was not working when there was more than
# one level of 'build-project'.
t.rm(".")
t.write('jamroot.jam', '')
t.write('jamfile.jam', 'build-project a ;')
t.write('a/jamfile.jam', 'build-project b ;')
t.write('a/b/jamfile.jam', '')
t.run_build_system("release")

t.cleanup()
