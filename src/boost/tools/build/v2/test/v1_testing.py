#!/usr/bin/python

# Copyright 2002 Dave Abrahams
# Copyright 2004 Vladimir Prus
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

import BoostBuild
import os
from string import strip
import re
import time

def match_re(actual,expected):
    return re.match(expected,actual,re.DOTALL) != None

t = BoostBuild.Tester(match = match_re, boost_build_path = os.path.join(os.getcwd(), ".."))
t.set_tree('v1_testing')

os.environ['TOOLS'] = 'gcc'
os.environ['NOARSCAN'] = '1'

# 1) No existing bin directories.  Both build and test ran fine. As
# expected, the residue files were a bit different: There was no
# path_test.success, and path_test.test contained the word "passed"
# instead of the path to the .cpp file.  I've haven't looked yet to
# see if the lack of the path is a problem for reporting, but
# hopefully the information is trivially available somewhere else.
t.run_build_system(arguments = 'test', status = 0)
t.expect_addition(
    ['bin/compile.test/gcc/debug/runtime-link-dynamic/compile.test'
     , 'bin/nocompile.test/gcc/debug/runtime-link-dynamic/nocompile.test'
     , 'bin/link.test/gcc/debug/runtime-link-dynamic/link.test'
     , 'bin/nolink.test/gcc/debug/runtime-link-dynamic/nolink.test'
     , 'bin/run.test/gcc/debug/runtime-link-dynamic/run.test'])


# 2) Missing source file for the library build. path_test.test was
# deleted, so the reporting programs would know that failure
# occurred. The stdout messages also indicated what had
# happened. Excellent!
t.rename('lib.cpp', 'lib.cpp.bak')
t.run_build_system(arguments = 'test', status = 1)
t.expect_removal(
    ['bin/link.test/gcc/debug/runtime-link-dynamic/link.test'
     , 'bin/nolink.test/gcc/debug/runtime-link-dynamic/nolink.test'
     , 'bin/run.test/gcc/debug/runtime-link-dynamic/run.test'])

# 3) Missing file restored. Worked fine; path_test.test was recreated,
# no other files were touched.
t.rename('lib.cpp.bak', 'lib.cpp')
t.run_build_system(arguments = 'test', status = 0)
t.expect_addition(
    [ 'bin/link.test/gcc/debug/runtime-link-dynamic/link.test'
     , 'bin/nolink.test/gcc/debug/runtime-link-dynamic/nolink.test'
     , 'bin/run.test/gcc/debug/runtime-link-dynamic/run.test'])
     # I didn't add a test for 'no other files were touched', because
     # it's a little complicated. There is an expect_nothing_more()
     # function, but we actually need to spell out a lot more than
     # what we currently have to do that.

# 4) Introduced error into one of the library files, causing a library build
# compile to fail. path_test.test was deleted, so the reporting programs
# would know that failure occurred. Excellent! This is the case that has
# caused regression testing to report the wrong results in the past, so it
# was good news to see it working correctly now. We probably should figure
# out some other test cases just to be sure it is working for full coverage.
t.rename('lib.cpp', 'lib.cpp.bak')
t.rename('lib-err.cpp', 'lib.cpp')
t.touch('lib.cpp')
t.run_build_system(arguments = 'test', status=1)
t.expect_removal(
    ['bin/link.test/gcc/debug/runtime-link-dynamic/link.test'
     , 'bin/nolink.test/gcc/debug/runtime-link-dynamic/nolink.test'
     , 'bin/run.test/gcc/debug/runtime-link-dynamic/run.test'])

# 5) Fixed the error in the library file.  The library build then worked, and
# path_test.exe was relinked, without first recompiling path_test.obj. Test was
# rerun. Exactly the right behavior!
t.rename('lib.cpp.bak', 'lib.cpp')
t.run_build_system(arguments = 'test', status = 0)
t.expect_addition(
    [ 'bin/link.test/gcc/debug/runtime-link-dynamic/link.test'
     , 'bin/nolink.test/gcc/debug/runtime-link-dynamic/nolink.test'
     , 'bin/run.test/gcc/debug/runtime-link-dynamic/run.test'])

t.cleanup()
print 'testing complete'
