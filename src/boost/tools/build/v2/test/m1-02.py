#!/usr/bin/python

# Copyright 2002 Vladimir Prus 
# Distributed under the Boost Software License, Version 1.0. 
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt) 

# Tests that 'make' accepts targets from other directories and that build
# requests for those targets can be overriden.

import BoostBuild

t = BoostBuild.Tester()

t.set_tree("test1")

t.run_build_system("-sTOOLSET=yfc")

t.expect_addition("bin/a.obj/yfc/debug/runtime-link-dynamic/a.obj")
t.expect_addition("auxillary/bin/b.obj/yfc/debug/runtime-link-dynamic/optimization-space/b.obj")
t.expect_addition("bin/a/yfc/debug/runtime-link-dynamic/a")
t.expect_nothing_more()

t.fail(t.read("bin/a.obj/yfc/debug/runtime-link-dynamic/a.obj") !=\
"""
<optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
a.cpp
""")

t.fail(t.read("auxillary/bin/b.obj/yfc/debug/runtime-link-dynamic/b.obj") !=\
"""
<optimization>space <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
b.cpp
""")

t.fail(t.read("bin/a/yfc/debug/runtime-link-dynamic/a") !=\
"""
<optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
<optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
a.cpp
<optimization>space <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
b.cpp
""")

# Check that we have vanilla target names available in subdirs.
t.touch("auxillary/b.cpp")
t.run_build_system("-sTOOLSET b.obj", subdir="auxillary")
t.expect_touch("auxillary/bin/b.obj/yfc/debug/runtime-link-dynamic/optimization-space/b.obj")
t.expect_no_modification("bin/a.obj/yfc/debug/runtime-link-dynamic/a.obj")
t.expect_no_modification("bin/a/yfc/debug/runtime-link-dynamic/a")


# Check that we can not request link-incompatible property for source target.
t.write('jamfile.jam', t.read('jamfile2.jam'))
stdout="""Error: subvariant of target ./a with properties
    <optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
requests link-incompatible property
    <rtti>off
for source @auxillary/b.obj
"""
t.run_build_system("-sTOOLSET=yfc", stdout=stdout)


# Check that if we request link-compatible property then requirement for the
# source target will override it, with a warning. This is similar to the way
# build requests are satisfied (see the first test).
#
# CONSIDER: should be print the main target which requests this one (and
# modifies requirements)?
t.write('jamfile.jam', t.read('jamfile3.jam'))
t.write('auxillary/jamfile.jam', t.read('auxillary/jamfile3.jam'))
stdout="""Warning: cannot exactly satisfy request for auxillary/b.obj with properties
    <optimization>space <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
Using
    <optimization>speed <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
instead.
"""
t.run_build_system("-sTOOLSET=yfc", stdout=stdout)


# Check for link-incompatible properties.
t.write('jamfile.jam', t.read('jamfile4.jam'))
t.write('auxillary/jamfile.jam', t.read('auxillary/jamfile4.jam'))
stdout="""Warning: cannot satisfy request for auxillary/b.obj with properties
    <optimization>space <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
Nothing will be built.
"""
t.run_build_system("-sTOOLSET=yfc", stdout=stdout)


t.pass_test()
