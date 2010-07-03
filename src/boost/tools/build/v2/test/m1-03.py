#!/usr/bin/python

# Copyright 2002 Vladimir Prus
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

# Tests that we can use objects from other projects, i.e. with other project
# root. Test also that we can refer to those target using project-id.

import BoostBuild

t = BoostBuild.Tester()

t.set_tree("test1")

t.run_build_system("-sTOOLSET=yfc", subdir="p1")

t.expect_addition("p1/bin/a.obj/yfc/debug/runtime-link-dynamic/a.obj")
t.expect_addition("p1/auxillary/bin/b.obj/yfc/debug/runtime-link-dynamic/optimization-space/b.obj")
t.expect_addition("p2/bin/c.obj/yfc/debug/runtime-link-dynamic/c.obj")
t.expect_addition("bin/a/yfc/debug/runtime-link-dynamic/a")
t.expect_nothing_more()

t.fail(t.read("p1/bin/a.obj/yfc/debug/runtime-link-dynamic/a.obj") !=\
"""
<optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
a.cpp
""")

t.fail(t.read("p1/auxillary/bin/b.obj/yfc/debug/runtime-link-dynamic/b.obj") !=\
"""
<optimization>space <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
b.cpp
""")

t.fail(t.read("p2/bin/c.obj/yfc/debug/runtime-link-dynamic/c.obj") !=\
"""
<include>everything <optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
a.cpp
""")

t.fail(t.read("bin/a/yfc/debug/runtime-link-dynamic/a") !=\
"""
<optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
<optimization>off <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
a.cpp
<optimization>space <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
b.cpp
<include>everything <optimization>space <rtti>on <runtime-link>dynamic <toolset>yfc <variant>debug
c.cpp
""")

t.expect_nothing_more()

# TODO: need to write test cases for referring to targets using project-id.

t.pass_test()
