#!/usr/bin/python

# Copyright 2002 Dave Abrahams 
# Distributed under the Boost Software License, Version 1.0. 
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt) 

import BoostBuild
import re

def match_re(actual, expected):
    return re.match(expected, actual, re.DOTALL) != None

t = BoostBuild.Tester(match=match_re)

t.set_tree('testing-primitives')

# We expect t5 and t7's output to be dumped to stdout.
t.run_build_system(stdout=r'''.*failing t5.*failing t7''')

t.expect_addition('t2.txt')
t.expect_addition('t3.txt')
t.expect_addition('t5.out')
t.expect_addition('t6.out')
t.expect_addition('t6.txt')
t.expect_addition('t7.out')
t.expect_addition('t7.txt')
t.expect_addition('t8.out')
t.expect_nothing_more()

t.cleanup()
