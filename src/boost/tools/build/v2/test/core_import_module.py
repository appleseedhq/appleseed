#!/usr/bin/python

# Copyright 2003 Vladimir Prus
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

import BoostBuild

t = BoostBuild.Tester(pass_toolset=0)

t.write("code", """
module a
{
    rule r1 ( )
    {
        ECHO R1 ;
    }
}
module a2
{
    rule r2 ( )
    {
        ECHO R2 ;
    }
}
IMPORT a2 : r2 : : a2.r2 ;

module b
{
    IMPORT_MODULE a : b ;
    rule test
    {
        # Call rule visible via IMPORT_MODULE
        a.r1 ;
        # Call rule in global scope
        a2.r2 ;
    }
}

IMPORT b : test : : test ;
test ;

module c
{
    rule test
    {
        ECHO CTEST ;
    }
}

IMPORT_MODULE c : ;
c.test ;

actions do-nothing { }
do-nothing all ;
""")

t.run_build_system("-fcode", stdout="""R1
R2
CTEST
""")

t.cleanup()
