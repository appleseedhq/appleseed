#!/usr/bin/python

# Copyright 2008 Jurko Gospodnetic
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

# Tests Boost Jam & Boost Build's source & target file name handling. Originally
# added as a regression test for a bug causing versions included in the Boost
# library 1.35 release (and earlier) not handling Windows short file names
# correctly. Also tests handling target file names containing spaces.

# Implementation notes:
#
#   We expect Boost Jam to automatically update the 'all' target when no target
# has been explicitly specified on the command line.
#
#   Windows short & long file names can not be matched until the file in
# question actually exists so we need to create the file before running Boost
# Jam.
#
#   Currently Windows short file names are hardcoded but in case this proves
# insufficient we should use the GetShortPathName() Windows API to get the
# correct short file for a given long file name.
#                                                    (30.04.2008.) (Jurko)

import BoostBuild
import os
import time


################################################################################
#
# prepare_file()
# --------------
#
################################################################################

def prepare_file(tester, target_name, age_in_seconds=0):
    """Prepares a new file with the given name, optionally setting its last
    access and modification timestamps to the given number of seconds in the
    history.
    """
    tester.write( target_name, "Original file content." )
    if ( ( not age_in_seconds is None ) and ( age_in_seconds != 0 ) ):
        t = time.time() - age_in_seconds
        os.utime( tester.native_file_name( target_name ), ( t, t ) )


################################################################################
#
# test_simple_file_names()
# ------------------------
#
################################################################################

def test_simple_file_names():
    """Runs simple file name handling that is not expected to fail anywhere. Not
    really needed for regular testing but the test is really fast and its
    content may be used for comparing to other more complex test functions in
    this module which test the same system but with some potentially more
    problematic file names.
    """
    t = BoostBuild.Tester(pass_toolset=0)

    prepare_file(t, "source.txt"              )
    prepare_file(t, "target.txt"         , 120)
    prepare_file(t, "target_noupdate.txt", 240)

    t.write("testScript.jam", """
actions create-file
{
    echo "Modified file content ($(1:E=""))."> "$(1:E="")"
}

DEPENDS all : standaloneTarget.txt ;
create-file standaloneTarget.txt : all ;

DEPENDS all : target.txt ;
DEPENDS target.txt : source.txt ;
create-file target.txt ;

NOUPDATE target_noupdate.txt ;
DEPENDS all : target_noupdate.txt ;
DEPENDS target_noupdate.txt : source.txt ;
create-file target_noupdate.txt ;

create-file shouldNotBeCreated1 ;
create-file shouldNotBeCreated2 ;
create-file shouldNotBeCreated3 ;
""")

    t.run_build_system("-ftestScript.jam")
    t.expect_addition("standaloneTarget.txt")
    t.expect_modification("target.txt")
    t.expect_nothing_more()

    t.cleanup()


################################################################################
#
# test_short_file_name_with_action()
# ----------------------------------
#
################################################################################

def test_short_file_name_with_action():
    """Tests how Boost Jam handles the case when a Windows short file name is
    passed to a Boost Jam action.
    """
    if ( not BoostBuild.windows ):
        return

    t = BoostBuild.Tester(pass_toolset=0)

    long_file_name1 = "1__target that should be rebuilt.txt"
    long_file_name2 = "2__target that should be rebuilt.txt"
    short_file_name2 = "2__tar~1.txt"

    prepare_file(t, long_file_name1)
    prepare_file(t, long_file_name2)

    t.write("testScript.jam", """
actions create-file
{
    echo Modified file content ($(1:E="")).> "$(1:E="")"
}

ALWAYS "%(long_file_name1)s" ;
DEPENDS all : "%(long_file_name1)s" ;
create-file "%(long_file_name1)s" ;

ALWAYS "%(long_file_name2)s" ;
DEPENDS all : "%(long_file_name2)s" ;
create-file "%(short_file_name2)s" ;
""" % {'long_file_name1': long_file_name1,
    'long_file_name2'   : long_file_name2,
    'short_file_name2'  : short_file_name2})

    t.run_build_system("-ftestScript.jam")
    t.expect_modification(long_file_name1)
    t.expect_modification(long_file_name2)
    t.expect_nothing_more()

    t.cleanup()


################################################################################
#
# test_short_file_name_with_ALWAYS()
# ----------------------------------
#
################################################################################

def test_short_file_name_with_ALWAYS():
    """Tests how Boost Jam handles the case when a Windows short file name is
    passed to the builtin ALWAYS rule.
    """
    if ( not BoostBuild.windows ):
        return

    t = BoostBuild.Tester(pass_toolset=0)

    long_file_name1 = "1__target that should be rebuilt.txt"
    long_file_name2 = "2__target that should be rebuilt.txt"
    short_file_name2 = "2__tar~1.txt"

    prepare_file(t, long_file_name1)
    prepare_file(t, long_file_name2)

    t.write("testScript.jam", """
actions create-file
{
    echo Modified file content ($(1:E="")).> "$(1:E="")"
}

ALWAYS "%(long_file_name1)s" ;
DEPENDS all : "%(long_file_name1)s" ;
create-file "%(long_file_name1)s" ;

ALWAYS "%(short_file_name2)s" ;
DEPENDS all : "%(long_file_name2)s" ;
create-file "%(long_file_name2)s" ;
""" % {'long_file_name1': long_file_name1,
    'long_file_name2'   : long_file_name2,
    'short_file_name2'  : short_file_name2})

    t.run_build_system("-ftestScript.jam")
    t.expect_modification(long_file_name1)
    t.expect_modification(long_file_name2)
    t.expect_nothing_more()

    t.cleanup()


################################################################################
#
# test_short_file_name_with_NOUPDATE()
# ------------------------------------
#
################################################################################

def test_short_file_name_with_NOUPDATE():
    """Tests how Boost Jam handles the case when a Windows short file name is
    passed to the builtin NOUPDATE rule.
    """
    if ( not BoostBuild.windows ):
        return

    t = BoostBuild.Tester(pass_toolset=0)

    long_file_name1 = "1__target that should be rebuilt.txt"
    long_file_name2 = "2__target that should not be rebuilt.txt"
    short_file_name2 = "2__tar~1.txt"

    prepare_file(t, "source.txt"      )
    prepare_file(t, long_file_name1, 120)
    prepare_file(t, long_file_name2, 120)

    t.write("testScript.jam", """
actions create-file
{
    echo Modified file content ($(1:E="")).> "$(1:E="")"
}

DEPENDS all : "%(long_file_name1)s" ;
DEPENDS "%(long_file_name1)s" : source.txt ;
create-file "%(long_file_name1)s" ;

NOUPDATE "%(short_file_name2)s" ;
DEPENDS all : "%(long_file_name2)s" ;
DEPENDS "%(long_file_name2)s" : source.txt ;
create-file "%(long_file_name2)s" ;
""" % {'long_file_name1': long_file_name1,
    'long_file_name2'   : long_file_name2,
    'short_file_name2'  : short_file_name2})

    t.run_build_system("-ftestScript.jam")
    t.expect_modification(long_file_name1)
    t.expect_nothing_more()

    t.cleanup()


################################################################################
#
# main()
# ------
#
################################################################################

test_simple_file_names()
test_short_file_name_with_action()
test_short_file_name_with_ALWAYS()
test_short_file_name_with_NOUPDATE()
