#! /bin/sh
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
#
# Tests for the h5mkgrp tool
#
# Quincey Koziol (koziol@hdfgroup.org)
# Tuesday, February 13, 2007
#

TESTNAME=h5mkgrp
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5MKGRP=h5mkgrp             # The tool name
H5MKGRP_BIN=`pwd`/$H5MKGRP  # The path of the tool binary
H5LS=h5ls                   # The h5ls tool name 
H5LS_ARGS=-vr               # Arguments to the h5ls tool
H5LS_BIN=`pwd`/../h5ls/$H5LS # The path of the h5ls tool binary

nerrors=0
verbose=yes

INDIR=$srcdir/../testfiles
OUTDIR=../testfiles
CMP='cmp -s'
DIFF='diff -c'

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
    srcdir=.
fi
test -d $OUTDIR || mkdir $OUTDIR

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING() 
{
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
VERIFY_H5LS() 
{
    SPACES="                                                               "
    echo "Verifying h5ls file structure $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test and print PASS or *FAIL*. If h5mkgrp can complete
# with exit status 0, consider it pass. If a test fails then increment
# the `nerrors' global variable.
# Assumed arguments:
# $* arguments for h5mkgrp.

TOOLTEST() 
{
    TESTING $H5MKGRP $@
    (
    echo "#############################"
    echo " output for '$H5MKGRP $@'"
    echo "#############################"
    $RUNSERIAL $H5MKGRP_BIN $@
    ) > output.out
    RET=$?
    if [ $RET != 0 ]; then
        echo "*FAILED*"
        echo "failed result is:"
        cat output.out
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"

        # Clean up output file
        if test -z "$HDF5_NOCLEANUP"; then
           rm -f output.out
        fi
    fi
}

# Call the h5ls tool to verify the correct output data in the destination file
#
H5LSTEST() 
{
    expect="$INDIR/`basename $1 .h5`.ls"
    actual="$OUTDIR/`basename $1 .h5`.out"

    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    VERIFY_H5LS  $@
    (
      echo "#############################"
      echo "Expected output for '$H5LS $@'" 
      echo "#############################"
      $RUNSERIAL $H5LS_BIN $H5LS_ARGS $@
    ) 2>&1 |sed 's/Modified:.*/Modified:  XXXX-XX-XX XX:XX:XX XXX/' >$actual


   if [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
    echo " CREATED"
    cp $actual $expect
   elif $CMP $expect $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result (*.ls) differs from actual result (*.out)"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
   fi

   # Clean up output file
   if test -z "$HDF5_NOCLEANUP"; then
      rm -f $actual $actual_err
   fi
}

# Single run of tool
#
# Assumed arguments:
# $1 is test file name
# $2 is h5mkgrp options
# $* are groups to create
RUNTEST() 
{
    FILEOUT=$OUTDIR/$1
    shift
    H5MKGRP_ARGS=$1
    shift

    # Remove any output file left over from previous test run
    rm -f $FILEOUT

    # Run test
    TOOLTEST $H5MKGRP_ARGS $FILEOUT $@

    # Verify that the file created above is correct
    H5LSTEST $FILEOUT

    # Remove output file created, if the "no cleanup" environment variable is
    #   not defined
echo "FILEOUT=" $FILEOUT
    if test -z "$HDF5_NOCLEANUP"; then
        rm -f $FILEOUT
    fi
}

# Single run of tool
#
# Assumed arguments:
# $1 is test expected output file
# $2 is h5mkgrp options
# $* are groups to create
CMPTEST() 
{
    FILEOUT=$OUTDIR/$1
    expect="$srcdir/testfiles/`basename $1 .h5`.txt"
    actual="$OUTDIR/`basename $1 .h5`.out"
    actual_err="$OUTDIR/`basename $1 .h5`.err"
    shift

    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    TESTING $H5MKGRP $@
    (
    $RUNSERIAL $H5MKGRP_BIN $@
    ) >$actual 2>$actual_err
    cat $actual_err >> $actual
    
   if [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
    echo " CREATED"
    cp $actual $expect
   elif $CMP $expect $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result (*.txt) differs from actual result (*.out)"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
   fi

   # Clean up output file
   if test -z "$HDF5_NOCLEANUP"; then
      rm -f $actual $actual_err
   fi
}

##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################

# Check that help & version is displayed properly
CMPTEST h5mkgrp_help.h5 "-h"
RUNTEST h5mkgrp_version.h5 "-V"

# Create single group at root level
RUNTEST h5mkgrp_single.h5 " " single
RUNTEST h5mkgrp_single.h5 "-v" single
RUNTEST h5mkgrp_single.h5 "-p" single
RUNTEST h5mkgrp_single_latest.h5 "-l" latest

# Create several groups at root level
RUNTEST h5mkgrp_several.h5 " " one two
RUNTEST h5mkgrp_several.h5 "-v" one two
RUNTEST h5mkgrp_several.h5 "-p" one two
RUNTEST h5mkgrp_several_latest.h5 "-l" one two

# Create various nested groups 
RUNTEST h5mkgrp_nested.h5 "-p" /one/two
RUNTEST h5mkgrp_nested_latest.h5 "-lp" /one/two
RUNTEST h5mkgrp_nested_mult.h5 "-p" /one/two /three/four
RUNTEST h5mkgrp_nested_mult_latest.h5 "-lp" /one/two /three/four


if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
