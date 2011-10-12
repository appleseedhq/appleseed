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
# Tests for the h5repart tool

TESTNAME=h5repart
EXIT_SUCCESS=0
EXIT_FAILURE=1

REPART=h5repart             # The tool name
REPART_BIN=`pwd`/$REPART    # The path of the tool binary

REPARTED_FAM=repart_test                # The test name
REPARTED_FAM_BIN=`pwd`/$REPARTED_FAM    # The path of the test binary

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d ../testfiles || mkdir ../testfiles

actual_dir=`pwd`/../testfiles

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable.
#
TOOLTEST() {
   # Run tool test.
   TESTING $REPART $@
   (
#      echo
      cd $srcdir/../testfiles
      $RUNSERIAL $REPART_BIN $@
   )

   if test $? -eq 0; then
       echo " PASSED"
   else
       echo " FAILED"
       nerrors=`expr $nerrors + 1`
   fi
}

OUTPUTTEST() {
   # Run test program.
   TESTING $REPARTED_FAM $@
   (
      cd $actual_dir
      $RUNSERIAL $REPARTED_FAM_BIN $@
   )

   if test $? -eq 0; then
       echo " PASSED"
   else
       echo " FAILED"
       nerrors=`expr $nerrors + 1`
   fi
}

# Print a "SKIP" message
SKIP() {
	 TESTING $REPART $@
	  echo  " -SKIP-"
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# repartition family member size to 20,000 bytes.
TOOLTEST -m 20000 family_file%05d.h5 $actual_dir/fst_family%05d.h5
# repartition family member size to 5 KB.
TOOLTEST -m 5k family_file%05d.h5 $actual_dir/scd_family%05d.h5
# convert family file to sec2 file of 20,000 bytes
TOOLTEST -m 20000 -family_to_sec2 family_file%05d.h5 $actual_dir/family_to_sec2.h5

# test the output files repartitioned above.
OUTPUTTEST
echo

# Clean up output file
if test -z "$HDF5_NOCLEANUP"; then
    cd $actual_dir
    rm -f fst_family*.h5 scd_family*.h5 family_to_sec2.h5
fi

if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
