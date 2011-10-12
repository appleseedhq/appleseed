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
# Test for external link with environment variable: HDF5_EXT_PREFIX

nerrors=0

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# test for external links with HDF5_EXT_PREFIX
echo "Testing external link with HDF5_EXT_PREFIX"
TEST_NAME=links_env 		# The test name
TEST_BIN=`pwd`/$TEST_NAME 	# The path of the test binary
ENVCMD="env HDF5_EXT_PREFIX=.:tmp" 	# The environment variable & value
#
# Run the test
$ENVCMD $TEST_BIN
exitcode=$?
if [ $exitcode -eq 0 ]; then
        echo "Test for HDF5_EXT_PREFIX PASSED"
    else
 	nerrors="`expr $nerrors + 1`"
	echo "***Error encountered for HDF5_EXT_PREFIX test***"
fi
exit $nerrors
