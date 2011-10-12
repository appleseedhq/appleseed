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

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

TESTNAME=ph5diff
EXIT_SUCCESS=0
EXIT_FAILURE=1

TOOL=${srcdir}/testh5diff.sh

nerrors=0

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test.  If a test fails then increment the `nerrors' global variable.
#
TOOLTEST() {
    # Run test.
    echo $TOOL "$@"
    /bin/sh $TOOL "$@"

    # Check if the command failed and increment nerrors if so.
    if test $? -ne 0 ; then
        nerrors="`expr $nerrors + 1`"
    fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# Invoke the regular h5diff testing script, with the -p parameter to indicate
#       that it should run the parallel version of the tests
TOOLTEST -p

# no need to print any message since this is just a shell to invoke
# testh5diff.sh which has already printed the result.  Just exit.
if test $nerrors -eq 0 ; then
    exit $EXIT_SUCCESS
else
    exit $EXIT_FAILURE
fi
