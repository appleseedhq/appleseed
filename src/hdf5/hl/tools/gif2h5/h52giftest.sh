#!/bin/sh
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
# HDF Utilities Test script


TESTFILE1="$srcdir/testfiles/h52giftst.h5"
TESTFILE2="$srcdir/testfiles/image1.gif"

# initialize errors variable
errors=0

TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}



TOOLTEST1()
{
 $RUNSERIAL ./h52gif $*
 
 RET=$?
 if [ $RET != 0 ] ; then
  echo "*FAILED*"
  errors="` expr $errors + 1 `";
 else
  echo " PASSED"
 fi

}

TOOLTEST2()
{
 $RUNSERIAL ./gif2h5 $*
 
 RET=$?
 if [ $RET != 0 ] ; then
  echo "*FAILED*"
  errors="` expr $errors + 1 `";
 else
  echo " PASSED"
 fi
}



TESTING "./h52gif h52giftst.h5 image1.gif -i image" 
TOOLTEST1 $TESTFILE1 image1.gif -i image
TESTING "./gif2h5 image1.gif image1.h5"
TOOLTEST2 $TESTFILE2 image1.h5


if test $errors -eq 0 ; then
    echo "All h52gif tests passed."
    exit 0
else
    echo "h52gif tests failed with $errors errors."
    exit 1
fi
