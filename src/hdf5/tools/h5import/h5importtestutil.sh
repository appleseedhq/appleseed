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
# Usage: h5importtestutil.sh [machine-type]

TESTNAME=h5import
EXIT_SUCCESS=0
EXIT_FAILURE=1

# initialize errors variable
nerrors=0

TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

TOOLTEST()
{
err=0
$RUNSERIAL ./h5import $*
$RUNSERIAL ../h5dump/h5dump $5 >log2

cd tmp_testfiles
$RUNSERIAL ../../h5dump/h5dump $5 >log1
cd ..

cmp -s tmp_testfiles/log1 log2 || err=1
rm -f log2 tmp_testfiles/log1
if [ $err -eq 1 ]; then
nerrors="` expr $nerrors + 1 `";
  echo "*FAILED*"
else
  echo " PASSED"
fi
}

echo "" 
echo "=============================="
echo "H5IMPORT tests started"
echo "=============================="

if [ -f h5import -a -f h5importtest ]; then
#echo "** Testing h5import  ***"

rm -f  output.h5 log1 tx* b* *.dat

mkdir tmp_testfiles
cp $srcdir/testfiles/*.h5 tmp_testfiles/

$RUNSERIAL ./h5importtest

TESTING "ASCII I32 rank 3 - Output BE " ;
TOOLTEST $srcdir/testfiles/in32.txt -c $srcdir/testfiles/textin32.conf -o test1.h5

TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
TOOLTEST $srcdir/testfiles/in16.txt -c $srcdir/testfiles/textin16.conf -o test2.h5

TESTING "ASCII I8 - rank 3 - Output I8 LE-Chunked+Extended+Compressed " 
TOOLTEST $srcdir/testfiles/in16.txt -c $srcdir/testfiles/textin8.conf  -o test3.h5

TESTING "ASCII UI32 - rank 3 - Output BE" 
TOOLTEST $srcdir/testfiles/in1.txt -c $srcdir/testfiles/textuin32.conf -o test4.h5

TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
TOOLTEST $srcdir/testfiles/in1.txt -c $srcdir/testfiles/textuin16.conf -o test5.h5

TESTING "ASCII F32 - rank 3 - Output LE " 
TOOLTEST $srcdir/testfiles/fp1.txt -c $srcdir/testfiles/textfp32.conf -o test6.h5

TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
TOOLTEST $srcdir/testfiles/fp2.txt -c $srcdir/testfiles/textfp64.conf -o test7.h5

TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
TOOLTEST binfp64.bin -c $srcdir/testfiles/binfp64.conf -o test8.h5

TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
TOOLTEST binin16.bin -c $srcdir/testfiles/binin16.conf -o test9.h5

TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
TOOLTEST binin8.bin -c $srcdir/testfiles/binin8.conf  -o test10.h5

TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
TOOLTEST binin32.bin -c $srcdir/testfiles/binin32.conf -o test11.h5

TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
TOOLTEST binuin16.bin -c $srcdir/testfiles/binuin16.conf -o test12.h5

TESTING "BINARY UI32 - rank 3 - Output LE + CHUNKED " 
TOOLTEST binuin32.bin -c $srcdir/testfiles/binuin32.conf -o test13.h5

TESTING "STR" 
TOOLTEST $srcdir/testfiles/str.txt -c $srcdir/testfiles/textstr.conf -o test14.h5

TESTING "ASCII F64 - rank 1 - INPUT-CLASS TEXTFPE " 
TOOLTEST $srcdir/testfiles/in64.txt -c $srcdir/testfiles/textpfe.conf -o test15.h5

rm -f  txtin32.txt txtin16.txt *.bin *.h5
rm -rf tmp_testfiles
else
	echo "** h5import or h5importtest not available ***"
	nerrors="` expr $nerrors + 1 `";
fi

#
# Check errors result
if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
