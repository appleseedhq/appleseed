@REM Copyright by The HDF Group.
@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from help@hdfgroup.org.


@ECHO OFF
REM This batch file is used to install HDF5 C++ examples'
REM executable files.
REM By Xuan Bai
REM Created on: 10/20/2004
REM Last Modified: 10/27/2004

cd c++\examples

mkdir cppexamplesREL
mkdir cppexamplesRELDLL

cd chunkstest
copy release\chunkstest.exe ..\cppexamplesREL\
cd ..

cd chunkstestdll
copy release\chunkstestdll.exe ..\cppexamplesRELDLL\
cd ..

cd compoundtest
copy release\compoundtest.exe ..\cppexamplesREL\
cd ..

cd compoundtestdll
copy release\compoundtestdll.exe ..\cppexamplesRELDLL\
cd ..

cd createtest
copy release\createtest.exe ..\cppexamplesREL\
cd ..

cd createtestdll
copy release\createtestdll.exe ..\cppexamplesRELDLL\
cd ..

cd extend_dstest
copy release\extend_dstest.exe ..\cppexamplesREL\
cd ..

cd extend_dstestdll
copy release\extend_dstestdll.exe ..\cppexamplesRELDLL\
cd ..

cd h5grouptest
copy release\h5grouptest.exe ..\cppexamplesREL\
cd ..

cd h5grouptestdll
copy release\h5grouptestdll.exe ..\cppexamplesRELDLL\
cd ..

cd readdatatest
copy release\readdatatest.exe ..\cppexamplesREL\
cd ..

cd readdatatestdll
copy release\readdatatestdll.exe ..\cppexamplesRELDLL\
cd ..

cd writedatatest
copy release\writedatatest.exe ..\cppexamplesREL\
cd ..

cd writedatatestdll
copy release\writedatatestdll.exe ..\cppexamplesRELDLL\
cd ..

cd ..\..
