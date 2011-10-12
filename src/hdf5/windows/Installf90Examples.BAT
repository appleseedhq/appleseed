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
REM This batch file is used to install HDF5 Fortran examples'
REM executable files.
REM By Xuan Bai
REM Created on: 10/20/2004
REM Last Modified: 10/28/2004

cd fortran\examples

mkdir f90examplesREL
mkdir f90examplesRELDLL

cd attreexampletest
copy release\attreexampletest.exe ..\f90examplesREL\
cd ..

cd attreexampletestdll
copy release\attreexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd compoundtest
copy release\compoundtest.exe ..\f90examplesREL\
cd ..

cd compoundtestdll
copy release\compoundtestdll.exe ..\f90examplesRELDLL\
cd ..

cd dsetexampletest
copy release\dsetexampletest.exe ..\f90examplesREL\
cd ..

cd dsetexampletestdll
copy release\dsetexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd fileexampletest
copy release\fileexampletest.exe ..\f90examplesREL\
cd ..

cd fileexampletestdll
copy release\fileexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd groupexampletest
copy release\groupexampletest.exe ..\f90examplesREL\
cd ..

cd groupexampletestdll
copy release\groupexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd grpdsetexampletest
copy release\grpdsetexampletest.exe ..\f90examplesREL\
cd ..

cd grpdsetexampletestdll
copy release\grpdsetexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd grpittest
copy release\grpittest.exe ..\f90examplesREL\
cd ..

cd grpittestdll
copy release\grpittestdll.exe ..\f90examplesRELDLL\
cd ..

cd grpsexampletest
copy release\grpsexampletest.exe ..\f90examplesREL\
cd ..

cd grpsexampletestdll
copy release\grpsexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd hyperslabtest
copy release\hyperslabtest.exe ..\f90examplesREL\
cd ..

cd hyperslabtestdll
copy release\hyperslabtestdll.exe ..\f90examplesRELDLL\
cd ..

cd mountexampletest
copy release\mountexampletest.exe ..\f90examplesREL\
cd ..

cd mountexampletestdll
copy release\mountexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd refobjexampletest
copy release\refobjexampletest.exe ..\f90examplesREL\
cd ..

cd refobjexampletestdll
copy release\refobjexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd refregexampletest
copy release\refregexampletest.exe ..\f90examplesREL\
cd ..

cd refregexampletestdll
copy release\refregexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd rwdsetexampletest
copy release\rwdsetexampletest.exe ..\f90examplesREL\
cd ..

cd rwdsetexampletestdll
copy release\rwdsetexampletestdll.exe ..\f90examplesRELDLL\
cd ..

cd selecteletest
copy release\selecteletest.exe ..\f90examplesREL\
cd ..

cd selecteletestdll
copy release\selecteletestdll.exe ..\f90examplesRELDLL\
cd ..

cd ..\..