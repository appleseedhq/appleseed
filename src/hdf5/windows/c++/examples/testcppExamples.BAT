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
REM This batch file is used to test HDF5 C++ examples.
REM By Xuan Bai
REM Created on: 10/20/2004
REM Last Modified: 10/20/2004

if %1.==. GOTO WRONG
if "%1"=="/?" GOTO HELP

type nul > %1.txt
createtest%2\%1\createtest%2 >> %1.txt
readdatatest%2\%1\readdatatest%2  >> %1.txt
writedatatest%2\%1\writedatatest%2  >> %1.txt
compoundtest%2\%1\compoundtest%2  >> %1.txt
extend_dstest%2\%1\extend_dstest%2  >> %1.txt
chunkstest%2\%1\chunkstest%2  >> %1.txt
h5grouptest%2\%1\h5grouptest%2  >> %1.txt
fc %1.txt expected.out >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C++ examples tests passed.
) else (
   echo HDF5 C++ examples tests failed.
   more temp.txt
)
del temp.txt
GOTO END

:WRONG
echo The syntax of the command is incorrect.
echo.

:HELP
echo Tests HDF5 C++ examples.
echo.
echo testcppExamples [OPTION]
echo.
echo Please use one of the following options!
echo.
echo    testcppExamples release        test HDF5 C++ examples -- release version
echo    testcppExamples release dll    test HDF5 C++ examples -- release dll version
echo    testcppExamples debug          test HDF5 C++ examples -- debug version
echo    testcppExamples debug dll      test HDF5 C++ examples -- debug dll version
echo    testcppExamples /?             Help information
echo.

:END
