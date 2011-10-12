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
REM This batch file is used to test HDF5 C examples.
REM by Xuan Bai
REM Created: 09/09/2004
REM Last Modified: 10/16/2004

if %1.==. GOTO WRONG
if "%1"=="/?" GOTO HELP

type nul > %1.txt
attributetest%2\%1\attributetest%2 >> %1.txt
compoundtest%2\%1\compoundtest%2 >> %1.txt
extendwritetest%2\%1\extendwritetest%2 >> %1.txt
grouptest%2\%1\grouptest%2 >> %1.txt
intermgrouptest%2\%1\intermgrouptest%2 >> %1.txt
selectest%2\%1\selectest%2 >> %1.txt
writetest%2\%1\writetest%2 >> %1.txt
chunkread%2\%1\chunkread%2 >> %1.txt
readtest%2\%1\readtest%2 >> %1.txt
more /e +3 testExamples_exp_output.txt  > output.txt
fc %1.txt output.txt >temp.txt
if %ERRORLEVEL%==0 (
   echo All HDF5 C examples tests passed.
) else (
   echo HDF5 C examples tests failed.
   echo.
   more temp.txt
)
del output.txt
del temp.txt
GOTO END

:WRONG
echo The syntax of the command is incorrect.
echo.

:HELP
echo Tests HDF5 C examples.
echo.
echo testExamples [OPTION]
echo.
echo Please use one of the following options!
echo.
echo    testExamples release        test HDF5 C examples -- release version
echo    testExamples release dll    test HDF5 C examples -- release dll version
echo    testExamples debug          test HDF5 C examples -- debug version
echo    testExamples debug dll      test HDF5 C examples -- debug dll version
echo    testExamples /?             Help information
echo.

:END
