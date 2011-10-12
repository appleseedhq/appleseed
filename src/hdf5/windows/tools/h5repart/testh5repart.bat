@echo off
rem
rem Copyright by The HDF Group.
rem Copyright by the Board of Trustees of the University of Illinois.
rem All rights reserved.
rem
rem This file is part of HDF5.  The full HDF5 copyright notice, including
rem terms governing use, modification, and redistribution, is contained in
rem the files COPYING and Copyright.html.  COPYING can be found at the root
rem of the source code distribution tree; Copyright.html can be found at the
rem root level of an installed copy of the electronic HDF5 document set and
rem is linked from the top-level documents page.  It can also be found at
rem http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
rem access to either file, you may request a copy from help@hdfgroup.org.
rem
rem Tests for the h5repart tool
rem
rem    Created:  Scott Wegner, 8/29/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

rem The tool name
set repart=h5repart%2
rem The path of the tool library
set repart_bin=%CD%\..\%repart%\%1\%repart%

rem The test name
set reparted_fam=h5reparttst
rem The path of the test binary
set reparted_fam_bin=%CD%\..\testfiles\%reparted_fam%\%1\%reparted_fam%

set nerrors=0
set verbose=yes

if not exist ..\testfiles mkdir ..\testfiles

set actual_dir=%CD%\..\testfiles

goto main


rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Testing".
rem
:testing
    set test_msg=Testing %dumper%
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    rem We need to replace PERCENT-ZERO here with "%0" for the tfamily test.
    rem --SJW 9/4/07
    set test_msg=!test_msg:PERCENT-ZERO=%%0!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Run a test and print PASS or *FAIL*.  If a test fails then increment
rem the `nerrors' global variable.
rem
:tooltest
    rem Run tool test.
    (
        rem We need to replace PERCENT-ZERO here with "%0" for the tfamily test.
        rem --SJW 9/4/07
        set params=%*
        set params=!params:PERCENT-ZERO=%%0!
        pushd %CD%\..\testfiles
        %repart_bin% !params!
        popd
    )
    
    if %errorlevel% equ 0 (
        call :testing PASSED %repart% %*
    ) else (
        call :testing *FAILED* %repart% %*
        set /a nerrors=!nerrors!+1
    )
    
    exit /b
    
    
:outputtest
    rem Run test program
    (
        pushd %actual_dir%
        %reparted_fam_bin% %*
        popd
    )
    
    if %errorlevel% equ 0 (
        call :testing PASSED %reparted_fam% %*
    ) else (
        call :testing *FAILED* %reparted_fam% %*
        set /a nerrors=!nerrors!+1
    )
    
    exit /b
    
    
rem Print a "SKIP" message
:skip
    call :testing -SKIP- %*
    exit /b
    
    
rem ############################################################################
rem ############################################################################
rem #			  T H E   T E S T S                                ###
rem ############################################################################
rem ############################################################################
:main

    rem On Windows, we pass "PERCENT-ZERO", and let other calls replace it with
    rem the "%0".  We cannot pass "%0" directly because Windows interprets it as
    rem the name of the script.  --SJW 9/4/07
    
    rem repartition family member size to 20,000 bytes.
    call :tooltest -m 20000 family_filePERCENT-ZERO5d.h5 %actual_dir%\fst_familyPERCENT-ZERO5d.h5
    rem repartition family member size to 5 KB.
    call :tooltest -m 5k family_filePERCENT-ZERO5d.h5 %actual_dir%\scd_familyPERCENT-ZERO5d.h5
    rem convert family file to sec2 file of 20,000 bytes
    call :tooltest -m 20000 -family_to_sec2 family_filePERCENT-ZERO5d.h5 %actual_dir%\family_to_sec2.h5

    rem test the output files repartitioned above.
    call :outputtest
    echo.

    if %nerrors% equ 0 (
        echo.All %repart% tests passed.
    )
    
    rem Clean up output file
    if not defined hdf5_nocleanup (
        pushd %actual_dir%
        del /f fst_family*.h5 scd_family*.h5 family_to_sec2.h5
        popd
    )
    
    popd
    endlocal & exit /b %nerrors%
    