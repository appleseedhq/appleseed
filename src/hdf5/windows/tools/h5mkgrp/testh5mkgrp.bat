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
rem Tests for the h5mkgrp tool
rem
rem    Created:  Scott Wegner, 8/29/07
rem    Modified: 
rem

rem We currently only build static version of h5mkgrp, but this batch file is
rem setup for dll versions, in case we decide to build them in the future.
rem --SJW 8/29/07

setlocal enabledelayedexpansion
pushd %~dp0

rem The tool name
set h5mkgrp=h5mkgrp%2
rem The path of the tool binary
set h5mkgrp_bin=%CD%\..\%h5mkgrp%\%1\%h5mkgrp%
rem The h5ls tool name
set h5ls=h5ls%2
rem Arguments to the h5ls tool
set h5ls_args=-vr
rem The path of the h5ls tool binary
set h5ls_bin=%CD%\..\%h5ls%\%1\%h5ls%

set nerrors=0
set verbose=yes

set indir=%CD%\..\testfiles
set outdir=%CD%\..\testfiles

if not exist %outdir% mkdir %outdir%

goto main

rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Testing".
rem
:testing
    set test_msg=Testing
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    rem We need to replace PERCENT-ZERO here with "%0" for the tfamily test.
    rem --SJW 8/24/07
    set test_msg=!test_msg:PERCENT-ZERO=%%0!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Verifying".
rem
:verify_h5ls
    set test_msg=Verifying h5ls file structure
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    rem We need to replace PERCENT-ZERO here with "%0" for the tfamily test.
    rem --SJW 8/24/07
    set test_msg=!test_msg:PERCENT-ZERO=%%0!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Run a test and print PASS or *FAIL*. If h5mkgrp can complete
rem with exit status 0, consider it pass. If a test fails then increment
rem the `nerrors' global variable.
rem Assumed arguments:
rem %* arguments for h5mkgrp.
:tooltest
    (
        echo.#############################
        echo. output for 'h5mkgrp %*'
        echo.#############################
        %h5mkgrp_bin% %*
    ) > output.out

    if %errorlevel% neq 0 (
        call :testing *FAILED* %*
        echo.failed result is:
        type output.out
        set nerrors=!nerrors!+1
    ) else (
        call :testing PASSED %*
    )
    
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f output.out
    )
    
    exit /b
    
    
rem Call the h5ls tool to verify the correct output data in the destination file
rem
:h5lstest
    set expect=%indir%\%~n1.ls
    set expect_parsed=%expect%.parsed
    set actual=%outdir%\%~n1.out
    set actual_parsed=%actual%.parsed
    
    rem Stderr is included in stdout so that the diff can detect
    rem any unexpected output from that stream too
    (
        echo.#############################
        echo.Expected output from h5ls %*
        echo.#############################
        %h5ls_bin% %h5ls_args% %*
    ) >%actual% 2>&1
    rem Windows doesn't have "sed" command, and parsing the files line-by-line
    rem to emulate Unix takes a very long time.  Instead, we simply remove lines
    rem with "Modified".  Do this for actual and expected otput.  If there is a 
    rem better alternative in the future, we should use it instead. --SJW 8/29/07
    for %%a in (expect actual) do (
        findstr /v /c:"    Modified:" !%%a! > tmp.txt
        move /y tmp.txt !%%a_parsed! > nul
    )

    rem Don't special case non-existing expected output as Linux does, because
    rem we depend on it above to parse anyway.  It should be an error if it
    rem doesn't exist.  --SJW 8/29/07
    rem if not exist %expect% (
    rem     call :verify_h5ls CREATED %*
    rem     copy %actual% %expect%
    rem )
    
    fc /w %expect_parsed% %expect_parsed% > nul
    if %errorlevel% equ 0 (
        call :verify_h5ls PASSED %*
    ) else ( 
        call :verify_h5ls *FAILED* %*
        echo.    Expected result ^(*.ls^) differs from actual result ^(*.out^)
        set /a nerrors=!nerrors!+1
        if "%verbose%"=="yes" fc %epect% %actual%
    )
    
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_parsed% %expect_parsed%
    )
    
    exit /b
    
    
rem Single run of tool
rem
rem Assumed arguments:
rem %1 is test file name
rem %2 is h5mkgrp options
rem %* are groups to create
:runtest

    set fileout=%outdir%\%1
    shift
    set h5mkgrp_args=%1
    rem Filter out quotes
    set h5mkgrp_args=%h5mkgrp_args:"=%
    shift
    
    rem Remove any output file left over from previous test run
    del /f %fileout% 2> nul
    
    rem On Windows, the shift command doesn't actually affect %*, so we must
    rem manipulate a params variable.  We need to be careful of how we iterate
    rem through them, because the " " parameter is tricky on Windows. 
    rem --SJW 8/29/07
    set params=
    if not "%1"=="" (
        set p_num=1
        for %%a in (%*) do (
            if !p_num! geq 3 (
                set params=!params! %%a
            )
            set /a p_num=!p_num!+1
        )
        rem Remove leading space
        set params=!params:* =!
    )
    
    rem Run test
    call :tooltest %h5mkgrp_args% %fileout% %params%
    
    rem Verify that the file created above is correct
    call :h5lstest %fileout%
    
    rem Remove output file created, if the "no cleanup" environment variable is
    rem not defined.
    rem Why do we echo FILEOUT on Linux?  --SJW 8/29/07
    rem echo.FILEOUT= %fileout%
    if not defined hdf5_nocleanup (
        del /f %fileout% 2> nul
    )
    
    exit /b
    
    
rem ############################################################################
rem #           T H E   T E S T S                                            ###
rem ############################################################################
:main
    rem Check that help & version is displayed properly
    call :runtest h5mkgrp_help.h5 "-h"
    call :runtest h5mkgrp_version.h5 "-V"

    rem Create single group at root level
    call :runtest h5mkgrp_single.h5 " " single
    call :runtest h5mkgrp_single.h5 "-v" single
    call :runtest h5mkgrp_single.h5 "-p" single
    call :runtest h5mkgrp_single_latest.h5 "-l" latest

    rem Create several groups at root level
    call :runtest h5mkgrp_several.h5 " " one two
    call :runtest h5mkgrp_several.h5 "-v" one two
    call :runtest h5mkgrp_several.h5 "-p" one two
    call :runtest h5mkgrp_several_latest.h5 "-l" one two

    rem Create various nested groups 
    call :runtest h5mkgrp_nested.h5 "-p" /one/two
    call :runtest h5mkgrp_nested_latest.h5 "-lp" /one/two
    call :runtest h5mkgrp_nested_mult.h5 "-p" /one/two /three/four
    call :runtest h5mkgrp_nested_mult_latest.h5 "-lp" /one/two /three/four

    
    if %nerrors% equ 0 (
        echo.All h5mkgrp tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
