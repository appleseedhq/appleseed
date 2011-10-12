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
rem Tests for the h5stat tool
rem
rem    Created:  Scott Wegner, 8/28/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

set EXIT_SUCCESS=0
set EXIT_FAILURE=1

set h5pubconf=%CD%\..\..\src\h5pubconf.h

rem Determine which filters are available
rem On Windows, the function :detect_filter sets these for us
call :detect_filter szip
call :detect_filter deflate
call :detect_filter shuffle
call :detect_filter fletcher32
call :detect_filter nbit
call :detect_filter scaleoffset

rem The tool name
set stat=h5stat%2
rem The path of the tool binary
set stat_bin=%CD%\..\%stat%\%1\%stat%

set nerrors=0
set verbose=yes

if not exist .\testfiles mkdir .\testfiles

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
    set test_msg=!test_msg!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Run a test and print PASS or *FAIL*.  If a test fails then increment
rem the `nerrors' global variable and (if %verbose% is set) display the
rem difference between the actual output and the expected output. The
rem expected output is given as the first argument to this function and
rem the actual output file is calculated by replacing the `.ddl' with
rem `.out'.  The actual output is not removed if %HDF5_NOCLEANUP% has a
rem non-zero value.
rem
:tooltest
    set expect=%CD%\testfiles\%1
    set actual=%CD%\testfiles\%~n1.out
    set actual_err=%CD%\testfiles\%~n1.err
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/28/07
    set params=%*
    set params=%params:* =%
    
    rem Run test.
    (
        echo.#############################
        rem Filter out quotes because they do on Linux.  --SJW 8/28/07
        echo.Expected output for 'h5stat %params:"=%'
        echo.#############################
        pushd %CD%\testfiles
        %stat_bin% %params%
        popd
    ) > %actual% 2> %actual_err%
    type %actual_err% >> %actual%
    
    
    if not exist %expect% (
        rem Create the expect file if it doesn't yet exist
        call :testing CREATED %stat% %params%
        copy /y %actual% %expect%
    ) else (
        fc /w %expect% %actual% > nul
        if !errorlevel! equ 0 (
            call :testing PASSED %stat% %params%
        ) else (
            call :testing *FAILED* %stat% %params%
            echo.    Expected results ^(*.ddl^) differs from actual result ^(*.out^)
            set /a nerrors=!nerrors!+1
            if "yes"=="%verbose%" fc %expect% %actual%
        )
    )
    
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_err%
    )
    
    exit /b
    
    
rem This is a Windows-specific function that detects if the filter passed
rem should be enabled for this test script.  It searches H5pubconf.h for the
rem string "#define H5_HAVE_FILTER_%1" and sets the variable "use_filter_%1"
rem accordingly.  On other platforms, this variable is set in the Makefile.
rem If we find a better way to test this in the future, we should use it.
rem --SJW 9/4/07
:detect_filter
    findstr /b /i /c:"#define H5_HAVE_FILTER_%1" %h5pubconf% > nul
    if %errorlevel% equ 0 (
        set use_filter_%1=yes
    ) else (
        set use_filter_%1=no
    )
    
    exit /b


rem Print a "SKIP" message
:skip
    call :testing -SKIP- %*
    exit /b

    
rem ############################################################################
rem ############################################################################
rem #                       T H E   T E S T S                                ###
rem ############################################################################
rem ############################################################################
:main

    rem Test for help flag
    call :tooltest h5stat_help1.ddl -h
    call :tooltest h5stat_help2.ddl --help

    rem Test file with groups, compressed datasets, user-applied fileters, etc.
    rem h5stat_filters.h5 is a copy of ../../testfiles/tfilters.h5 as of release 1.8.0-alpha4
    call :tooltest h5stat_filters.ddl h5stat_filters.h5
    call :tooltest h5stat_filters-file.ddl  -f   h5stat_filters.h5
    call :tooltest h5stat_filters-F.ddl  -F   h5stat_filters.h5
    call :tooltest h5stat_filters-d.ddl  -d   h5stat_filters.h5
    call :tooltest h5stat_filters-g.ddl  -g   h5stat_filters.h5
    call :tooltest h5stat_filters-dT.ddl -dT  h5stat_filters.h5
    call :tooltest h5stat_filters-UD.ddl -D  h5stat_filters.h5
    call :tooltest h5stat_filters-UT.ddl -T  h5stat_filters.h5
    rem h5stat_tsohm.h5 is a copy of ../../../test/tsohm.h5 generated by tsohm.c 
    rem as of release 1.8.0-alpha4
    call :tooltest h5stat_tsohm.ddl h5stat_tsohm.h5
    rem h5stat_newgrat.h5 is generated by h5stat_gentest.c
    call :tooltest h5stat_newgrat.ddl h5stat_newgrat.h5
    call :tooltest h5stat_newgrat-UG.ddl -G h5stat_newgrat.h5
    call :tooltest h5stat_newgrat-UA.ddl -A h5stat_newgrat.h5
    echo.
    
    if %nerrors% equ 0 (
        echo.All %stat% tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
