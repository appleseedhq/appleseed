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
rem Tests for the h5ls tool
rem
rem    Created:  Scott Wegner, 8/28/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

rem The tool name
set h5ls=h5ls%2
rem The path of the tool binary
set h5ls_bin=%CD%\..\%h5ls%\%1\%h5ls%

rem Max. lines of output to display if test fails
set nlines=20

set nerrors=0
set verbose=yes

if not exist ..\testfiles mkdir ..\testfiles

goto main


rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Testing".
rem
:testing
    set test_msg=Testing %h5ls%
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

    
rem Run a test and print PASS or *FAIL*. For now, if h5ls can complete
rem with exit status 0, consider it pass. If a test fails then increment
rem the `nerrors' global variable and (if $verbose is set) display up to %nlines%
rem lines of the actual output from the tool test.  The actual output is not
rem removed if $HDF5_NOCLEANUP has a non-zero value.
rem Arguemnts:
rem %1 -- actual output filename to use
rem %2 and on -- argument for the h5ls tool
:tooltest
    set expect=%CD%\..\testfiles\%1
    set actual=%CD%\..\testfiles\%~n1.out
    set actual_err=%CD%\..\testfiles\%~n1.err
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/23/07
    set params=%*
    set params=%params:* =%
    
    rem Target the first variable in params, retvalexpect
    for %%a in (%params%) do (
        set retvalexpect=%%a
        set params=%params:* =%
        goto break1
    )
    :break1

    rem Run test.
    rem Stderr is included in stdout so that the diff can detect
    rem any unexpected output from that stream too
    (
        echo.#############################
        rem We strip out the parentesis here because echo on Linux does.
        rem --SJW 8/28/07
        echo. output for 'h5ls %params:"=%'
        echo.#############################
        pushd %CD%\..\testfiles
        %h5ls_bin% %params%
        popd
    ) >%actual% 2>%actual_err%
    
    set exitcode=%errorlevel%
    type %actual_err% >> %actual%
    if "%exitcode%" neq "%retvalexpect%" (
        call :testing *FAILED* %params%
        set /a nerrors=!nerrors!+1
        if "yes"=="%verbose%" (
            echo.test returned with exit code !exitcode!
            echo.test output: ^(up to %nlines% lines^)
            rem Count lines echo'ed, and break out after 20.  --SJW 8/28/07
            set line=0
            for /f "tokens=* delims=" %%a in (%actual%) do (
                if !line! geq %nlines% goto break2
                echo.%%a
                set /a line=!line!+1
            )
            :break2
            echo.***end of test output***
            echo.
        )
    rem Don't special case non-existing expected output as Linux does, because
    rem we depend on it above to parse anyway.  It should be an error if it
    rem doesn't exist.  --SJW 8/28/07
    rem ) else if not exist %expect% (
    rem     rem Create the expect file if it doesn't yet exist
    rem     call :testing CREATED %params%
    rem     copy %actual% %expect% > nul
    ) else (
        fc /w %expect% %actual% > nul
        if !errorlevel! equ 0 (
            call :testing PASSED %params%
        ) else (
            call :testing *FAILED* %params%
            echo.    Expected result differs from actual result
            set nerrors=!nerrors!+1
            if "yes"=="%verbose%" fc %expect% %actual%
        )
    )
    
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_err%
    )
    
    exit /b
    
    
rem ############################################################################
rem ############################################################################
rem #                       T H E   T E S T S                                ###
rem ############################################################################
rem ############################################################################
:main
    
    rem Toss in a bunch of tests.  Not sure if they are the right kinds.
    rem test the help syntax
    call :tooltest help-1.ls 0 -w80 -h
    call :tooltest help-2.ls 0 -w80 -help
    call :tooltest help-3.ls 0 -w80 -?

    rem test simple command
    call :tooltest tall-1.ls 0 -w80 tall.h5
    call :tooltest tall-2.ls 0 -w80 -r -d tall.h5
    call :tooltest tgroup.ls 0 -w80 tgroup.h5
    call :tooltest tgroup-3.ls 0 -w80 tgroup.h5/g1

    rem test for displaying groups
    call :tooltest tgroup-1.ls 1 -w80 -r -g tgroup.h5
    call :tooltest tgroup-2.ls 0 -w80 -g tgroup.h5/g1

    rem test for displaying simple space datasets
    call :tooltest tdset-1.ls 0 -w80 -r -d tdset.h5

    rem test for displaying soft links
    call :tooltest tslink-1.ls 0 -w80 -r tslink.h5

    rem test for displaying more soft links with --follow-symlinks
    call :tooltest tsoftlinks-1.ls 0 --follow-symlinks tsoftlinks.h5
    call :tooltest tsoftlinks-2.ls 0 --follow-symlinks -r tsoftlinks.h5
    call :tooltest tsoftlinks-3.ls 0 --follow-symlinks tsoftlinks.h5/group1
    call :tooltest tsoftlinks-4.ls 0 --follow-symlinks -r tsoftlinks.h5/group1
    call :tooltest tsoftlinks-5.ls 0 --follow-symlinks tsoftlinks.h5/soft_dset1

    rem test for displaying external and user-defined links with 
    rem --follow-symlinks
    call :tooltest textlink-1.ls 0 -w80 -r textlink.h5
    call :tooltest textlinksrc-1.ls 0 -w80 --follow-symlinks -r textlinksrc.h5
    call :tooltest textlinksrc-2.ls 0 -w80 --follow-symlinks -rv textlinksrc.h5/ext_link5
    call :tooltest textlinksrc-3.ls 0 -w80 --follow-symlinks -r textlinksrc.h5/ext_link1
    call :tooltest textlinksrc-4.ls 0 -w80 -r textlinksrc.h5
    call :tooltest textlinksrc-5.ls 0 -w80 -r textlinksrc.h5/ext_link1
    call :tooltest textlinksrc-6.ls 0 -w80 --follow-symlinks textlinksrc.h5
    call :tooltest textlinksrc-7.ls 0 -w80 --follow-symlinks textlinksrc.h5/ext_link1
    call :tooltest tudlink-1.ls 0 -w80 -r tudlink.h5

    rem test for displaying external links with -E
    rem the option -E will be depriciated but keep it for backward compatibility
    call :tooltest textlinksrc-1-old.ls 0 -w80 -Er textlinksrc.h5
    call :tooltest textlinksrc-2-old.ls 0 -w80 -Erv textlinksrc.h5/ext_link5
    call :tooltest textlinksrc-3-old.ls 0 -w80 -Er textlinksrc.h5/ext_link1
    call :tooltest textlinksrc-6-old.ls 0 -w80 -E textlinksrc.h5
    call :tooltest textlinksrc-7-old.ls 0 -w80 -E textlinksrc.h5/ext_link1

    rem tests for no-dangling-links 
    rem if this option is given on dangling link, h5ls should return exit code 1
    rem when used alone , expect to print out help and return exit code 1
    call :tooltest textlinksrc-nodangle-1.ls 1 -w80 --no-dangling-links textlinksrc.h5
    rem external dangling link - expected exit code 1
    call :tooltest textlinksrc-nodangle-2.ls 1 -w80 --follow-symlinks --no-dangling-links textlinksrc.h5
    rem soft dangling link - expected exit code 1
    call :tooltest tsoftlinks-nodangle-1.ls 1 -w80 --follow-symlinks --no-dangling-links tsoftlinks.h5
    rem when used file with no dangling links - expected exit code 0
    call :tooltest thlinks-nodangle-1.ls 0 -w80 --follow-symlinks --no-dangling-links thlink.h5

    rem tests for hard links
    call :tooltest thlink-1.ls 0 -w80 thlink.h5

    rem tests for compound data types
    call :tooltest tcomp-1.ls 0 -w80 -r -d tcompound.h5

    rem test for the nested compound type
    call :tooltest tnestcomp-1.ls 0 -w80 -r -d tnestedcomp.h5
    call :tooltest tnestcomp-2.ls 0 -w80 -r -d -S tnestedcomp.h5
    call :tooltest tnestcomp-3.ls 0 -w80 -r -d -l tnestedcomp.h5
    call :tooltest tnestcomp-4.ls 0 -w80 -r -d -l -S tnestedcomp.h5

    rem test for loop detection
    call :tooltest tloop-1.ls 0 -w80 -r -d tloop.h5

    rem test for string 
    call :tooltest tstr-1.ls 0 -w80 -r -d tstr.h5

    rem test test file created from lib SAF team
    call :tooltest tsaf.ls 0 -w80 -r -d tsaf.h5

    rem test for variable length data types
    call :tooltest tvldtypes1.ls 0 -w80 -r -d tvldtypes1.h5

    rem test for array data types
    call :tooltest tarray1.ls 0 -w80 -r -d tarray1.h5

    rem test for empty data
    call :tooltest tempty.ls 0 -w80 -d tempty.h5

    rem test for all dataset types written to attributes
    rem enable -S for avoiding printing NATIVE types
    call :tooltest tattr2.ls 0 -w80 -v -S tattr2.h5

    rem tests for error handling.
    rem test for non-existing file
    call :tooltest nosuchfile.ls 1 nosuchfile.h5

    rem test for variable length data types in verbose mode 
    call :tooltest tvldtypes2le.ls 0 -v tvldtypes1.h5

    rem test for dataset region references data types in verbose mode 
    call :tooltest tdataregle.ls 0 -v tdatareg.h5

    if %nerrors% equ 0 (
        echo.All h5ls tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
