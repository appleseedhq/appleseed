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
rem Tests for the h5dump tool
rem
rem    Created:  Scott Wegner, 8/27/07
rem    Modified: 
rem

setlocal enabledelayedexpansion
pushd %~dp0

rem set h5_lone_colon=%h5_lone_colon%

rem The tool name
set dumper=h5dump%2
rem The path of the tool library
set dumper_bin=%CD%\..\%dumper%\%1\%dumper%

set nerrors=0
set verbose=yes

if not exist ..\testfiles mkdir ..\testfiles

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
    set test_msg=!test_msg!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

 
rem Run a test and print PASS or *FAIL*.  If a test fails then increment
rem the `nerrors' global variable and (if $verbose is set) display the
rem difference between the actual output and the expected output. The
rem expected output is given as the first argument to this function and
rem the actual output file is calculated by replacing the `.ddl' with
rem `.out'.  The actual output is not removed if HDF5_NOCLEANUP has a
rem non-zero value.
rem
:tooltest
    set expect=%CD%\..\testfiles\%1
    set expect_eol=%CD%\..\testfiles\%~n1.eol
    set actual=%CD%\..\testfiles\%~n1.out
    set actual_err=%CD%\..\testfiles\%~n1.err
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/23/07
    set params=%*
    set params=%params:* =%
    
    rem Run test.
    
    (
        rem Remove quotes here, because Linux 'echo' command strips them.
        rem --SJW 8/24/07
        echo.#############################
        echo.Expected output for 'h5dump !params:"=!'
        echo.#############################
        pushd %CD%\..\testfiles
        %dumper_bin% !params!
        popd
    ) > %actual% 2> %actual_err%
    type %actual_err% >> %actual%
    
    if not exist %expect% (
        call :testing CREATED %params%
        copy %actual% %expect% > nul
    ) else (
        fc /w %expect% %actual% > nul
        if !errorlevel! equ 0 (
            call :testing PASSED %params%
        ) else (
            rem First, check if the error is caused by Unix-style EOL, because
            rem FC can fail incorrectly when comparing them. --SJW 5/30/08
            more < %expect% > %expect_eol%
            fc /w %expect_eol% %actual% > nul
            if !errorlevel! equ 0 (
                call :testing PASSED %params%
            ) else (
                call :testing *FAILED* %params%
                echo.    Expected results ^(*.ddl^) differs from actual results ^(*.out^)
                set /a nerrors=!nerrors!+1
                if "yes"=="%verbose%" fc /w %expect% %actual%
            )
        )
    )
    
    rem Clean up output file
    if not defined HDF5_NOCLEANUP del /f %expect_eol% %actual% %actual_err% 
    
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

    rem test XML
    call :tooltest tall.h5.xml --xml tall.h5
    call :tooltest tattr.h5.xml --xml tattr.h5
    call :tooltest tbitfields.h5.xml --xml tbitfields.h5
    call :tooltest tcompound.h5.xml --xml tcompound.h5
    call :tooltest tcompound2.h5.xml --xml tcompound2.h5
    call :tooltest tdatareg.h5.xml --xml tdatareg.h5
    call :tooltest tdset.h5.xml --xml tdset.h5
    call :tooltest tdset2.h5.xml --xml tdset2.h5
    call :tooltest tenum.h5.xml --xml tenum.h5
    call :tooltest tgroup.h5.xml --xml tgroup.h5
    call :tooltest thlink.h5.xml --xml thlink.h5
    call :tooltest tloop.h5.xml --xml tloop.h5
    call :tooltest tloop2.h5.xml --xml tloop2.h5
    call :tooltest tmany.h5.xml --xml tmany.h5
    call :tooltest tnestedcomp.h5.xml --xml tnestedcomp.h5
    call :tooltest tcompound_complex.h5.xml --xml tcompound_complex.h5
    call :tooltest tobjref.h5.xml --xml tobjref.h5
    call :tooltest topaque.h5.xml --xml topaque.h5
    call :tooltest tslink.h5.xml --xml tslink.h5
    call :tooltest tudlink.h5.xml --xml tudlink.h5
    call :tooltest textlink.h5.xml --xml textlink.h5
    call :tooltest tstr.h5.xml --xml tstr.h5
    call :tooltest tstr2.h5.xml --xml tstr2.h5
    call :tooltest tref.h5.xml --xml tref.h5
    call :tooltest tname-amp.h5.xml --xml tname-amp.h5
    call :tooltest tname-apos.h5.xml --xml tname-apos.h5
    call :tooltest tname-gt.h5.xml --xml tname-gt.h5
    call :tooltest tname-lt.h5.xml --xml tname-lt.h5
    call :tooltest tname-quot.h5.xml --xml tname-quot.h5
    call :tooltest tname-sp.h5.xml --xml tname-sp.h5
    call :tooltest tstring.h5.xml --xml tstring.h5
    call :tooltest tstring-at.h5.xml --xml tstring-at.h5
    call :tooltest tref-escapes.h5.xml --xml tref-escapes.h5
    call :tooltest tref-escapes-at.h5.xml --xml tref-escapes-at.h5
    call :tooltest tnodata.h5.xml --xml tnodata.h5
    call :tooltest tarray1.h5.xml --xml tarray1.h5
    call :tooltest tarray2.h5.xml --xml tarray2.h5
    call :tooltest tarray3.h5.xml --xml tarray3.h5
    call :tooltest tarray6.h5.xml --xml tarray6.h5
    call :tooltest tarray7.h5.xml --xml tarray7.h5
    call :tooltest tvldtypes1.h5.xml --xml tvldtypes1.h5
    call :tooltest tvldtypes2.h5.xml --xml tvldtypes2.h5
    call :tooltest tvldtypes3.h5.xml --xml tvldtypes3.h5
    call :tooltest tvldtypes4.h5.xml --xml tvldtypes4.h5
    call :tooltest tvldtypes5.h5.xml --xml tvldtypes5.h5
    call :tooltest tvlstr.h5.xml --xml tvlstr.h5
    call :tooltest tsaf.h5.xml --xml tsaf.h5
    call :tooltest tempty.h5.xml --xml tempty.h5
    call :tooltest tnamed_dtype_attr.h5.xml --xml tnamed_dtype_attr.h5
    rem Test dataset and attribute of null space.  Commented out:
    rem wait until the XML schema is updated for null space. 
    rem call :tooltest tnullspace.h5.xml --xml tnulspace.h5

    rem other options for xml

    call :tooltest tempty-dtd.h5.xml --xml --use-dtd tempty.h5
    call :tooltest tempty-dtd-2.h5.xml --xml -u tempty.h5

    rem The lone colon here confuses some systems (Cray X1).  Skip
    rem it if configure detects that this is a problem.
    if not "X$H5_LONE_COLON"=="Xno" (
        call :tooltest tempty-nons.h5.xml --xml -X ":" tempty.h5
    ) else (
        call :skip tempty-nons.h5.xml --xml -X ":" tempty.h5
    )

    call :tooltest tempty-nons-2.h5.xml --xml --xml-ns=":" tempty.h5

    rem Some of these combinations are syntactically correct but
    rem  the URLs are dummies 
    call :tooltest tempty-ns.h5.xml --xml -X "thing:" tempty.h5
    call :tooltest tempty-ns-2.h5.xml --xml --xml-ns="thing:" tempty.h5
    call :tooltest tempty-nons-uri.h5.xml --xml --xml-ns=":" --xml-dtd="http://somewhere.net" tempty.h5
    call :tooltest tempty-dtd-uri.h5.xml --xml --use-dtd --xml-dtd="http://somewhere.net" tempty.h5

    call :tooltest tall-2A.h5.xml --xml -A tall.h5

    
    rem tests for attribute order
    call :tooltest torderattr1.h5.xml --xml -H --sort_by=name --sort_order=ascending torderattr.h5
    call :tooltest torderattr2.h5.xml --xml -H --sort_by=name --sort_order=descending torderattr.h5
    call :tooltest torderattr3.h5.xml --xml -H --sort_by=creation_order --sort_order=ascending torderattr.h5
    call :tooltest torderattr4.h5.xml --xml -H --sort_by=creation_order --sort_order=descending torderattr.h5    
    
    
    
    if %nerrors% equ 0 (
        echo.All %dumper% tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
