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
rem Tests for the h5copy tool
rem
rem    Created:  Scott Wegner, 8/16/07
rem    Modified: Scott Wegner, 8/22/07
rem


rem We don't currently build DLL version of h5copy, but this test script is
rem setup to support it if we do in the future.  --SJW 8/22/07

setlocal enabledelayedexpansion
pushd %~dp0

set EXIT_SUCCESS=0
set EXIT_FAILURE=1

rem The tool name
set h5copy=h5copy%2
rem The path of the tool binary
set h5copy_bin=%CD%\%1\%h5copy%
rem The h5diff tool name
set h5diff=h5diff%2
rem The path of the h5diff too binary
set h5diff_bin=%CD%\..\%h5diff%\%1\%h5diff%
rem The h5ls tool name
set h5ls=h5ls%2
rem Arguments to the h5ls tool
set h5ls_args=-Svr
rem The path of the h5ls tool binary
set h5ls_bin=%CD%\..\%h5ls%\%1\%h5ls%

set /a nerrors=0
set verbose=yes

set srcfile1=h5copytst.h5
set srcfile2=h5copy_ref.h5
set hdf_ext_src_file=h5copy_extlinks_src.h5
set hdf_ext_trg_file=h5copy_extlinks_trg.h5

set indir=%CD%\testfiles
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
    set test_msg=%test_msg%                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Verifying".
rem
:verify
    set verify_msg=Verifying h5diff output
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set verify_msg=!verify_msg! %%~nxa
        ) )
    )
    set verify_msg=%verify_msg%                                                                
    echo.%verify_msg:~0,69% %1
    
    exit /b

rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Verifying".
rem
:verify_h5ls
    set verifyh5ls_msg=Verifying h5ls file structure
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set verifyh5ls_msg=!verifyh5ls_msg! %%~nxa
        ) )
    )
    set verifyh5ls_msg=%verifyh5ls_msg%                                                                
    echo.%verifyh5ls_msg:~0,69% %1
    
    exit /b

    
    
rem Run a test and print PASS or *FAIL*. If h5copy can complete
rem with exit status 0, consider it pass. If a test fails then increment
rem the `nerrors' global variable.
rem Assumed arguments:
rem $1 is -i
rem $2 is input file
rem $3 is -o
rem $4 is output file
rem $* everything else arguments for h5copy.

:tooltest
    set runh5diff=yes
    if "%1"=="-i" (
        set inputfile=%2
    ) else (
        set runh5diff=no
    )
    if "%3"=="-o" (
        set outputfile=%4
    ) else (
        set runh5diff=no
    )
    
    (
        echo.#############################
        echo. output for %h5copy%  %*
        echo.#############################
        %h5copy_bin% %*
    ) > output.out
    
    if %errorlevel% neq 0 (
        call :testing *FAILED* %h5copy% %*
        echo.failed result is:
        type output.out
        set /a nerrors=!nerrors!+1
    ) else (
        call :testing PASSED %h5copy% %*
        
        rem Clean up output file
        if not defined HDF5_NOCLEANUP (
            del /f output.out
        )
    )
    
    if %runh5diff% neq no (
        call :h5difftest %inputfile% %outputfile% %7 %9
    )
    
    exit /b
    
    
:tooltest_fail
    set runh5diff=yes
    if "%1"=="-i" (
        set inputfile=%2
    ) else (
        set runh5diff=no
    )
    if "%3"=="-o" (
        set outputfile=%4
    ) else (
        set runh5diff=no
    )
    
    (
        echo.#############################
        echo. output for %h5copy%  %*
        echo.#############################
        %h5copy_bin% %*
    ) > output.out
    
    if %errorlevel% neq 0 (
        call :testing *FAILED* %h5copy% %*
        echo.failed result is:
        type output.out
        set /a nerrors=!nerrors!+1
    ) else (
        call :testing PASSED %h5copy% %*
        
        rem Clean up output file
        if not defined HDF5_NOCLEANUP (
            del /f output.out
        )
    )
    
    if %runh5diff% neq no (
        call :h5difftest_fail %inputfile% %outputfile% %7 %9
    )
    
    exit /b
    
    
rem Call the h5diff tool
rem
:h5difftest
    %h5diff_bin% -q %*
    if %errorlevel% neq 0 (
        call :verify *FAILED* %*
        set /a nerrors=!nerrors!+1
    ) else (
        call :verify PASSED %*
    )
    
    exit /b
    
    
rem Call the h5diff tool with a call that is expected to fail
rem
:h5difftest_fail
    %h5diff_bin% -q %*
    if %errorlevel% neq 1 (
        call :verify *FAILED* %*
        set /a nerrors=!nerrors!+1
    ) else (
        call :verify PASSED %*
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
    rem any unexpected output from that stream too.
    rem
    rem Note: The modification time and storage utilization are masked off
    rem so that the output is more portable
    (
        echo.#############################
        echo.Expected output for %h5ls% %*
        echo.#############################
        %h5ls_bin% %h5ls_args% %*
    ) > %actual% 2>&1
    
    rem Windows doesn't have "sed" command, and parsing the files line-by-line
    rem to emulate Unix takes a very long time.  Instead, we simply remove lines
    rem with "Modified" or "Storage".  We also remove lines "Opened (filename) 
    rem with sec2 driver" and "Expected output for (h5ls test)", because Windows 
    rem paths differ from Linux.  Do this for actual and expected otput.
    rem If there is a better alternative in the future, we should use it instead.
    rem --SJW 8/22/07
    for %%a in (expect actual) do (
        findstr /v /c:"    Modified:" !%%a! > tmp.txt
        findstr /v /c:"    Storage:" tmp.txt > tmp2.txt
        findstr /v /b /c:"Expected output for " tmp2.txt > tmp.txt
        findstr /v /b /c:"Opened " tmp.txt > !%%a_parsed!
    )
    del /f tmp.txt tmp2.txt
        
    rem Don't special case non-existing expected output as Linux does, because
    rem we depend on it above to parse anyway.  It should be an error if it
    rem doesn't exist.  --SJW 8/22/07
rem    if not exist %expect% (
rem        rem Create the expect file if it doesn't yet exist
rem        call :verify_h5ls CREATED %*
rem        copy %actual% %expect%
rem    ) else (
    fc %expect_parsed% %actual_parsed% > nul
    if %errorlevel% equ 0 (
        call :verify_h5ls PASSED %*
    ) else (
        call :verify_h5ls *FAILED* %*
        echo.    Expected result ^(*.ls^) differs from actual result ^(*.out^)
        set /a nerrors=!nerrors!+1
        if "yes"=="%verbose%" fc %expect_parsed% %actual_parsed%
    )
rem    )
    
    rem Clean up output file
    if not defined HDF5_NOCLEANUP (
        for %%a in (%actual% %actual_parsed% %expect_parsed%) do del /f %%a
    )
    
    exit /b
    
    
    
rem Copy single datasets of various forms from one group to another,
rem       adding object copied to the destination file each time
rem
rem Assumed arguments:
rem <none>
:copyobjects
    
    set testfile=%indir%\%srcfile1%
    set fileout=%outdir%\%srcfile1:.h5=.out.h5%
    
    rem Remove any output file left over from previous test run
    del /f %fileout% 2> nul
    
    echo.Test copying various forms of datasets
    call :tooltest -i %testfile% -o %fileout% -v -s simple     -d simple
    call :tooltest -i %testfile% -o %fileout% -v -s chunk      -d chunk
    call :tooltest -i %testfile% -o %fileout% -v -s compact    -d compact
    call :tooltest -i %testfile% -o %fileout% -v -s compound   -d compound
    call :tooltest -i %testfile% -o %fileout% -v -s compressed -d compressed
    call :tooltest -i %testfile% -o %fileout% -v -s named_vl   -d named_vl
    call :tooltest -i %testfile% -o %fileout% -v -s nested_vl  -d nested_vl

    echo.Test copying dataset within group in source file to root of destination
    call :tooltest -i %testfile% -o %fileout% -v -s grp_dsets/simple  -d simple_top

    echo.Test copying ^& renaming dataset.
    call :tooltest -i %testfile% -o %fileout% -v -s compound   -d rename

    echo.Test copying empty, 'full' ^& 'nested' groups
    call :tooltest -i %testfile% -o %fileout% -v -s grp_empty  -d grp_empty
    call :tooltest -i %testfile% -o %fileout% -v -s grp_dsets  -d grp_dsets
    call :tooltest -i %testfile% -o %fileout% -v -s grp_nested -d grp_nested

    echo.Test copying dataset within group in source file to group in destination
    call :tooltest -i %testfile% -o %fileout% -v -s /grp_dsets/simple  -d /grp_dsets/simple_group

    echo.Test copying ^& renaming group
    call :tooltest -i %testfile% -o %fileout% -v -s grp_dsets  -d grp_rename

    echo.Test copying 'full' group hierarchy into group in destination file
    call :tooltest -i %testfile% -o %fileout% -v -s grp_dsets  -d /grp_rename/grp_dsets

    echo.Test copying objects into group hier. that doesn't exist yet in destination file
    call :tooltest -i %testfile% -o %fileout% -vp -s simple    -d /A/B1/simple
    call :tooltest -i %testfile% -o %fileout% -vp -s simple    -d /A/B2/simple2
    call :tooltest -i %testfile% -o %fileout% -vp -s /grp_dsets/simple    -d /C/D/simple
    call :tooltest -i %testfile% -o %fileout% -vp -s /grp_dsets -d /E/F/grp_dsets
    call :tooltest -i %testfile% -o %fileout% -vp -s /grp_nested -d /G/H/grp_nested

    rem Verify that the file created above is correct
    call :h5lstest %fileout%

    rem Remove output file created, if the "no cleanup" environment variable is
    rem   not defined
    if not defined HDF5_NOCLEANUP (
        del /f %fileout%
    )

    exit /b
    

rem Copy references in various way.
rem adding to the destination file each time compare the result
rem
rem Assumed arguments:
rem <none>
:copyreferences 

    set testfile=%indir%\%srcfile2%
    set fileout=%outdir%\%srcfile2:.h5=.out.h5%

    rem Remove any output file left over from previous test run
    del /f %fileout% 2> nul

    echo.Test copying object and region references
    rem echo.TOOLTEST -f ref -i $TESTFILE -o $FILEOUT -v -s / -d /COPY
    call :tooltest -f ref -i %testfile% -o %fileout% -v -s / -d /COPY

    rem Verify that the file created above is correct
    call :h5lstest %fileout%

    rem Remove output file created, if the "no cleanup" environment variable is
    rem   not defined
    if not defined HDF5_NOCLEANUP (
        del /f %fileout%
    )

    exit /b
 
rem Copy external links.
rem adding to the destination file each time compare the result
rem
rem Assumed arguments:
rem <none>
:copy_ext_links 

    set testfile=%indir%\%hdf_ext_src_file%
    set fileout=%outdir%\%hdf_ext_src_file:.h5=.out.h5%

    rem Remove any output file left over from previous test run
    del /f %fileout% 2> nul

    echo.Test copying external link directly without -f ext
    call :tooltest -v -i %testfile% -o %fileout% -s /group_ext/extlink_dset -d /copy1_dset

    echo.Test copying external link directly with -f ext
    call :tooltest -f ext -i %testfile% -o %fileout% -v -s /group_ext/extlink_dset -d /copy2_dset

    echo.Test copying dangling external link (no obj) directly without -f ext
    call :tooltest -v -i %testfile% -o %fileout% -s /group_ext/extlink_notyet1 -d /copy_dangle1_1

    echo.Test copying dangling external link (no obj) directly with -f ext
    call :tooltest -f ext -i %testfile% -o %fileout% -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_2

    echo.Test copying dangling external link (no file) directly without -f ext
    call :tooltest -v -i %testfile% -o %fileout% -s /group_ext/extlink_notyet2 -d /copy_dangle2_1

    echo.Test copying dangling external link (no file) directly with -f ext
    call :tooltest -f ext -i %testfile% -o %fileout% -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_2

    echo.Test copying a group contains external links without -f ext
    call :tooltest -v -i %testfile% -o %fileout% -s /group_ext -d /copy1_group

    echo.Test copying a group contains external links with -f ext
    call :tooltest -f ext -i %testfile% -o %fileout% -v -f ext -s /group_ext -d /copy2_group

    rem Verify that the file created above is correct
    call :h5lstest %fileout%

    rem Remove output file created, if the "no cleanup" environment variable is
    rem   not defined
    if not defined HDF5_NOCLEANUP (
        del /f %fileout%
    )

    exit /b
     
rem ##############################################################################
rem ###           T H E   T E S T S                                            ###
rem ##############################################################################

:main
    call :copyobjects
    call :copyreferences
    call :copy_ext_links

    if %nerrors% equ 0 (
        echo.All h5copy tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
