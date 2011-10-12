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
rem Tests for the h5jam/h5unjam tools
rem
rem    Created:  Scott Wegner, 8/27/07
rem    Modified:
rem

rem We currently don't build DLL version os h5jam / h5unjam, but the test script
rem is setup to handle it if we ever decide to.  --SJW 8/27/07

setlocal enabledelayedexpansion
pushd %~dp0

set h5pubconf=%CD%\..\..\src\h5pubconf.h

rem Determine which filters are available
rem On Windows, the function :detect_filter sets these for us
call :detect_filter szip
call :detect_filter deflate
call :detect_filter shuffle
call :detect_filter fletcher32

rem The dumper to use
set dumper=..\h5dump%2\%1\h5dump%2
rem The path of the dumper binary
set dumper_bin=%CD%\%dumper%

rem Tool to test
set jam=h5jam%2
rem Tool to test
set unjam=h5unjam%2
rem The path of the jam binary
set jam_bin=%CD%\..\%jam%\%1\%jam%
rem The path of the unjam binary
set unjam_bin=%CD%\..\%unjam%\%1\%unjam%

rem The tellub to use
set tellub=..\..\test\tellub%2\%1\tellub%2
rem The path of the tellub binary
set tellub_bin=%CD%\%tellub%

rem The getub to use
set getub=..\..\test\getub%2\%1\getub%2
rem The path of the getub binary
set getub_bin=%CD%\%getub%

set nerrors=0
set verbose=yes

set testfiles=%CD%\testfiles

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
    rem Replace ARROW_RIGHT with the correct symbol.  If it was passed directly,
    rem our output would be incorrectly redirected.  --SJW 8/27/07
    set test_msg=!test_msg:ARROW_RIGHT=^>!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

 
rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Compare".
rem
:compare
    set test_msg=Compare
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    set test_msg=!test_msg!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Print a "SKIP" message
:skip
    call :testing -SKIP- %*
    exit /b


rem 
rem  COMPARE_FILES a.h5 b.h5
rem    Compare two files, skipping the first line.  This is used to 
rem    compare the output of the dumper, skipping the file name which
rem    is different.
rem    The result is stored in 'compval'.
rem
:compare_files
    rem The easiest way to compare 2 files on Windows and skip the first line
    rem is to simply filter the first line differences from the output.  If the
    rem first line is different, FC will also display the second line.
    rem --SJW 8/27/07
    fc %1 %2 | findstr /v /b /c:"Comparing files" | findstr /v /b /c:"*****" > cmp1
    findstr /v /b /c:"    1:  " cmp1 | findstr /v /b /c:"    2:  " > cmp2
    findstr /b /c:"  " cmp2 > nul
    if %errorlevel% neq 1 (
        set cmpval=1
    ) else (
        set cmpval=0
    )
    del /f cmp1 cmp2
    
    exit /b
    
    
rem CLEANUP files
rem     Clean up named files
:cleanup
    if not defined hdf5_nocleanup (
        for %%a in (%*) do (
            del /f %%a 2> nul
        )
    )
    exit /b
    
    
rem  SETUP file tocopy
rem    Clone a standard input file in the test directory
rem
:setup
    copy /y %1 %2 > nul
    exit /b
    
    
rem
rem  CHECKFILE orig.h5 compar.h5
rem     Check that the test file is the same as an original.
rem     The two files are dumped with the dumper, and the output
rem     compared with COMPARE_FILES.
rem     If the files are the same, the test reports " PASSED",
rem     otherwise, it reports "*FAILED*"
:checkfile
    set expected=%~dpn2.out
    set expected_err=%~dpn2.err
    set actual=%~n1.out
    set actual_err=%~n1.err
    %dumper_bin% %1 >%expected% 2>%expected_err%
    type %expected_err% >> %expected%
    
    rem dump the test file
    %dumper_bin% %2 >%actual% 2>%actual_err%
    type %actual_err% >> %actual%
    
    rem compare the two files (ignore line 1)
    call :compare_files %actual% %expected%
    if "%cmpval%"=="0" (
        call :compare PASSED %2 to %1
    ) else (
        call :compare *FAILED* %2 to %1
        echo.    Expected result ^(*.ddl^) differs from actual result ^(*.out^)
        set /a nerrors=!nerrors!+1
        if "yes"=="%verbose%" fc %expected% %actual%
    )
    
    rem Clean up output files
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_err%
        del /f %expected% %expected_err%
    )
    exit /b
    
    
rem
rem CHECK_UB file.h5 user_block_file origfile.h5 
rem
rem   Check the user block in 'file.h5' is the same as
rem   'user_block' (allowing for padding).
rem
rem   If the original file had a user block before the test
rem   then 'compare.h5' is passed.  The user block must be extracted
rem   and the test file compared to:
rem      cat compare_ub user_block_file.
rem
rem   This test uses './getub' to extract the user block from 
rem   'file.h5', which is compared to the file described above.
rem
rem   The result is set in variable 'result1'.
rem
:check_ub_1
    set hfile=%1
    set ufile=%2
    
    rem check for third argument (the original file)
    set origfile=
    if not "%3"=="" (
        set origfile="%3"
    )
    
    rem find the length of the user block to check
    for /f "tokens=4" %%a in ('dir /-c %ufile% ^| findstr /v /b /c:" "') do (
        set s1=%%a
    )
    if "%s1%"=="0" (
        echo.File %ufile% is empty
        set result1=1
    )
    
    rem Get the size of the original user block, if any
    if defined origfile (
        rem 'tellub' calls H5Fget_user_block to get the size
        rem of the user block
        for /f %%a in ('%tellub_bin% %origfile%') do set s2=%%a
        if "!s2!"=="0" (
            set size=%s1%
            set cmpfile=%ufile%
        ) else (
            set cmpfile=tt2
            set /a size=!s2!+%s1%
            %getub_bin% -c !s2! %origfile% > !cmpfile!
            type %ufile% >> !cmpfile!
        )
    ) else (
        rem assume no user block
        set s2=0
        set size=%s1%
        set cmpfile=%ufile%
    )
    
    rem Extract 'size' bytes from the front of 'hfile'
    rem Compare to 'cmpfile', result is set in result1
    set tfile=tt1
    %getub_bin% -c %size% %hfile% > %tfile%
    fc /w %cmpfile% %tfile% > nul
    if %errorlevel% neq 0 (
        fc /w %cmpfile% %file%
        set result1=1
    ) else (
        set result1=0
    )
    
    rem clean up
    del /f %tfile%
    if not "%s2%"=="0" (
        del /f %cmpfile%
    )
    exit /b
    
    
rem  CHECK_NOUB file.h5
rem
rem  Check that 'file.h5' has no user block.
rem  Setst result2 to 1 if there is a user block (fail), 0 if none (pass)
:check_noub
    set hfile=%1
    rem call `ubsize` to get the size of the user block
    %tellub_bin% %hfile% > tmp.txt
    if %errorlevel% neq 0 (
        rem error
        set result2=1
    ) else (
        for /f %%a in (tmp.txt) do set ubsize=%%a
        if "!ubsize!"=="0" (
            rem pass
            set result2=0
        ) else (
            rem fail
            set result2=1
        )
    )
    del /f tmp.txt 2> nul
    
    exit /b

    
rem  JAMTEST user_block file.h5 [--clobber] [ofile.h5]
rem
rem    Test the 'jam' tool:
rem      1. figure out the input and output, and the comparision
rem         that will be done.
rem      2. call 'jam' with the appropriate arguments
rem      3. check the user block is correct in the output (Check_UB)
rem    If the user block is correct, print "PASSED", else "*FAILED*"
:jamtest
    set ufile=%1
    set ifile=%2
    rem the file to test
    set compare_test=
    rem the comparison to test against
    set compare_orig=
    set cleanup=
    
    rem sort out the arguments for the test and the check
    set do_clobber=no
    if "%3"=="--clobber" (
        rem clobber overwrites and existing user block
        set do_clobber=yes
        set clobber=--clobber
        set compare_orig=
        if "%4"=="" (
            rem output goes to infile, compare ubfile to infile
            set ofile=
            set compare_test=%ifile%
        ) else (
            rem output goes to %4, compare ofile to ubfile
            set ofile=%4
            set compare_test=!ofile!
        )
    ) else (
        set clobber=
        rem add user block to existing ub, if any
        if "%3"=="" (
            rem output goes to infile, compare ubfile to infile
            set ofile=
            set compare_test=%ifile%
            copy /y %ifile% xxofile.h5 > nul
            set compare_orig=xxofile.h5
            set cleanup=%cleanup% !compare_orig!
        ) else (
            rem output goes to %4, compare ofile to ubfile
            set ofile=%3
            set compare_test=!ofile!
            set compare_orig=%ifile%
        )
    )
    
    rem call 'jam' with the appropriate arguments
    if defined ofile (
        %jam_bin% -u %ufile% -i %ifile% -o %ofile% %clobber%
    ) else (
        %jam_bin% -u %ufile% -i %ifile% %clobber%
    )
    
    call :check_ub_1 %compare_test% %ufile% %compare_orig%
    
    if "%result1%"=="0" (
        if defined ofile (
            call :testing PASSED %jam% -u %ufile% -i %ifile% -o %ofile% %clobber%
        ) else (
            call :testing PASSED %jam% -u %ufile% -i %ifile% %clobber%
        )
    ) else ( 
        if defined ofile (
            call :testing *FAILED* %jam% -u %ufile% -i %ifile% -o %ofile% %clobber%
        ) else (
            call :testing *FAILED* %jam% -u %ufile% -i %ifile% %clobber%
        )
        set /a nerrors=%nerrors%+1
    )
    call :cleanup %cleanup%
    
    exit /b
    
    
rem UNJAMTEST  file.h5 [- | --delete] ofile
rem
rem  Test the 'unjam' tool
rem
rem ##fix the working directory here and in jamtest
:unjamtest
    set infile=%1
    set ofile=%3
    if "%2%"=="-" (
        set uofile=uofile
        %unjam_bin% -i %infile% -o %ofile% > !uofile!
    ) else if "%2"=="--delete" (
        set uofile=none
        %unjam_bin% -i %infile% -o %ofile% --delete
    ) else (
        set uofile=%2
        %unjam_bin% -i %infile% -u !uofile! -o %ofile%
    )
    
    set result1=0
    set result2=0
    set cleanup=
    if not "%uofile%"=="none" (
        rem sets results1
        call :check_ub_1 %infile% %uofile%
        call :cleanup %uofile%
    )
    
    rem sets result2
    call :check_noub %ofile%
    if "%result1% and %result2%"=="0 and 0" (
        if "%2%"=="-" (
            rem We use "ARROW_RIGHT" here and replace it in :testing because
            rem Windows interprets it as a pipe.  --SJW 8/27/07
            call :testing PASSED %unjam% -i %infile% -o %ofile% ARROW_RIGHT %uofile%
        ) else if "%2"=="--delete" (
            call :testing PASSED %unjam% -i %infile% -o %ofile% --delete
        ) else (
            call :testing PASSED %unjam% -i %infile% -u %uofile% -o %ofile%
        )
    ) else (
        if "%2%"=="-" (
            rem We use "ARROW_RIGHT" here and replace it in :testing because
            rem Windows interprets it as a pipe.  --SJW 8/27/07
            call :testing *FAILED* %unjam% -i %infile% -o %ofile% ARROW_RIGHT %uofile%
        ) else if "%2"=="--delete" (
            call :testing *FAILED* %unjam% -i %infile% -o %ofile% --delete
        ) else (
            call :testing *FAILED* %unjam% -i %infile% -u %uofile% -o %ofile%
        )
        set /a nerrors=%nerrors%+1
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


rem ############################################################################
rem ############################################################################
rem #                       T H E   T E S T S                                ###
rem ############################################################################
rem ############################################################################
:main
    call :jamtest %testfiles%\u10.txt %testfiles%\tall.h5 ta2.h5
    call :checkfile %testfiles%\tall.h5 ta2.h5
    call :cleanup ta2.h5
    call :jamtest %testfiles%\u511.txt %testfiles%\tall.h5 ta3.h5
    call :checkfile %testfiles%\tall.h5 ta3.h5
    call :cleanup ta3.h5
    call :jamtest %testfiles%\u512.txt %testfiles%\tall.h5 ta4.h5
    call :checkfile %testfiles%\tall.h5 ta4.h5
    call :cleanup ta4.h5
    call :jamtest %testfiles%\u513.txt %testfiles%\tall.h5 ta5.h5
    call :checkfile %testfiles%\tall.h5 ta5.h5
    call :cleanup ta5.h5

    call :setup %testfiles%\tall.h5 ta.h5
    call :jamtest %testfiles%\u10.txt ta.h5 
    call :checkfile %testfiles%\tall.h5 ta.h5
    call :setup %testfiles%\tall.h5 ta.h5
    call :jamtest %testfiles%\u511.txt ta.h5 
    call :checkfile %testfiles%\tall.h5 ta.h5
    call :setup %testfiles%\tall.h5 ta.h5
    call :jamtest %testfiles%\u512.txt ta.h5 
    call :checkfile %testfiles%\tall.h5 ta.h5
    call :setup %testfiles%\tall.h5 ta.h5
    call :jamtest %testfiles%\u513.txt ta.h5 
    call :checkfile %testfiles%\tall.h5 ta.h5
    call :cleanup ta.h5

    call :jamtest %testfiles%\u10.txt %testfiles%\twithub.h5 tax2.h5
    call :checkfile %testfiles%\tall.h5 tax2.h5
    call :cleanup tax2.h5
    call :jamtest %testfiles%\u511.txt %testfiles%\twithub.h5 tax3.h5
    call :checkfile %testfiles%\tall.h5 tax3.h5
    call :cleanup tax3.h5
    call :jamtest %testfiles%\u512.txt %testfiles%\twithub.h5 tax4.h5
    call :checkfile %testfiles%\tall.h5 tax4.h5
    call :cleanup tax4.h5
    call :jamtest %testfiles%\u513.txt %testfiles%\twithub.h5 tax5.h5
    call :checkfile %testfiles%\tall.h5 tax5.h5
    call :cleanup tax5.h5

    call :jamtest %testfiles%\u10.txt %testfiles%\twithub513.h5 tax6.h5
    call :checkfile %testfiles%\tall.h5 tax6.h5
    call :cleanup tax6.h5
    call :jamtest %testfiles%\u511.txt %testfiles%\twithub513.h5 tax7.h5
    call :checkfile %testfiles%\tall.h5 tax7.h5
    call :cleanup tax7.h5
    call :jamtest %testfiles%\u512.txt %testfiles%\twithub513.h5 tax8.h5
    call :checkfile %testfiles%\tall.h5 tax8.h5
    call :cleanup tax8.h5
    call :jamtest %testfiles%\u513.txt %testfiles%\twithub513.h5 tax9.h5
    call :checkfile %testfiles%\tall.h5 tax9.h5
    call :cleanup tax9.h5

    call :jamtest %testfiles%\u10.txt %testfiles%\twithub.h5 --clobber taz2.h5 
    call :checkfile %testfiles%\tall.h5 taz2.h5
    call :cleanup taz2.h5
    call :jamtest %testfiles%\u511.txt %testfiles%\twithub.h5 --clobber taz3.h5
    call :checkfile %testfiles%\tall.h5 taz3.h5
    call :cleanup taz3.h5
    call :jamtest %testfiles%\u512.txt %testfiles%\twithub.h5 --clobber taz4.h5 
    call :checkfile %testfiles%\tall.h5 taz4.h5
    call :cleanup taz4.h5
    call :jamtest %testfiles%\u513.txt %testfiles%\twithub.h5 --clobber taz5.h5 
    call :checkfile %testfiles%\tall.h5 taz5.h5
    call :cleanup taz5.h5

    call :jamtest %testfiles%\u10.txt %testfiles%\twithub513.h5 --clobber taz6.h5 
    call :checkfile %testfiles%\tall.h5 taz6.h5
    call :cleanup taz6.h5
    call :jamtest %testfiles%\u511.txt %testfiles%\twithub513.h5 --clobber taz7.h5
    call :checkfile %testfiles%\tall.h5 taz7.h5
    call :cleanup taz7.h5
    call :jamtest %testfiles%\u512.txt %testfiles%\twithub513.h5 --clobber taz8.h5 
    call :checkfile %testfiles%\tall.h5 taz8.h5
    call :cleanup taz8.h5
    call :jamtest %testfiles%\u513.txt %testfiles%\twithub513.h5 --clobber taz9.h5 
    call :checkfile %testfiles%\tall.h5 taz9.h5
    call :cleanup taz9.h5

    call :setup %testfiles%\twithub.h5 tay2.h5
    call :jamtest %testfiles%\u10.txt tay2.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay2.h5
    call :cleanup tay2.h5
    call :setup %testfiles%\twithub.h5 tay3.h5
    call :jamtest %testfiles%\u511.txt tay3.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay3.h5
    call :cleanup tay3.h5
    call :setup %testfiles%\twithub.h5 tay4.h5
    call :jamtest %testfiles%\u512.txt tay4.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay4.h5
    call :cleanup tay4.h5
    call :setup %testfiles%\twithub.h5 tay5.h5
    call :jamtest %testfiles%\u513.txt tay5.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay5.h5
    call :cleanup tay5.h5

    call :setup %testfiles%\twithub513.h5 tay6.h5
    call :jamtest %testfiles%\u10.txt tay6.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay6.h5
    call :cleanup tay6.h5
    call :setup %testfiles%\twithub513.h5 tay7.h5
    call :jamtest %testfiles%\u511.txt tay7.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay7.h5
    call :cleanup tay7.h5
    call :setup %testfiles%\twithub513.h5 tay8.h5
    call :jamtest %testfiles%\u512.txt tay8.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay8.h5
    call :cleanup tay8.h5
    call :setup %testfiles%\twithub513.h5 tay9.h5
    call :jamtest %testfiles%\u513.txt tay9.h5 --clobber
    call :checkfile %testfiles%\tall.h5 tay9.h5
    call :cleanup tay9.h5

    call :setup %testfiles%\twithub.h5 tai1.h5
    call :unjamtest tai1.h5 o10.txt taa1.h5
    call :checkfile %testfiles%\tall.h5 taa1.h5
    call :cleanup taa1.h5 tai1.h5 o10.txt
    call :setup %testfiles%\twithub513.h5 tai2.h5
    call :unjamtest tai2.h5 o512.txt taa2.h5
    call :checkfile %testfiles%\tall.h5 taa2.h5
    call :cleanup taa2.h5 tai2.h5 o512.txt

    call :setup %testfiles%\twithub.h5 tai3.h5
    call :unjamtest tai3.h5 - taa3.h5
    call :checkfile %testfiles%\tall.h5 taa3.h5
    call :cleanup taa3.h5 tai3.h5
    call :setup %testfiles%\twithub513.h5 tai4.h5
    call :unjamtest tai4.h5 - taa4.h5
    call :checkfile %testfiles%\tall.h5 taa4.h5
    call :cleanup taa4.h5 tai4.h5

    call :setup %testfiles%\twithub.h5 taj2.h5
    call :unjamtest taj2.h5 --delete tac2.h5
    call :checkfile %testfiles%\tall.h5 tac2.h5
    call :cleanup tac2.h5 taj2.h5
    call :setup %testfiles%\twithub513.h5 taj3.h5
    call :unjamtest taj3.h5 --delete tac3.h5
    call :checkfile %testfiles%\tall.h5 tac3.h5
    call :cleanup tac3.h5 taj3.h5

    if %nerrors% equ 0 (
        echo.All %jam% tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
