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
rem Tests for the h5diff tool
rem
rem    Created:  Scott Wegner, 8/22/07
rem    Modified: Allen Byrne, 2/23/10
rem

setlocal enabledelayedexpansion
pushd %~dp0

rem ############################################################################
rem test file names 
rem ############################################################################

set indir=%CD%\testfiles

set srcfile1=h5diff_basic1.h5
set srcfile2=h5diff_basic2.h5
set srcfile3=h5diff_types.h5
set srcfile4=h5diff_dtypes.h5
set srcfile5=h5diff_attr1.h5
set srcfile6=h5diff_attr2.h5
set srcfile7=h5diff_dset1.h5
set srcfile8=h5diff_dset2.h5
set srcfile9=h5diff_hyper1.h5
set srcfile10=h5diff_hyper2.h5
set srcfile11=h5diff_empty.h5
set srcfile12=h5diff_links.h5
set srcfile13=h5diff_softlinks.h5
set srcfile14=h5diff_linked_softlink.h5
set srcfile15=h5diff_extlink_src.h5
set srcfile16=h5diff_extlink_trg.h5
set srcfile17=h5diff_ext2softlink_src.h5
set srcfile18=h5diff_ext2softlink_trg.h5
set srclnkfile1=h5diff_danglelinks1.h5
set srclnkfile2=h5diff_danglelinks2.h5
set src_grp_recurse1=h5diff_grp_recurse1.h5
set src_grp_recurse2=h5diff_grp_recurse2.h5
set src_grp_recurse1_ext=h5diff_grp_recurse_ext1.h5
set src_grp_recurse2_ext1=h5diff_grp_recurse_ext2-1.h5
set src_grp_recurse2_ext2=h5diff_grp_recurse_ext2-2.h5
set src_grp_recurse2_ext3=h5diff_grp_recurse_ext2-3.h5
set srcexclude1_1=h5diff_exclude1-1.h5
set srcexclude1_2=h5diff_exclude1-2.h5
set srcexclude2_1=h5diff_exclude2-1.h5
set srcexclude2_2=h5diff_exclude2-2.h5
set src_comp_vl_strs=h5diff_comp_vl_strs.h5
set src_ATTR_VERBOSE_LEVEL_FILE1=h5diff_attr_v_level1.h5
set src_ATTR_VERBOSE_LEVEL_FILE2=h5diff_attr_v_level2.h5

set file1=%indir%\h5diff_basic1.h5
set file2=%indir%\h5diff_basic2.h5
set file3=%indir%\h5diff_types.h5
set file4=%indir%\h5diff_dtypes.h5
set file5=%indir%\h5diff_attr1.h5
set file6=%indir%\h5diff_attr2.h5
set file7=%indir%\h5diff_dset1.h5
set file8=%indir%\h5diff_dset2.h5
set file9=%indir%\h5diff_hyper1.h5
set file10=%indir%\h5diff_hyper2.h5
set file11=%indir%\h5diff_empty.h5
set file12=%indir%\h5diff_links.h5
set file13=%indir%\h5diff_softlinks.h5
set file14=%indir%\h5diff_linked_softlink.h5
set file15=%indir%\h5diff_extlink_src.h5
set file16=%indir%\h5diff_extlink_trg.h5
set file17=%indir%\h5diff_ext2softlink_src.h5
set file18=%indir%\h5diff_ext2softlink_trg.h5
set lnkfile1=%indir%\h5diff_danglelinks1.h5
set lnkfile2=%indir%\h5diff_danglelinks2.h5
set grp_recurse1=%indir%\h5diff_grp_recurse1.h5
set grp_recurse2=%indir%\h5diff_grp_recurse2.h5
set grp_recurse1_ext=%indir%\h5diff_grp_recurse_ext1.h5
set grp_recurse2_ext1=%indir%\h5diff_grp_recurse_ext2-1.h5
set grp_recurse2_ext2=%indir%\h5diff_grp_recurse_ext2-2.h5
set grp_recurse2_ext3=%indir%\h5diff_grp_recurse_ext2-3.h5
set exclude1_1=%indir%\h5diff_exclude1-1.h5
set exclude1_2=%indir%\h5diff_exclude1-2.h5
set exclude2_1=%indir%\h5diff_exclude2-1.h5
set exclude2_2=%indir%\h5diff_exclude2-2.h5
set comp_vl_strs=%indir%\h5diff_comp_vl_strs.h5
set ATTR_VERBOSE_LEVEL_FILE1=%indir%\h5diff_attr_v_level1.h5
set ATTR_VERBOSE_LEVEL_FILE2=%indir%\h5diff_attr_v_level2.h5


rem The tool name
set h5diff=h5diff%2
rem The path of the tool binary
set h5diff_bin=%CD%\..\%h5diff%\%1\%h5diff%

set EXIT_SUCCESS=0
set EXIT_FAILURE=1

set /a nerrors=0
set verbose=yes
rem default to run h5diff tests
set pmode=
rem following not needed for windows see #10 ADB 1/22/2009
rem mydomainname=`domainname 2>/dev/null`

if not exist .\testfiles mkdir .\testfiles

rem Parse options
rem On Windows, we don't parse, because we only want to worry about
rem debug/release and dll  --SJW 9/5/07

goto main


rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Testing".
rem On Windows, simply set up the test_msg variable, so it can be printed later
rem with the :results function.  This is because Windows doesn't support
rem printing without a linefeed.  --SJW 6/20/08
rem
:testing
    set test_msg=Testing
    for %%a in (%*) do (
            set test_msg=!test_msg! %%~nxa
    )
    set test_msg=%test_msg%                                                                
    set test_msg=%test_msg:~0,69%
    
    exit /b


rem Print the testing results.  Simply echo the contents of test_msg (set up
rem above), along with the passed parameter, generall PASSED, FAILED, or -SKIP-
:results
    echo.%test_msg% %*
    
    exit /b
    
    
    
rem Function STDOUT_FILTER isn't technically needed on Windows, because this
rem script will never run on platforms that require it.  However, include empty
rem interface for consistency.  --SJW 8/22/07
:stdout_filter
    exit /b
    

rem Function STDERR_FILTER isn't technically needed on Windows, because this
rem script will never run on platforms that require it.  However, include empty
rem interface for consistency.  --SJW 8/22/07
:stderr_filter
    exit /b
    
    
    
rem Run a test and print PASS or *FAIL*.  If a test fails then increment
rem the `nerrors' global variable and (if verbose is set) display the
rem difference between the actual output and the expected output. The
rem expected output is given as the first argument to this function and
rem the actual output file is calculated by replacing the `.ddl' with
rem `.out'.  The actual output is not removed if HDF5_NOCLEANUP has a
rem non-zero value.
rem
:tooltest
    set expect=%CD%\testfiles\%1
    set actual=%CD%\testfiles\%~n1.out
    set actual_err=%CD%\testfiles\~n1.err
    set actual_sav=%actual%-sav
    set actual_err_sav=%actual_err%-sav
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/22/07
    set params=
    for /f "tokens=2*" %%a in ("%*") do (
        if "%%b"=="" (
            set params=%%a
        ) else (
            set params=%%a %%b
        )
    )
    rem Parallel mode not actually supported, but included for consistency.
    if defined pmode (
        rem do nothing
    )
    
    rem Run test.
    (
        rem echo.#############################
        rem rem Remove quotes here, because Linux 'echo' command strips them
        rem echo.Expected output for 'h5diff %params:"=%'
        rem echo.#############################
        pushd testfiles
        %h5diff_bin% %params%
        popd
    ) > %actual% 2> %actual_err%
	set EXIT_CODE=!errorlevel!
    rem save actual and actual_err in case they are needed later.
    copy /y %actual% %actual_sav% > nul
    call :stdout_filter %actual%
    copy /y %actual_err% %actual_err_sav% > nul
    call :stderr_filter %actual_err%
    type %actual_err% >> %actual%
    echo EXIT CODE: !EXIT_CODE! >> %actual%
    
    if not exist %expect% (
        rem Create the expect file if it doesn't yet exist.
        call :results CREATED
        copy /y %actual% %expect% > nul
    ) else (
        fc /w %expect% %actual% > nul
        if !errorlevel! equ 0 (
            call :results PASSED
        ) else (
            call :results *FAILED*
            echo.    Expected result ^(%expect%^) differs from actual result ^(%actual%^)
            set /a nerrors=!nerrors!+1
            if "yes"=="%verbose%" fc /w %actual% %expect%
        )
    )
        
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_err% %actual_sav% %actual_err_sav%
    )
    
    exit /b
    
    
rem Print a "SKIP" message
:skip
    call :testing -SKIP- %h5diff% %*
    
    exit /b
    
    
:main
rem ############################################################################
rem  The tests 
rem  To avoid the printing of the complete full path of the test file, that hides
rem  all the other parameters for long paths, the printing of the command line 
rem  is done first in
rem  TESTING with the name only of the test file $TOOL, not its full path $TESTFILE
rem ############################################################################

rem ############################################################################
rem # Common usage
rem ############################################################################


    rem 1.0
    call :testing %h5diff% -h
    call :tooltest h5diff_10.txt -h

    rem 1.1 normal mode
    call :testing %h5diff% %srcfile1% %srcfile2%
    call :tooltest h5diff_11.txt  %file1% %file2% 

    rem 1.2 normal mode with objects
    call :testing %h5diff% %srcfile1% %srcfile2% g1/dset1 g1/dset2
    call :tooltest h5diff_12.txt  %file1% %file2%  g1/dset1 g1/dset2

    rem 1.3 report mode
    call :testing %h5diff% -r %srcfile1% %srcfile2%
    call :tooltest h5diff_13.txt -r %file1% %file2%

    rem 1.4 report  mode with objects
    call :testing %h5diff% -r %srcfile1% %srcfile2% g1/dset1 g1/dset2
    call :tooltest h5diff_14.txt -r %file1% %file2% g1/dset1 g1/dset2

    rem 1.5 with -d
    call :testing %h5diff% --report --delta=5 %srcfile1% %srcfile2% g1/dset3 g1/dset4
    call :tooltest h5diff_15.txt --report --delta=5 %file1% %file2% g1/dset3 g1/dset4

    rem 1.6.1 with -p (int)
    call :testing %h5diff% -v -p 0.02 %srcfile1% %srcfile1% g1/dset5 g1/dset6
    call :tooltest h5diff_16_1.txt -v -p 0.02 %file1% %file1% g1/dset5 g1/dset6

    rem 1.6.2 with -p (unsigned long_long)
    call :testing %h5diff% --verbose --relative=0.02 %srcfile1% %srcfile1% g1/dset7 g1/dset8
    call :tooltest h5diff_16_2.txt --verbose --relative=0.02 %file1% %file1% g1/dset7 g1/dset8

    rem 1.6.3 with -p (double)
    call :testing %h5diff% -v -p 0.02 %srcfile1% %srcfile1% g1/dset9 g1/dset10
    call :tooltest h5diff_16_3.txt -v -p 0.02 %file1% %file1% g1/dset9 g1/dset10

    rem 1.7 verbose mode
    call :testing %h5diff% -v %srcfile1% %srcfile2%
    call :tooltest h5diff_17.txt -v %file1% %file2% 

    rem 1.71 test 32-bit INFINITY
    call :testing %h5diff% -v %srcfile1% %srcfile1% /g1/fp19
    call :tooltest h5diff_171.txt -v %file1% %file1% /g1/fp19 

    rem 1.72 test 64-bit INFINITY
    call :testing %h5diff% -v %srcfile1% %srcfile1% /g1/fp20
    call :tooltest h5diff_172.txt -v %file1% %file1% /g1/fp20 

    rem 1.8 quiet mode 
    call :testing %h5diff% -q %srcfile1% %srcfile2%
    call :tooltest h5diff_18.txt -q %file1% %file2% 
    
    rem ########################################################################
    rem # not comparable types
    rem ########################################################################

    rem 2.0
    call :testing %h5diff% -v %srcfile3% %srcfile3% dset g1
    call :tooltest h5diff_20.txt -v %file3% %file3% dset g1

    rem 2.1
    call :testing %h5diff% -v %srcfile3% %srcfile3% dset l1
    call :tooltest h5diff_21.txt -v %file3% %file3% dset l1

    rem 2.2
    call :testing %h5diff% -v %srcfile3% %srcfile3% dset t1
    call :tooltest h5diff_22.txt -v %file3% %file3% dset t1

    rem #######################################################################
    rem # compare groups, types, links (no differences and differences)
    rem #######################################################################

    rem 2.3
    call :testing %h5diff%  -v %srcfile3% %srcfile3% g1 g1
    call :tooltest h5diff_23.txt -v %file3% %file3% g1 g1

    rem 2.4
    call :testing %h5diff% -v  %srcfile3% %srcfile3% t1 t1
    call :tooltest h5diff_24.txt -v %file3% %file3% t1 t1

    rem 2.5
    call :testing %h5diff% -v  %srcfile3% %srcfile3% l1 l1 
    call :tooltest h5diff_25.txt -v %file3% %file3% l1 l1 

    rem 2.6
    call :testing %h5diff% -v %srcfile3% %srcfile3% g1 g2
    call :tooltest h5diff_26.txt -v %file3% %file3% g1 g2

    rem 2.7
    call :testing %h5diff% -v %srcfile3% %srcfile3% t1 t2
    call :tooltest h5diff_27.txt -v %file3% %file3% t1 t2

    rem 2.8
    call :testing %h5diff% -v %srcfile3% %srcfile3% l1 l2
    call :tooltest h5diff_28.txt -v %file3% %file3% l1 l2



    rem ########################################################################
    rem # Dataset datatypes
    rem ########################################################################

    rem 5.0
    call :testing %h5diff% -v %srcfile4% %srcfile4% dset0a dset0b
    call :tooltest h5diff_50.txt -v %file4% %file4% dset0a dset0b

    rem 5.1
    call :testing %h5diff% -v %srcfile4% %srcfile4%  dset1a dset1b
    call :tooltest h5diff_51.txt -v %file4% %file4% dset1a dset1b

    rem 5.2
    call :testing %h5diff% -v %srcfile4% %srcfile4%  dset2a dset2b
    call :tooltest h5diff_52.txt -v %file4% %file4% dset2a dset2b

    rem 5.3
    call :testing %h5diff% -v %srcfile4% %srcfile4%  dset3a dset4b
    call :tooltest h5diff_53.txt -v %file4% %file4% dset3a dset4b

    rem 5.4
    call :testing %h5diff% -v %srcfile4% %srcfile4%  dset4a dset4b
    call :tooltest h5diff_54.txt -v %file4% %file4% dset4a dset4b

    rem 5.5
    call :testing %h5diff% -v %srcfile4% %srcfile4%  dset5a dset5b
    call :tooltest h5diff_55.txt -v %file4% %file4% dset5a dset5b

    rem 5.6
    call :testing %h5diff% -v %srcfile4% %srcfile4%  dset6a dset6b
    call :tooltest h5diff_56.txt -v %file4% %file4% dset6a dset6b

    rem 5.7
    call :testing %h5diff% -v %srcfile4% %srcfile4%  dset7a dset7b
    call :tooltest h5diff_57.txt -v %file4% %file4% dset7a dset7b

    rem 5.8 (region reference)
    call :testing %h5diff% -v %srcfile7% %srcfile8%  refreg
    call :tooltest h5diff_58.txt -v %file7% %file8% refreg

    rem ########################################################################
    rem # Error messages
    rem ########################################################################


    rem 6.0: Check if the command line number of arguments is less than 3
    call :testing %h5diff% %srcfile1%
    call :tooltest h5diff_600.txt %file1% 

    rem 6.1: Check if non-exist object name is specified 
    call :testing %h5diff% %srcfile1% %srcfile1% nono_obj
    rem SKIP this test as on Wondows legacy specific 
    rem call :tooltest h5diff_601.txt %file1% %file1% nono_obj
    call :results -SKIP-


    rem ########################################################################
    rem # -d 
    rem ########################################################################


    rem 6.3: negative value
    call :testing %h5diff%  -d -4 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_603.txt -d -4 %file1% %file2% g1/dset3 g1/dset4

    rem 6.4: zero
    call :testing %h5diff%  -d 0 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_604.txt -d 0 %file1% %file2% g1/dset3 g1/dset4

    rem 6.5: non number
    call :testing %h5diff% -d u %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_605.txt -d u %file1% %file2% g1/dset3 g1/dset4

    rem 6.6: hexadecimal
    call :testing %h5diff% -d 0x1 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_606.txt -d 0x1 %file1% %file2% g1/dset3 g1/dset4

    rem 6.7: string
    call :testing %h5diff% -d "1" %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_607.txt -d "1" %file1% %file2%  g1/dset3 g1/dset4

    rem 6.8: repeated option
    call :testing %h5diff% --use-system-epsilon %srcfile1% %srcfile2%   g1/dset3 g1/dset4
    call :tooltest h5diff_608.txt --use-system-epsilon %file1% %file2% g1/dset3 g1/dset4

    rem 6.9: number larger than biggest difference
    call :testing %h5diff% -d 200 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_609.txt -d 200 %file1% %file2%  g1/dset3 g1/dset4

    rem 6.10: number smaller than smallest difference
    call :testing %h5diff% -d 1 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_610.txt -d 1 %file1% %file2%  g1/dset3 g1/dset4


    rem ########################################################################
    rem # -p
    rem ########################################################################



    rem 6.12: negative value
    call :testing %h5diff% -p -4 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_612.txt -p -4 %file1% %file2% g1/dset3 g1/dset4

    rem 6.13: zero
    call :testing %h5diff% -p 0 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_613.txt -p 0 %file1% %file2% g1/dset3 g1/dset4

    rem 6.14: non number
    call :testing %h5diff% -p u %srcfile1% %srcfile2%   g1/dset3 g1/dset4
    call :tooltest h5diff_614.txt -p u %file1% %file2% g1/dset3 g1/dset4

    rem 6.15: hexadecimal
    call :testing %h5diff% -p 0x1 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_615.txt -p 0x1 %file1% %file2% g1/dset3 g1/dset4

    rem 6.16: string
    call :testing %h5diff% -p "0.21" %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_616.txt -p "0.21" %file1% %file2% g1/dset3 g1/dset4

    rem 6.17: repeated option
    call :testing %h5diff% -p 0.21 -p 0.22 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_617.txt -p 0.21 -p 0.22 %file1% %file2% g1/dset3 g1/dset4

    rem 6.18: number larger than biggest difference
    call :testing %h5diff% -p 2 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_618.txt -p 2 %file1% %file2% g1/dset3 g1/dset4

    rem 6.19: number smaller than smallest difference
    call :testing %h5diff% -p 0.005 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_619.txt -p 0.005 %file1% %file2% g1/dset3 g1/dset4


    rem ########################################################################
    rem # -n
    rem ########################################################################

    rem 6.21: negative value
    call :testing %h5diff% -n -4 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_621.txt -n -4 %file1% %file2% g1/dset3 g1/dset4

    rem 6.22: zero
    call :testing %h5diff% -n 0 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_622.txt -n 0 %file1% %file2% g1/dset3 g1/dset4

    rem 6.23: non number
    call :testing %h5diff% -n u %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_623.txt -n u %file1% %file2% g1/dset3 g1/dset4

    rem 6.24: hexadecimal
    call :testing %h5diff% -n 0x1 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_624.txt -n 0x1 %file1% %file2% g1/dset3 g1/dset4

    rem 6.25: string
    call :testing %h5diff% -n "2" %srcfile1% %srcfile2%   g1/dset3 g1/dset4
    call :tooltest h5diff_625.txt -n "2" %file1% %file2% g1/dset3 g1/dset4

    rem 6.26: repeated option
    call :testing %h5diff% -n 2 -n 3 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_626.txt -n 2 -n 3 %file1% %file2% g1/dset3 g1/dset4

    rem 6.27: number larger than biggest difference
    call :testing %h5diff% --count=200 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_627.txt --count=200 %file1% %file2% g1/dset3 g1/dset4

    rem 6.28: number smaller than smallest difference
    call :testing %h5diff% -n 1 %srcfile1% %srcfile2%  g1/dset3 g1/dset4
    call :tooltest h5diff_628.txt -n 1 %file1% %file2% g1/dset3 g1/dset4

	rem This is disabled on *nix platforms
    rem 6.29  non valid files
    call :testing %h5diff% file1.h6 file2.h6
    call :tooltest h5diff_629.txt file1.h6 file2.h6

    rem ########################################################################
    rem 7.  attributes
    rem ########################################################################
    call :testing %h5diff% -v  %srcfile5% %srcfile6%
    call :tooltest h5diff_70.txt -v %file5% %file6%

    rem ##################################################
    rem  attrs with verbose option level
    rem ##################################################
    call :testing %h5diff% -v1 %srcfile5% %srcfile6%
    call :tooltest h5diff_700.txt -v1 %file5% %file6%

    call :testing %h5diff% -v2 %srcfile5% %srcfile6%
    call :tooltest h5diff_701.txt -v2 %file5% %file6% 

    call :testing %h5diff%  --verbose=1 %srcfile5% %srcfile6%
    call :tooltest h5diff_702.txt --verbose=1 %file5% %file6% 

    call :testing %h5diff% --verbose=2 %srcfile5% %srcfile6%
    call :tooltest h5diff_703.txt --verbose=2 %file5% %file6% 

    rem same attr number , all same attr name
    call :testing %h5diff% -v2 %src_ATTR_VERBOSE_LEVEL_FILE1% %src_ATTR_VERBOSE_LEVEL_FILE2% /g
    call :tooltest h5diff_704.txt -v2 %ATTR_VERBOSE_LEVEL_FILE1% %ATTR_VERBOSE_LEVEL_FILE2% /g

    rem same attr number , some same attr name
    call :testing %h5diff%  -v2 %src_ATTR_VERBOSE_LEVEL_FILE1% %src_ATTR_VERBOSE_LEVEL_FILE2% /dset
    call :tooltest h5diff_705.txt -v2 %ATTR_VERBOSE_LEVEL_FILE1% %ATTR_VERBOSE_LEVEL_FILE2% /dset

    rem same attr number , all different attr name
    call :testing %h5diff% -v2 %src_ATTR_VERBOSE_LEVEL_FILE1% %src_ATTR_VERBOSE_LEVEL_FILE2% /ntype
    call :tooltest h5diff_706.txt -v2 %ATTR_VERBOSE_LEVEL_FILE1% %ATTR_VERBOSE_LEVEL_FILE2% /ntype

    rem different attr number , same attr name (intersected)
    call :testing %h5diff% -v2 %src_ATTR_VERBOSE_LEVEL_FILE1% %src_ATTR_VERBOSE_LEVEL_FILE2% /g2
    call :tooltest h5diff_707.txt -v2 %ATTR_VERBOSE_LEVEL_FILE1% %ATTR_VERBOSE_LEVEL_FILE2% /g2

    rem different attr number , all different attr name 
    call :testing %h5diff% -v2 %src_ATTR_VERBOSE_LEVEL_FILE1% %src_ATTR_VERBOSE_LEVEL_FILE2% /g3
    call :tooltest h5diff_708.txt -v2 %ATTR_VERBOSE_LEVEL_FILE1% %ATTR_VERBOSE_LEVEL_FILE2% /g3

    rem when no attributes exist in both objects
    call :testing %h5diff% -v2 %src_ATTR_VERBOSE_LEVEL_FILE1% %src_ATTR_VERBOSE_LEVEL_FILE2% /g4
    call :tooltest h5diff_709.txt -v2 %ATTR_VERBOSE_LEVEL_FILE1% %ATTR_VERBOSE_LEVEL_FILE2% /g4

    rem file vs file
    call :testing %h5diff% -v2 %src_ATTR_VERBOSE_LEVEL_FILE1% %src_ATTR_VERBOSE_LEVEL_FILE2%
    call :tooltest h5diff_710.txt -v2 %ATTR_VERBOSE_LEVEL_FILE1% %ATTR_VERBOSE_LEVEL_FILE2%


    rem #######################################################################
    rem 8.  all dataset datatypes
    rem #######################################################################
    call :testing %h5diff% -v  %srcfile7% %srcfile8%
    call :tooltest h5diff_80.txt -v %file7% %file8%

    rem 9. compare a file with itself
    call :testing %h5diff% -v  %srcfile2% %srcfile2%
    call :tooltest h5diff_90.txt -v %file2% %file2%

    rem 10. read by hyperslab, print indexes
    rem #######################################################################
    rem   Not skipped on windows as this has not been a problem - ADB 1/22/2009
    rem    if test -n "$pmode" -a "$mydomainname" = hdfgroup.uiuc.edu; then
    rem    # skip this test which sometimes hangs in some THG machines
    rem    SKIP -v $SRCFILE9 $SRCFILE10
    rem    else
    rem #######################################################################
    call :testing %h5diff% -v %srcfile9% %srcfile10%
    call :tooltest h5diff_100.txt -v %file9% %file10%
    rem    fi

    rem 11. floating point comparison
    rem Not tested on Windows due to difference in formatting of scientific 
    rem notation (101, 102)  --SJW 8/23/07
    call :testing %h5diff% -v %srcfile1% %srcfile1% g1/d1  g1/d2
    rem call :tooltest h5diff_101.txt -v %file1% %file1% g1/d1  g1/d2
    call :results -SKIP-

    call :testing %h5diff% -v  %srcfile1% %srcfile1%  g1/fp1 g1/fp2
    rem call :tooltest h5diff_102.txt -v %file1% %file1% g1/fp1 g1/fp2
    call :results -SKIP-

    rem Not tested on Windows due to difference in formatting of scientific 
    rem notation with other OS. printf("%g") (103, 104)  
    call :testing %h5diff% -v --use-system-epsilon %srcfile1% %srcfile1% g1/d1  g1/d2
    rem call :tooltest h5diff_103.txt -v --use-system-epsilon %file1% %file1% g1/d1  g1/d2
    call :results -SKIP-

    call :testing %h5diff% -v --use-system-epsilon %srcfile1% %srcfile1%  g1/fp1 g1/fp2
    rem call :tooltest h5diff_102.txt -v --use-system-epsilon %file1% %file1% g1/fp1 g1/fp2
    call :results -SKIP-

    rem   New option added #1368(E1)  - ADB 2/5/2009
	rem not compable -c flag
	call :testing %h5diff% %srcfile2% %srcfile2% g2/dset1  g2/dset2
    call :tooltest h5diff_200.txt %file2% %file2% g2/dset1  g2/dset2 

	call :testing %h5diff% -c %srcfile2% %srcfile2% g2/dset1  g2/dset2
    call :tooltest h5diff_201.txt -c %file2% %file2% g2/dset1  g2/dset2 

	call :testing %h5diff% -c %srcfile2% %srcfile2% g2/dset2  g2/dset3
    call :tooltest h5diff_202.txt -c %file2% %file2% g2/dset2  g2/dset3

	call :testing %h5diff% -c %srcfile2% %srcfile2% g2/dset3  g2/dset4
    call :tooltest h5diff_203.txt -c %file2% %file2% g2/dset3  g2/dset4

	call :testing %h5diff% -c %srcfile2% %srcfile2% g2/dset4  g2/dset5
    call :tooltest h5diff_204.txt -c %file2% %file2% g2/dset4  g2/dset5

	call :testing %h5diff% -c %srcfile2% %srcfile2% g2/dset5  g2/dset6
    call :tooltest h5diff_205.txt -c %file2% %file2% g2/dset5  g2/dset6
	
    rem   New option added - ADB 2/11/2009
	rem # not comparable in compound
	call :testing %h5diff% -c %srcfile2% %srcfile2% g2/dset7  g2/dset8
    call :tooltest h5diff_206.txt -c %file2% %file2% g2/dset7  g2/dset8

	call :testing %h5diff% -c %srcfile2% %srcfile2% g2/dset8  g2/dset9
    call :tooltest h5diff_207.txt -c %file2% %file2% g2/dset8  g2/dset9

    rem #######################################################################
    rem # Links compare without --follow-symlinks nor --no-dangling-links
    rem #######################################################################
    rem test for bug1749
	call :testing %h5diff% -v %srcfile12% %srcfile12% /link_g1 /link_g2
    call :tooltest h5diff_300.txt -v %file12% %file12% /link_g1 /link_g2

    rem #######################################################################
    rem # Links compare with --follow-symlinks Only
    rem #######################################################################
    rem soft links file to file
	call :testing %h5diff% --follow-symlinks -v  %srcfile13% %srcfile13%
    call :tooltest h5diff_400.txt --follow-symlinks -v %file13% %file13% 

    rem softlink vs dset"
	call :testing %h5diff% --follow-symlinks -v %srcfile13% %srcfile13% /softlink_dset1_1 /target_dset2
    call :tooltest h5diff_401.txt --follow-symlinks -v %file13% %file13% /softlink_dset1_1 /target_dset2

    rem dset vs softlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile13% %srcfile13% /target_dset2 /softlink_dset1_1
    call :tooltest h5diff_402.txt --follow-symlinks -v %file13% %file13% /target_dset2 /softlink_dset1_1

    rem softlink vs softlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile13% %srcfile13% /softlink_dset1_1 /softlink_dset2
    call :tooltest h5diff_403.txt --follow-symlinks -v %file13% %file13% /softlink_dset1_1 /softlink_dset2

    rem extlink vs extlink (FILE)"
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile15%
    call :tooltest h5diff_404.txt --follow-symlinks -v %file15% %file15%

    rem extlink vs dset"
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile16% /ext_link_dset1 /target_group2/x_dset
    call :tooltest h5diff_405.txt --follow-symlinks -v %file15% %file16% /ext_link_dset1 /target_group2/x_dset

    rem dset vs extlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile16% %srcfile15% /target_group2/x_dset /ext_link_dset1
    call :tooltest h5diff_406.txt --follow-symlinks -v %file16% %file15% /target_group2/x_dset /ext_link_dset1

    rem extlink vs extlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile15% /ext_link_dset1 /ext_link_dset2
    call :tooltest h5diff_407.txt --follow-symlinks -v %file15% %file15% /ext_link_dset1 /ext_link_dset2

    rem softlink vs extlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile13% %srcfile15% /softlink_dset1_1 /ext_link_dset2
    call :tooltest h5diff_408.txt --follow-symlinks -v %file13% %file15% /softlink_dset1_1 /ext_link_dset2

    rem extlink vs softlink "
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile13% /ext_link_dset2 /softlink_dset1_1
    call :tooltest h5diff_409.txt --follow-symlinks -v %file15% %file13% /ext_link_dset2 /softlink_dset1_1

    rem linked_softlink vs linked_softlink (FILE)"
	call :testing %h5diff% --follow-symlinks -v %srcfile14% %srcfile14%
    call :tooltest h5diff_410.txt --follow-symlinks -v %file14% %file14%

    rem dset2 vs linked_softlink_dset1"
	call :testing %h5diff% --follow-symlinks -v %srcfile14% %srcfile14% /target_dset2 /softlink1_to_slink2
    call :tooltest h5diff_411.txt --follow-symlinks -v %file14% %file14% /target_dset2 /softlink1_to_slink2

    rem    rem linked_softlink_dset1 vs dset2"
	call :testing %h5diff% --follow-symlinks -v %srcfile14% %srcfile14% /softlink1_to_slink2 /target_dset2
    call :tooltest h5diff_412.txt --follow-symlinks -v %file14% %file14% /softlink1_to_slink2 /target_dset2

    rem linked_softlink_to_dset1 vs linked_softlink_to_dset2"
	call :testing %h5diff% --follow-symlinks -v %srcfile14% %srcfile14% /softlink1_to_slink2 /softlink2_to_slink2
    call :tooltest h5diff_413.txt --follow-symlinks -v %file14% %file14% /softlink1_to_slink2 /softlink2_to_slink2

    rem group vs linked_softlink_group1"
	call :testing %h5diff% --follow-symlinks -v %srcfile14% %srcfile14% /target_group /softlink3_to_slink2
    call :tooltest h5diff_414.txt --follow-symlinks -v %file14% %file14% /target_group /softlink3_to_slink2

    rem linked_softlink_group1 vs group"
	call :testing %h5diff% --follow-symlinks -v %srcfile14% %srcfile14% /softlink3_to_slink2 /target_group
    call :tooltest h5diff_415.txt --follow-symlinks -v %file14% %file14% /softlink3_to_slink2 /target_group

    rem linked_softlink_to_group1 vs linked_softlink_to_group2"
	call :testing %h5diff% --follow-symlinks -v %srcfile14% %srcfile14% /softlink3_to_slink2 /softlink4_to_slink2
    call :tooltest h5diff_416.txt --follow-symlinks -v %file14% %file14% /softlink3_to_slink2 /softlink4_to_slink2

    rem non-exist-softlink vs softlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile13% %srcfile13% /softlink_noexist /softlink_dset2
    call :tooltest h5diff_417.txt --follow-symlinks -v %file13% %file13% /softlink_noexist /softlink_dset2

    rem softlink vs non-exist-softlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile13% %srcfile13% /softlink_dset2 /softlink_noexist
    call :tooltest h5diff_418.txt --follow-symlinks -v %file13% %file13% /softlink_dset2 /softlink_noexist

    rem non-exist-extlink_file vs extlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile15% /ext_link_noexist2 /ext_link_dset2
    call :tooltest h5diff_419.txt --follow-symlinks -v %file15% %file15% /ext_link_noexist2 /ext_link_dset2

    rem exlink vs non-exist-extlink_file"
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile15% /ext_link_dset2 /ext_link_noexist2
    call :tooltest h5diff_420.txt --follow-symlinks -v %file15% %file15% /ext_link_dset2 /ext_link_noexist2

    rem extlink vs non-exist-extlink_obj"
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile15% /ext_link_dset2 /ext_link_noexist1
    call :tooltest h5diff_421.txt --follow-symlinks -v %file15% %file15% /ext_link_dset2 /ext_link_noexist1

    rem non-exist-extlink_obj vs extlink"
	call :testing %h5diff% --follow-symlinks -v %srcfile15% %srcfile15% /ext_link_noexist1 /ext_link_dset2
    call :tooltest h5diff_422.txt --follow-symlinks -v %file15% %file15% /ext_link_noexist1 /ext_link_dset2

    rem extlink_to_softlink_to_dset1 vs dset2"
	call :testing %h5diff% --follow-symlinks -v %srcfile17% %srcfile18% /ext_link_to_slink1 /dset2
    call :tooltest h5diff_423.txt --follow-symlinks -v %file17% %file18% /ext_link_to_slink1 /dset2

    rem dset2 vs extlink_to_softlink_to_dset1"
	call :testing %h5diff% --follow-symlinks -v %srcfile18% %srcfile17% /dset2 /ext_link_to_slink1
    call :tooltest h5diff_424.txt --follow-symlinks -v %file18% %file17% /dset2 /ext_link_to_slink1

    rem extlink_to_softlink_to_dset1 vs extlink_to_softlink_to_dset2"
	call :testing %h5diff% --follow-symlinks -v %srcfile17% %srcfile17% /ext_link_to_slink1 /ext_link_to_slink2
    call :tooltest h5diff_425.txt --follow-symlinks -v %file17% %file17% /ext_link_to_slink1 /ext_link_to_slink2


    rem #######################################################################
    rem # Dangling links compare (--follow-symlinks and --no-dangling-links)
    rem #######################################################################
    rem dangling links --follow-symlinks (FILE to FILE)
	call :testing %h5diff% --follow-symlinks -v %srclnkfile1% %srclnkfile2%
    call :tooltest h5diff_450.txt --follow-symlinks -v %lnkfile1% %lnkfile2%

    rem dangling links --follow-symlinks and --no-dangling-links (FILE to FILE)
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links  %srclnkfile1% %srclnkfile2%
    call :tooltest h5diff_451.txt --follow-symlinks -v --no-dangling-links  %lnkfile1% %lnkfile2% 

    rem try --no-dangling-links without --follow-symlinks options
	call :testing %h5diff%  --no-dangling-links %srcfile13% %srcfile13% 
    call :tooltest h5diff_452.txt  --no-dangling-links %file13% %file13% 

    rem dangling link found for soft links (FILE to FILE)
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links %srcfile13% %srcfile13%
    call :tooltest h5diff_453.txt --follow-symlinks -v --no-dangling-links %file13% %file13% 

    rem dangling link found for soft links (obj to obj)
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links %srcfile13% %srcfile13% /softlink_dset2 /softlink_noexist
    call :tooltest h5diff_454.txt --follow-symlinks -v --no-dangling-links %file13% %file13% /softlink_dset2 /softlink_noexist

    rem dangling link found for soft links (obj to obj) Both dangle links
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links %srcfile13% %srcfile13% /softlink_noexist /softlink_noexist
    call :tooltest h5diff_455.txt --follow-symlinks -v --no-dangling-links %file13% %file13% /softlink_noexist /softlink_noexist

    rem dangling link found for ext links (FILE to FILE)
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links %srcfile15% %srcfile15%
    call :tooltest h5diff_456.txt --follow-symlinks -v --no-dangling-links %file15% %file15%

    rem dangling link found for ext links (obj to obj). target file exist
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links %srcfile15% %srcfile15% /ext_link_dset1 /ext_link_noexist1
    call :tooltest h5diff_457.txt --follow-symlinks -v --no-dangling-links %file15% %file15% /ext_link_dset1 /ext_link_noexist1

    rem dangling link found for ext links (obj to obj). target file NOT exist
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links %srcfile15% %srcfile15% /ext_link_dset1 /ext_link_noexist2
    call :tooltest h5diff_458.txt --follow-symlinks -v --no-dangling-links %file15% %file15% /ext_link_dset1 /ext_link_noexist2

    rem dangling link found for ext links (obj to obj). Both dangle links
	call :testing %h5diff% --follow-symlinks -v --no-dangling-links %srcfile15% %srcfile15% /ext_link_noexist1 /ext_link_noexist2
    call :tooltest h5diff_459.txt --follow-symlinks -v --no-dangling-links %file15% %file15% /ext_link_noexist1 /ext_link_noexist2

    rem ########################################################################
    rem # test for group diff recursivly
    rem ########################################################################
    rem root 
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% / /
	call :tooltest h5diff_500.txt -v %grp_recurse1% %grp_recurse2% / /

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1% %src_grp_recurse2% / /
	call :tooltest h5diff_501.txt -v --follow-symlinks %grp_recurse1% %grp_recurse2% / /

    rem root vs group
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% / /grp1/grp2/grp3
	call :tooltest h5diff_502.txt -v %grp_recurse1% %grp_recurse2% / /grp1/grp2/grp3

    rem group vs group (same name and structure)
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% /grp1 /grp1
	call :tooltest h5diff_503.txt -v %grp_recurse1% %grp_recurse2% /grp1 /grp1

    rem group vs group (different name and structure)
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% /grp1/grp2 /grp1/grp2/grp3
	call :tooltest h5diff_504.txt -v %grp_recurse1% %grp_recurse2% /grp1/grp2 /grp1/grp2/grp3

    rem groups vs soft-link
    call :testing %h5diff%
	call :tooltest h5diff_505.txt -v %grp_recurse1% %grp_recurse2% /grp1 /slink_grp1

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1% %src_grp_recurse2% /grp1/grp2 /slink_grp2
	call :tooltest h5diff_506.txt -v --follow-symlinks %grp_recurse1% %grp_recurse2% /grp1/grp2 /slink_grp2

    rem groups vs ext-link
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% /grp1 /elink_grp1
	call :tooltest h5diff_507.txt -v %grp_recurse1% %grp_recurse2% /grp1 /elink_grp1

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1% %src_grp_recurse2% /grp1 /elink_grp1
	call :tooltest h5diff_508.txt -v --follow-symlinks %grp_recurse1% %grp_recurse2% /grp1 /elink_grp1

    rem soft-link vs ext-link
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% /slink_grp1 /elink_grp1
	call :tooltest h5diff_509.txt -v %grp_recurse1% %grp_recurse2% /slink_grp1 /elink_grp1

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1% %src_grp_recurse2% /slink_grp1 /elink_grp1
	call :tooltest h5diff_510.txt -v --follow-symlinks %grp_recurse1% %grp_recurse2% /slink_grp1 /elink_grp1

    rem circled ext links
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% /grp10 /grp11
	call :tooltest h5diff_511.txt -v %grp_recurse1% %grp_recurse2% /grp10 /grp11

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1% %src_grp_recurse2% /grp10 /grp11
	call :tooltest h5diff_512.txt -v --follow-symlinks %grp_recurse1% %grp_recurse2% /grp10 /grp11

    rem circled soft2ext-link vs soft2ext-link
    call :testing %h5diff% -v %src_grp_recurse1% %src_grp_recurse2% /slink_grp10 /slink_grp11
	call :tooltest h5diff_513.txt -v %grp_recurse1% %grp_recurse2% /slink_grp10 /slink_grp11

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1% %src_grp_recurse2% /slink_grp10 /slink_grp11
	call :tooltest h5diff_514.txt -v --follow-symlinks %grp_recurse1% %grp_recurse2% /slink_grp10 /slink_grp11

    rem ######################################################################
    rem # Test for group recursive diff via multi-linked external links 
    rem # With follow-symlinks, file $GRP_RECURSE1_EXT and $GRP_RECURSE2_EXT1 
    rem # should be same with the external links.
    rem ######################################################################
    rem file vs file
    call :testing %h5diff% -v %src_grp_recurse1_ext% %src_grp_recurse2_ext1%
    call :tooltest h5diff_515.txt -v %grp_recurse1_ext% %grp_recurse2_ext1%

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1_ext% %src_grp_recurse2_ext1%
    call :tooltest h5diff_516.txt -v --follow-symlinks %grp_recurse1_ext% %grp_recurse2_ext1%

    rem group vs group
    call :testing %h5diff% -v %src_grp_recurse1_ext% %src_grp_recurse2_ext1% /g1
    call :tooltest h5diff_517.txt -v %grp_recurse1_ext% %grp_recurse2_ext1% /g1

    call :testing %h5diff% -v --follow-symlinks %src_grp_recurse1_ext% %src_grp_recurse2_ext1% /g1
    call :tooltest h5diff_518.txt -v --follow-symlinks %grp_recurse1_ext% %grp_recurse2_ext1% /g1


    rem ##############################################################################
    rem # Exclude objects (--exclude-path)
    rem ##############################################################################
    rem #-------------------------------------------------
    rem # Same structure, same names and different value.

    rem Exclude the object with different value. Expect return - same
    call :testing %h5diff% -v --exclude-path /group1/dset3 %srcexclude1_1% %srcexclude1_2%
    call :tooltest h5diff_480.txt -v --exclude-path /group1/dset3 %exclude1_1% %exclude1_2%

    rem Verify different by not excluding. Expect return - diff
    call :testing %h5diff% -v %srcexclude1_1% %srcexclude1_2%
    call :tooltest h5diff_481.txt -v %exclude1_1% %exclude1_2%

    rem #----------------------------------------
    rem # Different structure, different names. 

    rem Exclude all the different objects. Expect return - same
    call :testing %h5diff% -v --exclude-path "/group1" --exclude-path "/dset1" %srcexclude2_1% %srcexclude2_2%
    call :tooltest h5diff_482.txt -v --exclude-path "/group1" --exclude-path "/dset1" %exclude2_1% %exclude2_2%

    rem Exclude only some different objects. Expect return - diff
    call :testing %h5diff% -v --exclude-path "/group1" %srcexclude2_1% %srcexclude2_2%
    call :tooltest h5diff_483.txt -v --exclude-path "/group1" %exclude2_1% %exclude2_2%

    rem Exclude from group compare
    call :testing %h5diff% -v --exclude-path "/dset3" %srcexclude1_1% %srcexclude1_2% /group1
    call :tooltest h5diff_484.txt -v --exclude-path "/dset3"  %exclude1_1% %exclude1_2% /group1

    rem ##############################################################################
    rem # diff various multiple vlen and fixed strings in a compound type dataset
    rem ##############################################################################
    call :testing %h5diff% -v %src_comp_vl_strs% %src_comp_vl_strs%
    call :tooltest h5diff_530.txt -v  %comp_vl_strs% %comp_vl_strs%

	
    rem #######################################################################
    rem # END
    rem #######################################################################

    if %nerrors% equ 0 (
       echo.All %h5diff% tests passed.
    )

    popd
    endlocal & exit /b %nerrors%
    
