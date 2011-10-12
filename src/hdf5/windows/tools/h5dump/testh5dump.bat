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
rem    Created:  Scott Wegner, 8/23/07
rem    Modified: Scott Wegner, 5/12/08
rem

setlocal enabledelayedexpansion
pushd %~dp0

set h5pubconf=%CD%\..\..\src\h5pubconf.h

rem Determine which filters are available
rem On Windows, the function :detect_filter sets these for us
call :detect_filter szip
call :detect_filter deflate
call :detect_filter shuffle
call :detect_filter fletcher32
call :detect_filter nbit
call :detect_filter scaleoffset

call :detect_packedbits

rem The tool name
set dumper=h5dump%2
rem The path of the tool library
set dumper_bin=%CD%\..\%dumper%\%1\%dumper%
set testdir=%CD%\..\testfiles

rem The h5diff tool name
set h5diff=..\h5diff%2\%1\h5diff%2
rem The path of the h5diff tool binary
set h5diff_bin=%CD%\%h5diff%

rem The h5import tool name
set h5import=..\h5import%2\%1\h5import%2
rem The path of the h5import tool binary
set h5import_bin=%CD%\%h5import%

set nerrors=0
set verbose=yes

set srcdir=%CD%

if not exist %testdir% mkdir %testdir%

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
    rem We need to replace PERCENT here with "%" for tests that use a percent
    rem sign.  --SJW 5/12/08
    set test_msg=!test_msg:PERCENT=%%!                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Run a test and print PASS or *FAIL*.  If a test fails then increment
rem the `nerrors' global variable and (if $verbose is set) display the
rem difference between the actual output and the expected output. The
rem expected output is given as the first argument to this function and
rem the actual output file is calculated by replacing the `.ddl' with
rem `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
rem non-zero value.
rem
:tooltest
    set expect=%CD%\..\testfiles\%1
    set actual=%CD%\..\testfiles\%~n1.out
    set actual_err=%CD%\..\testfiles\%~n1.err
    
    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/23/07
    set params=%*
    rem If there is not 2nd parameter, that means we have no filename, which 
    rem implies that we are on the "tnofilename" test.  Make sure we remove the
    rem expected output from the params, and add a space.  --SJW 8/27/07
    if "%2"=="" (
        set params= 
    ) else (
        set params=!params:* =!
    )
    
    rem Run test.
    
    (
        rem We need to replace PERCENT here with "%" for tests that use percents
        rem Also remove quotes here, because Linux 'echo' command strips them.
        rem --SJW 8/24/07
        set params_echo=!params:PERCENT=%%!
        echo.#############################
        echo.Expected output for 'h5dump !params_echo:"=!'
        echo.#############################
        pushd %CD%\..\testfiles
        %dumper_bin% !params:PERCENT=%%!
        popd
    ) > %actual% 2> %actual_err%
    type %actual_err% >> %actual%
    
    if not exist %expect% (
        rem Create the expect file if it doesn't yet exist.
        call :testing CREATED %params%
        copy /y %actual% %expect% > nul
    ) else (
        fc /w %expect% %actual% > nul
        if !errorlevel! equ 0 (
            call :testing PASSED %params%
        ) else (
            call :testing *FAILED* %params%
            echo.    Expected results ^(*.ddl^) differs from actual results ^(*.out^)
            set /a nerrors=!nerrors!+1
            if "yes"=="%verbose%" fc /w %expect% %actual%
        )
    )
    
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_err%
    )
    
    exit /b
    

rem same as TOOLTEST but does not print the header Expected output
rem use for the binary tests that expect a full path in -o
:tooltest1

    set expect=%srcdir%\..\testfiles\%1
    set actual=%CD%\..\testfiles\%~n1.out
    set actual_err=%CD%\..\testfiles\%~n1.err

    rem We define %params% here because Windows `shift` command doesn't affect
    rem the %* variable.  --SJW 8/23/07
    set params=%*
    rem If there is not 2nd parameter, that means we have no filename, which 
    rem implies that we are on the "tnofilename" test.  Make sure we remove the
    rem expected output from the params, and add a space.  --SJW 8/27/07
    if "%2"=="" (
        set params= 
    ) else (
        set params=!params:* =!
    )
    
    rem Run test.
    (
        pushd %CD%\..\testfiles
        %dumper_bin% !params:PERCENT=%%!
        popd
    ) > %actual% 2> %actual_err%
    type %actual_err% >> %actual%
        
    if not exist %expect% (
        rem Create the expect file if it doesn't yet exist.
        call :testing CREATED %params%
        copy /y %actual% %expect% > nul
    ) else (
        fc /w %expect% %actual% > nul
        if !errorlevel! equ 0 (
            call :testing PASSED %params%
        ) else (
            call :testing *FAILED* %params%
            echo.    Expected results ^(*.ddl^) differs from actual results ^(*.out^)
            set /a nerrors=!nerrors!+1
            if "yes"=="%verbose%" fc /w %expect% %actual%
        )
    )
    
    rem Clean up output file
    if not defined hdf5_nocleanup (
        del /f %actual% %actual_err%
    )
    
    exit /b

rem Print a "SKIP" message
:skip
    call :testing -SKIP- %*
    exit /b
    
    
rem Print a line-line message left justified in a field of 70 characters
rem
:print_h5diff
    set test_msg=Running h5diff
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    set test_msg=%test_msg%                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Call the h5diff tool
rem
:difftest
    (
        pushd %CD%\..\testfiles
        %h5diff_bin% %* -q
        popd
    )
    if %errorlevel% neq 0 (
        call :print_h5diff *FAILED* %*
        set /a nerrors=!nerrors!+1
    ) else (
        call :print_h5diff PASSED %*
    )
    
    exit /b

    
rem Print a line-line message left justified in a field of 70 characters
rem beginning with the word "Verifying".
rem
:print_h5import
    set test_msg=Running h5import
    for %%a in (%*) do (
        if %%a neq PASSED (
        if %%a neq *FAILED* (
            set test_msg=!test_msg! %%~nxa
        ) )
    )
    set test_msg=%test_msg%                                                                
    echo.%test_msg:~0,69% %1
    
    exit /b

    
rem Call the h5import tool
rem
:importtest
    rem Remove the output hdf5 file if it exists
    set hdf5_file=%CD%\..\testfiles\%5
    if exist %hdf5_file% (
        del /f %hdf5_file%
    )
    
    (
        pushd %CD%\..\testfiles
        %h5import_bin% %*
        popd
    )
    if %errorlevel% neq 0 (
        call :print_h5import *FAILED* %*
        set /a nerrors=!nerrors!+1
    ) else (
        call :print_h5import PASSED %*
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

:detect_packedbits
    findstr /b /i /c:"#define H5_HAVE_H5DUMP_PACKED_BITS" %h5pubconf% > nul
    if %errorlevel% equ 0 (
        set Have_Packed_Bits=yes
    ) else (
        set Have_Packed_Bits=no
    )
    
    exit /b


rem ############################################################################
rem ############################################################################
rem #                       T H E   T E S T S                                ###
rem ############################################################################
rem ############################################################################
:main

    rem test for displaying groups
    call :tooltest tgroup-1.ddl tgroup.h5
    rem test for displaying the selected groups
    call :tooltest tgroup-2.ddl --group=/g2 --group / -g /y tgroup.h5

    rem test for displaying simple space datasets
    call :tooltest tdset-1.ddl tdset.h5
    rem test for displaying selected datasets
    call :tooltest tdset-2.ddl -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5

    rem test for displaying attributes
    call :tooltest tattr-1.ddl tattr.h5
    rem test for displaying the selected attributes of string type and scalar space
    call :tooltest tattr-2.ddl -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5
    rem test for header and error messages
    call :tooltest tattr-3.ddl --header -a /attr2 --attribute=/attr tattr.h5
    rem test for displaying attributes in shared datatype (also in group and dataset)
    call :tooltest tnamed_dtype_attr.ddl tnamed_dtype_attr.h5

    rem test for displaying soft links and user-defined links
    call :tooltest tslink-1.ddl tslink.h5
    call :tooltest tudlink-1.ddl tudlink.h5
    rem test for displaying the selected link
    call :tooltest tslink-2.ddl -l slink2 tslink.h5
    call :tooltest tudlink-2.ddl -l udlink2 tudlink.h5

    rem tests for hard links
    call :tooltest thlink-1.ddl thlink.h5
    call :tooltest thlink-2.ddl -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5
    call :tooltest thlink-3.ddl -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5
    call :tooltest thlink-4.ddl -g /g1 thlink.h5
    call :tooltest thlink-5.ddl -d /dset1 -g /g2 -d /g1/dset2 thlink.h5

    rem tests for compound data types
    call :tooltest tcomp-1.ddl tcompound.h5
    rem test for named data types
    call :tooltest tcomp-2.ddl -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5
    rem test for unamed type 
    call :tooltest tcomp-3.ddl -t /#6632 -g /group2 tcompound.h5
    rem test complicated compound datatype
    call :tooltest tcomp-4.ddl tcompound_complex.h5
    
    rem test for the nested compound type
    call :tooltest tnestcomp-1.ddl tnestedcomp.h5

    rem test for options
    call :tooltest tall-1.ddl tall.h5
    call :tooltest tall-2.ddl --header -g /g1/g1.1 -a attr2 tall.h5
    call :tooltest tall-3.ddl -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5

    rem test for loop detection
    call :tooltest tloop-1.ddl tloop.h5

    rem test for string 
    call :tooltest tstr-1.ddl tstr.h5
    call :tooltest tstr-2.ddl tstr2.h5

    rem test for file created by Lib SAF team
    call :tooltest tsaf.ddl tsaf.h5

    rem test for file with variable length data
    call :tooltest tvldtypes1.ddl tvldtypes1.h5
    call :tooltest tvldtypes2.ddl tvldtypes2.h5
    call :tooltest tvldtypes3.ddl tvldtypes3.h5
    call :tooltest tvldtypes4.ddl tvldtypes4.h5
    call :tooltest tvldtypes5.ddl tvldtypes5.h5

    rem test for file with variable length string data
    call :tooltest tvlstr.ddl tvlstr.h5

    rem test for files with array data
    call :tooltest tarray1.ddl tarray1.h5
    call :tooltest tarray1_big.ddl -R tarray1_big.h5
    call :tooltest tarray2.ddl tarray2.h5
    call :tooltest tarray3.ddl tarray3.h5
    call :tooltest tarray4.ddl tarray4.h5
    call :tooltest tarray5.ddl tarray5.h5
    call :tooltest tarray6.ddl tarray6.h5
    call :tooltest tarray7.ddl tarray7.h5
    call :tooltest tarray8.ddl tarray8.h5

    rem test for files with empty data
    call :tooltest tempty.ddl tempty.h5

    rem test for files with groups that have comments
    call :tooltest tgrp_comments.ddl tgrp_comments.h5

    rem test the --filedriver flag
    call :tooltest tsplit_file.ddl --filedriver=split tsplit_file
    rem On Windows, we pass "PERCENT", and let other calls replace it with
    rem the "%".  We cannot pass "%" directly because Windows interprets it as
    rem the name of the script.  --SJW 8/24/07
    call :tooltest tfamily.ddl --filedriver=family tfamilyPERCENT05d.h5
    call :tooltest tmulti.ddl --filedriver=multi tmulti

    rem test for files with group names which reach > 1024 bytes in size
    call :tooltest tlarge_objname.ddl -w157 tlarge_objname.h5

    rem test '-A' to suppress data but print attr's
    call :tooltest tall-2A.ddl -A tall.h5

    rem test '-r' to print attributes in ASCII instead of decimal
    call :tooltest tall-2B.ddl -A -r tall.h5

    rem test Subsetting
    call :tooltest tall-4s.ddl --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5
    call :tooltest tall-5s.ddl -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5
    call :tooltest tdset-3s.ddl -d "/dset1[1,1;;;]" tdset.h5
    rem block
    rem call :tooltest tdset2-1s.ddl -d "/dset1[;3,2;4,4;1,4]" tdset2.h5

    rem test printing characters in ASCII instead of decimal
    call :tooltest tchar1.ddl -r tchar.h5

    rem test failure handling
    rem Missing file name
	if "%Have_Packed_Bits%"=="yes" (
		call :tooltest tnofilename-with-packed-bits.ddl 
	) else (
		call :tooltest tnofilename.ddl 
	)

    rem rev. 2004

    rem tests for super block
    call :tooltest tboot1.ddl -H -B -d dset tfcontents1.h5
    call :tooltest tboot2.ddl -B tfcontents2.h5

    rem test -p with a non existing dataset
    call :tooltest tperror.ddl -p -d bogus tfcontents1.h5

    rem test for file contents
    call :tooltest tcontents.ddl -n tfcontents1.h5

    rem tests for storage layout
    rem compact
    call :tooltest tcompact.ddl -H -p -d compact tfilters.h5
    rem contiguous
    call :tooltest tcontiguos.ddl -H -p -d contiguous tfilters.h5
    rem chunked
    call :tooltest tchunked.ddl -H -p -d chunked tfilters.h5
    rem external 
    call :tooltest texternal.ddl -H -p -d external tfilters.h5

    rem fill values
    call :tooltest tfill.ddl -p tfvalues.h5

    rem several datatype, with references , print path
    call :tooltest treference.ddl  tattr2.h5

    rem escape/not escape non printable characters
    call :tooltest tstringe.ddl -e tstr3.h5
    call :tooltest tstring.ddl tstr3.h5
    rem char data as ASCII with non escape
    call :tooltest tstring2.ddl -r -d str4 tstr3.h5

    rem array indices print/not print
    call :tooltest tindicesyes.ddl taindices.h5
    call :tooltest tindicesno.ddl -y taindices.h5

    rem ######### array indices with subsetting
    rem 1D case
    call :tooltest tindicessub1.ddl -d 1d -s 1 -S 10 -c 2 -k 3 taindices.h5

    rem 2D case
    call :tooltest tindicessub2.ddl -d 2d -s 1,2 -S 3,3 -c 3,2 -k 2,2 taindices.h5

    rem 3D case
    call :tooltest tindicessub3.ddl -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2 -k 1,2,2 taindices.h5

    rem 4D case
    call :tooltest tindicessub4.ddl -d 4d -s 0,0,1,2 -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2 taindices.h5


    rem tests for filters
    rem SZIP
    set option=-H -p -d szip tfilters.h5
    if not "%use_filter_szip%"=="yes" (
        call :skip %option%
    ) else (
        call :tooltest tszip.ddl %option%
    )
    rem deflate
    set option=-H -p -d deflate tfilters.h5
    if not "%use_filter_deflate%"=="yes" (
        call :skip %option%
    ) else (
        call :tooltest tdeflate.ddl %option%
    )
    rem shuffle
    set option=-H -p -d shuffle tfilters.h5
    if not "%use_filter_shuffle%"=="yes" (
        call :skip %option%
    ) else (
        call :tooltest tshuffle.ddl %option%
    )
    rem fletcher32
    set option=-H -p -d fletcher32  tfilters.h5
    if not "%use_filter_fletcher32%"=="yes" (
        call :skip %option%
    ) else (
        call :tooltest tfletcher32.ddl %option%
    )
    rem nbit
    set option=-H -p -d nbit  tfilters.h5
    if not "%use_filter_nbit%"=="yes" (
        call :skip %option%
    ) else (
        call :tooltest tnbit.ddl %option%
    )
    rem scaleoffset
    set option=-H -p -d scaleoffset  tfilters.h5
    if not "%use_filter_scaleoffset%"=="yes" (
        call :skip %option%
    ) else (
        call :tooltest tscaleoffset.ddl %option%
    )
    rem all
    set option=-H -p -d all  tfilters.h5
    rem Windows doesn't have "or" for compound conditional, so we must check
    rem each one individually.  --SJW 8/24/07
    if not "%use_filter_fletcher32%"=="yes" (
        call :skip %option%
    ) else if not "%use_filter_szip%"=="yes" (
        call :skip %option%
    ) else if not "%use_filter_deflate%"=="yes" (
        call :skip %option%
    ) else if not "%use_filter_shuffle%"=="yes" (
        call :skip %option%
    ) else if not "%use_filter_nbit%"=="yes" (
        call :skip %option%
    ) else if not "%use_filter_scaleoffset%"=="yes" (
        call :skip %option%
    ) else (
        call :tooltest tallfilters.ddl %option%
    )
    rem user defined
    call :tooltest tuserfilter.ddl -H  -p -d myfilter  tfilters.h5

    rem test for displaying objects with very long names
    call :tooltest tlonglinks.ddl tlonglinks.h5

    rem dimensions over 4GB, print boundary 
    call :tooltest tbigdims.ddl -d dset4gb -s 4294967284 -c 22 tbigdims.h5

    rem hyperslab read
    call :tooltest thyperslab.ddl thyperslab.h5


    rem
        
    rem test for displaying dataset and attribute of null space
    call :tooltest tnullspace.ddl tnullspace.h5

    rem test for long double (some systems do not have long double)
    rem call :tooltest tldouble.ddl tldouble.h5

    rem test for vms
    call :tooltest tvms.ddl tvms.h5

    rem test for binary output
    rem Don't use %testdir% here, because we are already in the correct
    rem directory, and using it only gets in the way of the output formatting.
    rem --SJW 8/24/07
    call :tooltest1   tbin1.ddl -d integer -o out1.bin -b LE tbinary.h5

    rem NATIVE default. the NATIVE test can be validated with h5import/h5diff
    call :tooltest1   tbin1.ddl -d integer -o out1.bin  -b     tbinary.h5
    call :importtest out1.bin -c out3.h5import -o out1.h5
    call :difftest tbinary.h5 out1.h5 /integer /integer

    call :tooltest1   tbin2.ddl -b BE -d float  -o out2.bin      tbinary.h5

    rem the NATIVE test can be validated with h5import/h5diff
    call :tooltest1   tbin3.ddl -d integer -o out3.bin -b NATIVE tbinary.h5
    call :importtest out3.bin -c out3.h5import -o out3.h5
    call :difftest tbinary.h5 out3.h5 /integer /integer

    call :tooltest1   tbin4.ddl -d double  -o out4.bin -b FILE   tbinary.h5
       
    rem Clean up binary output files
    if not defined hdf5_nocleanup (
        for /l %%a in (1,1,4) do del /f %testdir%\out%%a.bin
        del /f %testdir%\out3.h5
    )
    
    rem test for dataset region references 
    call :tooltest tdatareg.ddl tdatareg.h5
    call :tooltest tdataregR.ddl -R tdatareg.h5
    call :tooltest tattrreg.ddl tattrreg.h5
    call :tooltest tattrregR.ddl -R tattrreg.h5

    rem tests for group creation order
    rem "1" tracked, "2" name, root tracked
    call :tooltest tordergr1.ddl --group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5
    call :tooltest tordergr2.ddl --group=1 --sort_by=creation_order --sort_order=descending tordergr.h5
    call :tooltest tordergr3.ddl -g 2 -q name -z ascending tordergr.h5
    call :tooltest tordergr4.ddl -g 2 -q name -z descending tordergr.h5
    call :tooltest tordergr5.ddl -q creation_order tordergr.h5

    rem tests for attribute order
    call :tooltest torderattr1.ddl -H --sort_by=name --sort_order=ascending torderattr.h5
    call :tooltest torderattr2.ddl -H --sort_by=name --sort_order=descending torderattr.h5
    call :tooltest torderattr3.ddl -H --sort_by=creation_order --sort_order=ascending torderattr.h5
    call :tooltest torderattr4.ddl -H --sort_by=creation_order --sort_order=descending torderattr.h5

    rem tests for floating point user defined printf format
    rem Note: Make sure to use PERCENT rather than "%", because Windows needs
    rem to handle it specially.  --SJW 5/12/08
    call :tooltest tfpformat.ddl -m PERCENT.7f tfpformat.h5
    
    rem tests for traversal of external links
    call :tooltest textlinksrc.ddl textlinksrc.h5
    call :tooltest textlinkfar.ddl textlinkfar.h5

    rem test for dangling external links
    call :tooltest textlink.ddl textlink.h5
    
    rem tests for traversal of external links
    call :tooltest textlinksrc.ddl textlinksrc.h5
    call :tooltest textlinkfar.ddl textlinkfar.h5

	if "%Have_Packed_Bits%"=="yes" (
		rem test for dataset packed bits 
		rem Set up xCMD to test or skip.
        rem Limits:
        rem Maximum number of packed bits is 8 (for now).
        rem Maximum integer size is 64 (for now).
        rem Maximun Offset is 63 (Maximum size - 1).
        rem Maximum Offset+Length is 64 (Maximum size).
        rem Tests:
        rem Normal operation on both signed and unsigned int datasets.
        rem Sanity check
        rem Their rawdata output should be the same.
        call :tooltest tpbitsSignedWhole.ddl -d /DS08BITS -M 0,8 packedbits.h5
        call :tooltest tpbitsUnsignedWhole.ddl -d /DU08BITS -M 0,8 packedbits.h5
        call :tooltest tpbitsSignedIntWhole.ddl -d /DS16BITS -M 0,16 packedbits.h5
        call :tooltest tpbitsUnsignedIntWhole.ddl -d /DU16BITS -M 0,16 packedbits.h5
        call :tooltest tpbitsSignedLongWhole.ddl -d /DS32BITS -M 0,32 packedbits.h5
        call :tooltest tpbitsUnsignedLongWhole.ddl -d /DU32BITS -M 0,32 packedbits.h5
        call :tooltest tpbitsSignedLongLongWhole.ddl -d /DS64BITS -M 0,64 packedbits.h5
        call :tooltest tpbitsUnsignedLongLongWhole.ddl -d /DU64BITS -M 0,64 packedbits.h5
        call :tooltest tpbitsSignedLongLongWhole63.ddl -d /DS64BITS -M 0,63 packedbits.h5
        call :tooltest tpbitsUnsignedLongLongWhole63.ddl -d /DU64BITS -M 0,63 packedbits.h5
        call :tooltest tpbitsSignedLongLongWhole1.ddl -d /DS64BITS -M 1,63 packedbits.h5
        call :tooltest tpbitsUnsignedLongLongWhole1.ddl -d /DU64BITS -M 1,63 packedbits.h5
        rem Half sections
        call :tooltest tpbitsSigned4.ddl -d /DS08BITS -M 0,4,4,4 packedbits.h5
        call :tooltest tpbitsUnsigned4.ddl -d /DU08BITS -M 0,4,4,4 packedbits.h5
        call :tooltest tpbitsSignedInt8.ddl -d /DS16BITS -M 0,8,8,8 packedbits.h5
        call :tooltest tpbitsUnsignedInt8.ddl -d /DU16BITS -M 0,8,8,8 packedbits.h5
        call :tooltest tpbitsSignedLong16.ddl -d /DS32BITS -M 0,16,16,16 packedbits.h5
        call :tooltest tpbitsUnsignedLong16.ddl -d /DU32BITS -M 0,16,16,16 packedbits.h5
        call :tooltest tpbitsSignedLongLong32.ddl -d /DS64BITS -M 0,32,32,32 packedbits.h5
        call :tooltest tpbitsUnsignedLongLong32.ddl -d /DU64BITS -M 0,32,32,32 packedbits.h5
        rem Quarter sections
        call :tooltest tpbitsSigned2.ddl -d /DS08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5
        call :tooltest tpbitsUnsigned2.ddl -d /DU08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5
        call :tooltest tpbitsSignedInt4.ddl -d /DS16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5
        call :tooltest tpbitsUnsignedInt4.ddl -d /DU16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5
        call :tooltest tpbitsSignedLong8.ddl -d /DS32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5
        call :tooltest tpbitsUnsignedLong8.ddl -d /DU32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5
        call :tooltest tpbitsSignedLongLong16.ddl -d /DS64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5
        call :tooltest tpbitsUnsignedLongLong16.ddl -d /DU64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5
        rem Begin and End
        call :tooltest tpbitsSigned.ddl -d /DS08BITS -M 0,2,2,6 packedbits.h5
        call :tooltest tpbitsUnsigned.ddl -d /DU08BITS -M 0,2,2,6 packedbits.h5
        call :tooltest tpbitsSignedInt.ddl -d /DS16BITS -M 0,2,10,6 packedbits.h5
        call :tooltest tpbitsUnsignedInt.ddl -d /DU16BITS -M 0,2,10,6 packedbits.h5
        call :tooltest tpbitsSignedLong.ddl -d /DS32BITS -M 0,2,26,6 packedbits.h5
        call :tooltest tpbitsUnsignedLong.ddl -d /DU32BITS -M 0,2,26,6 packedbits.h5
        call :tooltest tpbitsSignedLongLong.ddl -d /DS64BITS -M 0,2,58,6 packedbits.h5
        call :tooltest tpbitsUnsignedLongLong.ddl -d /DU64BITS -M 0,2,58,6 packedbits.h5
        rem Overlapped packed bits.
        call :tooltest tpbitsOverlapped.ddl -d /DS08BITS -M 0,1,1,1,2,1,0,3 packedbits.h5
        rem Maximum number of packed bits.
        call :tooltest tpbitsMax.ddl -d /DS08BITS -M 0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5
        rem Compound type.
        call :tooltest tpbitsCompound.ddl -d /dset1 -M 0,1,1,1 tcompound.h5
        rem Array type.
        call :tooltest tpbitsArray.ddl -d /Dataset1 -M 0,1,1,1 tarray1.h5
        rem Test Error handling.
        rem Too many packed bits requested. Max is 8 for now.
        call :tooltest tpbitsMaxExceeded.ddl -d /DS08BITS -M 0,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5
        rem Offset too large. Max is 7 (8-1) for now.
        call :tooltest tpbitsOffsetExceeded.ddl -d /DS08BITS -M 64,1 packedbits.h5
        call :tooltest tpbitsCharOffsetExceeded.ddl -d /DS08BITS -M 8,1 packedbits.h5
        call :tooltest tpbitsIntOffsetExceeded.ddl -d /DS16BITS -M 16,1 packedbits.h5
        call :tooltest tpbitsLongOffsetExceeded.ddl -d /DS32BITS -M 32,1 packedbits.h5
        rem Bad offset, must not be negative.
        call :tooltest tpbitsOffsetNegative.ddl -d /DS08BITS -M -1,1 packedbits.h5
        rem Bad length, must not be positive.
        call :tooltest tpbitsLengthPositive.ddl -d /DS08BITS -M 4,0 packedbits.h5
        rem Offset+Length is too large. Max is 8 for now.
        call :tooltest tpbitsLengthExceeded.ddl -d /DS08BITS -M 37,28 packedbits.h5
        call :tooltest tpbitsCharLengthExceeded.ddl -d /DS08BITS -M 2,7 packedbits.h5
        call :tooltest tpbitsIntLengthExceeded.ddl -d /DS16BITS -M 10,7 packedbits.h5
        call :tooltest tpbitsLongLengthExceeded.ddl -d /DS32BITS -M 26,7 packedbits.h5
        rem Incomplete pair of packed bits request.
        call :tooltest tpbitsIncomplete.ddl -d /DS08BITS -M 0,2,2,1,0,2,2, packedbits.h5
	)
    
    if %nerrors% equ 0 (
        echo.All %dumper% tests passed.
    )
    
    popd
    endlocal & exit /b %nerrors%
    
