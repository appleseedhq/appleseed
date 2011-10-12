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
rem Tests for the hdf5 library
rem
rem    Created:  Scott Wegner, 9/4/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

set /a nerrors=0

rem Clean any variables starting with "HDF5_LIBTEST_", as we use these for our
rem tests.  Also clear "HDF5_LIBTEST_TESTS", as we will be addding all of our tests
rem to this variable.
rem Set at least one variable in set beforehand to avoid error message.
rem --SJW 9/5/07
set hdf5_libtest_=foo
for /f "tokens=1 delims==" %%a in ('set hdf5_libtest_') do set %%a=
set hdf5_libtest_tests=

goto main


rem Function to add a test to the test suite.  
rem Expects the following parameters:
rem     %1 - Name of the libtest being tested
rem     %2 - Relative path of script
:add_test

    set hdf5_libtest_tests=%hdf5_libtest_tests% %1
    set hdf5_libtest_%1_test=%CD%\%2\%1

    exit /b
    

rem Run all of the tests that have been added to the suite.  Print a header
rem at the beginning of each one.  Short-circuit if a test fails.
rem Expects the following parameters:
rem     %1 - release or debug version
rem     %2 - "dll" or nothing
:run_tests
    for %%a in (%hdf5_libtest_tests%) do (
        echo.
        echo.************************************
        echo.  Testing %%a ^(%1 %2^)
        echo.************************************
        
        rem Only add our parameters for batch scripts.
        call !hdf5_libtest_%%a_test:.bat= %1 %2!
        rem Exit early if test fails.
        if errorlevel 1 (
            set /a nerrors=!nerrors!+1
			echo.
			echo.************************************
			echo.  Testing %%a ^(%1 %2^)  FAILED
			exit /b 1
		)
    )
    
    rem If we get here, that means all of our tests passed.
    exit /b


rem This is where we add tests to the suite, and run them all at the end.
rem Make sure only to run dll versions of tests you build dll for
rem Also make sure to add *.bat to batch scripts, as the above functions rely
rem on it for sending parameters.  --SJW 9/6/07
:main

    call :add_test testerror.bat .
    call :add_test testhdf5%2 .\testhdf5%2\%1
    call :add_test lheap%2 .\lheap%2\%1
    call :add_test ohdr%2 .\ohdr%2\%1
    call :add_test stab%2 .\stab%2\%1
    call :add_test gheap%2 .\gheap%2\%1
    call :add_test cache%2 .\cache%2\%1
    call :add_test cache_api%2 .\cache_api%2\%1
    call :add_test pool%2 .\pool%2\%1
    call :add_test hyperslab%2 .\hyperslab%2\%1
    call :add_test istore%2 .\istore%2\%1
    call :add_test bittests%2 .\bittests%2\%1
    call :add_test dt_arith%2 .\dt_arith%2\%1
    call :add_test dtypes%2 .\dtypes%2\%1
    call :add_test dsets%2 .\dsets%2\%1
    call :add_test cmpd_dset%2 .\cmpd_dset%2\%1
    call :add_test extend%2 .\extend%2\%1
    call :add_test external%2 .\external%2\%1
    call :add_test objcopy%2 .\objcopy%2\%1
    call :add_test links%2 .\links%2\%1
    call :add_test unlink%2 .\unlink%2\%1
    call :add_test big%2 .\big%2\%1
    call :add_test mtime%2 .\mtime%2\%1
    call :add_test fillval%2 .\fillval%2\%1
    call :add_test mount%2 .\mount%2\%1
    call :add_test flush1%2 .\flush1%2\%1
    call :add_test flush2%2 .\flush2%2\%1
    call :add_test app_ref%2 .\app_ref%2\%1
    call :add_test enum%2 .\enum%2\%1
    call :add_test set_extent%2 .\set_extent%2\%1
    rem Test commented because threadsafe is not built by default on Windows.
    rem --SJW 9/5/07
    rem call :add_test ttsafe%2 .\ttsafe%2\%1
    rem Test commented because stream driver is not supported on Windows.
    rem --SJW 9/5/07
    rem call :add_test stream_test%2 .\stream_test%2\%1
    call :add_test getname%2 .\getname%2\%1
    call :add_test vfd%2 .\vfd%2\%1
    call :add_test ntypes%2 .\ntypes%2\%1
    call :add_test dangle%2 .\dangle%2\%1
    call :add_test reserved%2 .\reserved%2\%1
    call :add_test cross_read%2 .\cross_read%2\%1
    call :add_test freespace%2 .\freespace%2\%1
    call :add_test mf%2 .\mf%2\%1
    call :add_test btree2%2 .\btree2%2\%1
    call :add_test fheap%2 .\fheap%2\%1
    call :add_test tcheckversion%2 .\tcheckversion%2\%1
    
    
    rem Run the tests, passing in which version to run
    call :run_tests %*
        
    if "%nerrors%"=="0" (
		echo.All library tests passed.
	) else (
        echo.** FAILED Library tests.
    )
        
    popd
    endlocal & exit /b %nerrors%
    