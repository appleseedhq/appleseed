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
rem Tests for the hdf5 c++ library
rem
rem    Created:  Scott Wegner, 9/4/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

set /a nerrors=0

rem Clean any variables starting with "HDF5_CPPTEST_", as we use these for our
rem tests.  Also clear "HDF5_CPPTEST_TESTS", as we will be addding all of our tests
rem to this variable.
rem Set at least one variable in set beforehand to avoid error message.
rem --SJW 9/5/07
set hdf5_cpptest_=foo
for /f "tokens=1 delims==" %%a in ('set hdf5_cpptest_') do set %%a=
set hdf5_cpptest_tests=

goto main


rem Function to add a test to the test suite.  
rem Expects the following parameters:
rem     %1 - Name of the cpptest being tested
rem     %2 - Relative path of script
:add_test

    set hdf5_cpptest_tests=%hdf5_cpptest_tests% %1
    set hdf5_cpptest_%1_test=%CD%\%2\%1

    exit /b
    

rem Run all of the tests that have been added to the suite.  Print a header
rem at the beginning of each one.  Short-circuit if a test fails.
rem Expects the following parameters:
rem     %1 - release or debug version
rem     %2 - "dll" or nothing
:run_tests
    for %%a in (%hdf5_cpptest_tests%) do (
        echo.
        echo.************************************
        echo.  Testing %%a ^(%1 %2^)
        echo.************************************
        
        rem Only add our parameters for batch scripts.
        call !hdf5_cpptest_%%a_test:.bat= %1 %2!
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

    call :add_test testhdf5_cpp%2 .\testhdf5_cpp%2\%1
    
    
    rem Run the tests, passing in which version to run
    call :run_tests %*
        
    if "%nerrors%"=="0" (
		echo.All C++ library tests passed.
	) else (
        echo.** FAILED C++ library tests.
    )

    popd
    endlocal & exit /b %nerrors%
