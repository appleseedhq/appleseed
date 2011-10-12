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
rem  File Name: hdf5check.bat
rem  This batch file is used to test HDF5 Libraries and Tools.
rem  There are 4 options for this batch file:
rem    1. hdf5check                -- HDF5 tools and c library tests
rem    2. hdf5check enablecpp      -- HDF5 tools and c/c++ library tests
rem    3. hdf5check enablefortran  -- HDF5 tools and c/fortran library tests
rem    4. hdf5check enableall      -- HDF5 tools and c/c++/fortran library tests
rem    nodebug                     -- can be added to any of the above to not test debug versions

setlocal enabledelayedexpansion
pushd %~dp0

rem Clean any variables starting with "HDF5_TEST_", as we use these for our
rem tests.  Also clear "HDF5_TEST_TESTS", as we will be addding all of our tests
rem to this variable.
rem Set at least one variable in set beforehand to avoid error message.
rem --SJW 9/5/07
set hdf5_test_=foo
for /f "tokens=1 delims==" %%a in ('set hdf5_test_') do set %%a=
set hdf5_test_tests=
set chkdebug=debug
set chkrelease=release

rem Put built DLLs in the system folder for testing
call install_dll.BAT

set nerrors=0

rem See if we have built the HL C++ / Fortran libraries, and set 
rem BUILD_*_CONDITIONAL appropriately
if /i "%1" equ "enablecpp" (
    set build_cpp_conditional=true
) else if /i "%1" equ "enablefortran" (
    set build_fortran_conditional=true
) else if /i "%1" equ "enableall" (
    set build_cpp_conditional=true
    set build_fortran_conditional=true
) else if /i "%1" equ "nodebug" (
    set chkdebug=
)
if /i "%2" equ "nodebug" (
    set chkdebug=
)

goto main


rem Function to add a test to the test suite.  
rem Expects the following parameters:
rem     %1 - Name of the test being tested
rem     %2 - Relative path of script
:add_test

    set hdf5_test_tests=%hdf5_test_tests% %1
    set hdf5_test_%1_test=%CD%\%2\%1

    exit /b
    

rem Run all of the tests that have been added to the suite.  Print a header
rem at the beginning of each one.  Short-circuit if a test fails.
rem Expects the following parameters:
rem     %1 - release or debug version
rem     %2 - "dll" or nothing
:run_tests
    for %%a in (%hdf5_test_tests%) do (
        echo.
        echo.**==**==**==**==**==**==**==**==**==**
        echo.  Testing %%a ^(%1 %2^)
        echo.**==**==**==**==**==**==**==**==**==**
        
        rem Only add our parameters for batch scripts.
        call !hdf5_test_%%a_test:.bat= %1 %2!
        rem Exit early if test fails.
        if not errorlevel 0 (
            set /a nerrors=!nerrors!+1
			echo.  Testing %%a ^(%1 %2^)  FAILED
			echo.**==**==**==**==**==**==**==**==**==**
			exit /b 1
		) else if errorlevel 1 (
            set /a nerrors=!nerrors!+1
			echo.  Testing %%a ^(%1 %2^)  FAILED
			echo.**==**==**==**==**==**==**==**==**==**
			exit /b 1
		)
    )
    
    rem If we get here, that means all of our tests passed.
    echo.All HDF5 %* tests passed.

    exit /b


rem This is where we add tests to the suite, and run them all at the end.
rem Make sure only to run dll versions of tests you build dll for
rem Also make sure to add *.bat to batch scripts, as the above functions rely
rem on it for sending parameters.  --SJW 9/6/07
:main

    call :add_test checktests.bat .\test
    call :add_test checktools.bat .\tools
    
    rem Only check C++/Fortran if they are set to build.
    if defined build_cpp_conditional (
        call :add_test checkcpptests.bat .\c++\test
    )
    if defined build_fortran_conditional (
        call :add_test checkfortrantests.bat .\fortran\test
    )
    
    rem Assume HL libraries are built.  This should be commented out if the
    rem default is changed.
    call :add_test checkhltests.bat .\hl\test
    call :add_test checkperformtests.bat .\perform
    
    
    rem Run the tests for each version of HDF5
    rem We use "nodll" here because we cannot simply leave it blank.  Filter
    rem it out below. --SJW 9/10/07
    for %%b in (nodll dll) do (
		echo.======================================
        for %%a in (%chkdebug% %chkrelease%) do (
            set hdf5_config=%%a %%b
            call :run_tests !hdf5_config:nodll=!
            if not errorlevel 0 (
                set /a nerrors=!nerrors!+1
			) else if errorlevel 1 (
                set /a nerrors=!nerrors!+1
            )
        )
		echo.======================================
    )
    
    if "%nerrors%"=="0" (
        echo.HDF5 Tests passed for all configurations!
    ) else (
        echo.** FAILED HDF5 Tests!
	)
    
    popd
    endlocal & exit /b %nerrors%
    
