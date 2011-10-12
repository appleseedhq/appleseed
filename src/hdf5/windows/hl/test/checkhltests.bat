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
rem Tests for the hdf5 HL library
rem
rem    Created:  Scott Wegner, 9/10/07
rem    Modified:
rem

setlocal enabledelayedexpansion
pushd %~dp0

set /a nerrors=0

rem Clean any variables starting with "HDF5_HLTEST_", as we use these for our
rem tests.  Also clear "HDF5_HLTEST_TESTS", as we will be addding all of our tests
rem to this variable.
rem Set at least one variable in set beforehand to avoid error message.
rem --SJW 9/5/07
set hdf5_hltest_=foo
for /f "tokens=1 delims==" %%a in ('set hdf5_hltest_') do set %%a=
set hdf5_hltest_tests=

rem See if we have built the HL C++ / Fortran libraries, and set 
rem BUILD_*_CONDITIONAL appropriately
call :check_built fortran %*
call :check_built cxx %*

goto main


rem Function to add a test to the test suite.  
rem Expects the following parameters:
rem     %1 - Name of the hltest being tested
rem     %2 - Relative path of script
:add_test

    set hdf5_hltest_tests=%hdf5_hltest_tests% %1
    set hdf5_hltest_%1_test=%CD%\%2\%1

    exit /b
    

rem Run all of the tests that have been added to the suite.  Print a header
rem at the beginning of each one.  Short-circuit if a test fails.
rem Expects the following parameters:
rem     %1 - release or debug version
rem     %2 - "dll" or nothing
:run_tests
    for %%a in (%hdf5_hltest_tests%) do (
        echo.
        echo.************************************
        echo.  Testing %%a ^(%1 %2^)
        echo.************************************
        
        rem Only add our parameters for batch scripts.
        call !hdf5_hltest_%%a_test:.bat= %1 %2!
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


rem Check to see if one of our output files exist for the given parameter.  If
rem it does, we can assume that that set of files were set to build, and we can
rem test them.  In Linux, the corresponding variable is set by the Makefile.
rem Expects the following parameters:
rem     %1 - fortran or cxx
rem     %2 - debug or release
rem     %3 - dll or nothing
:check_built

    rem diffuse early if the variable is already defined
    if defined build_%1_conditional exit /b
    
    if /i "%1" equ "cxx" (
        if "%2"=="release" (
            set hdf5_hl_cpp=hdf5_hl_cpp%3.lib
        ) else (
            set hdf5_hl_cpp=hdf5_hl_cppd%3.lib
        )
        
        if exist %CD%\..\..\proj\hdf5_hl_cpp%3\%2\!hdf5_hl_cpp! (
            set build_cxx_conditional=true
        )
    ) else if /i "%1" equ "fortran" (
        if "%2"=="release" (
            set hdf5_hl_fortran=hdf5_hl_fortran%3.lib
        ) else (
            set hdf5_hl_fortran=hdf5_hl_fortrand%3.lib
        )
        
        if exist %CD%\..\..\proj\hdf5_hl_fortran%3\%2\!hdf5_hl_fortran! (
            set build_fortran_conditional=true
        )
    )
    
    exit /b

rem This is where we add tests to the suite, and run them all at the end.
rem Make sure only to run dll versions of tests you build dll for
rem Also make sure to add *.bat to batch scripts, as the above functions rely
rem on it for sending parameters.  --SJW 9/6/07
:main

    call :add_test hl_test_lite%2 .\hl_test_lite%2\%1
    call :add_test hl_test_image%2 .\hl_test_image%2\%1
    call :add_test hl_test_table%2 .\hl_test_table%2\%1
    call :add_test hl_test_ds%2 .\hl_test_ds%2\%1
    call :add_test hl_test_packet%2 .\hl_test_packet%2\%1
    
    rem Only check HL C++/Fortran if they are set to build.
    if defined build_cxx_conditional (
        call :add_test checkhlcpptests.bat ..\c++\test
    )
    if defined build_fortran_conditional (
        call :add_test checkhlfortrantests.bat ..\fortran\test
    )
    
    rem Run the tests, passing in which version to run
    call :run_tests %*
        
    if "%nerrors%"=="0" (
		echo.All HL library tests passed.
	) else (
        echo.** FAILED HL Library tests.
    )
        
    popd
    endlocal & exit /b %nerrors%
    