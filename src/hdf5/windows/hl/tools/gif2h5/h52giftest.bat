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
rem HDF Utilities Test script
rem
rem    Created:  Scott Wegner, 4/5/07
rem    Modified: Scott Wegner, 8/22/07
rem

setlocal enabledelayedexpansion
pushd %~dp0

rem h52gif name
set h52gif=h52gif%2
rem The path to the h52gif binary
set h52gif_bin=%CD%\..\gifconv%2\%h52gif%\%1\%h52gif%
rem gif2h5 name
set gif2h5=gif2h5%2
rem The path to the gif2h5 binary
set gif2h5_bin=%CD%\..\gifconv%2\%gif2h5%\%1\%gif2h5%

set testfile1=%CD%\..\testfiles\h52giftst.h5
set testfile2=%CD%\..\testfiles\image1.gif

rem initialze errors variable
set errors=0

goto main

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



:tooltest1
    %h52gif_bin% %*
    
    if %errorlevel% neq 0 (
        call :testing *FAILED* %h52gif_bin% %*
        set /a errors=!errors!+1
    ) else (
        call :testing PASSED %h52gif_bin% %*
    )
    
    exit /b
    

:tooltest2
    %gif2h5_bin% %*
    
    if %errorlevel% neq 0 (
        call :testing *FAILED* %gif2h5_bin% %*
        set /a errors=!errors!+1
    ) else (
        call :testing PASSED %gif2h5_bin% %*
    )
    
    exit /b
    
    
:main
    call :tooltest1 %testfile1% image1.gif -i image
    call :tooltest2 %testfile2% image1.h5
    
    popd
    endlocal & exit /b %errors%
    