@echo OFF
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

rem This batch file is used to test HDF5 High Level C examples.
rem Written by   : Fang GUO
rem Created      : 07/26/2005
rem Last Modified: 2/18/08

rem Use delayed expansion to track changes inside a loop
setlocal enabledelayedexpansion

rem Track errors
set total_error=0

if %1.==. GOTO WRONG
if "%1"=="/?" GOTO HELP
if "%1"=="release" GOTO CONFIG
if "%1"=="debug" GOTO CONFIG

GOTO WRONG

:CONFIG
    if "%2"=="dll" GOTO TEST
    if %2.==. GOTO TEST
    GOTO WRONG

:TEST
    echo Testing %1 %2 version of High Level examples 
    echo.

    ex_ds1%2\%1\ex_ds1%2.exe ex_ds1%1%2
    
    for /l %%a in (1,1,2) do (
        ex_image%%a%2\%1\ex_image%%a%2.exe ex_image%%a%1%2
        if not !errorlevel!==0 set /A total_error=!total_error!+1
    )
    
    for /l %%a in (1,1,3) do (
        ex_lite%%a%2\%1\ex_lite%%a%2.exe ex_lite%%a%1%2
        if not !errorlevel!==0 set /A total_error=!total_error!+1
    )

    for /l %%a in (1,1,9) do (
        ex_table0%%a%2\%1\ex_table0%%a%2.exe ex_table0%%a%1%2
        if not !errorlevel!==0 set /A total_error=!total_error!+1
    )
    
    for /l %%a in (10,1,12) do (
        ex_table%%a%2\%1\ex_table%%a%2.exe ex_table%%a%1%2
        if not !errorlevel!==0 set /A total_error=!total_error!+1
    )
    
    for %%a in (FL VL) do (
        ptExample%%a%2\%1\ptExample%%a%2.exe ptExample%%a%1%2
        if not !errorlevel!==0 set /A total_error=!total_error!+1
    )

    if %total_error%==0 (
        echo.
        echo. All of the HL C Examples Passed!
    ) else (
        echo.
        echo. %total_error% HL C Examples Failed!
    )

    GOTO END

:WRONG
    echo The syntax of the command is incorrect.
    echo Use test_hl_cexamples /? for usage information
    echo.
    GOTO END

:HELP
    echo Tests HDF5 High Level C examples.
    echo.
    echo test_hl_cexamples [OPTION]
    echo.
    echo Please use one of the following options!
    echo.
    echo test_hl_cexamples release     test HDF5 HL C examples -- release version
    echo test_hl_cexamples release dll test HDF5 HL C examples -- release dll version
    echo test_hl_cexamples debug       test HDF5 HL C examples -- debug version
    echo test_hl_cexamples debug dll   test HDF5 HL C examples -- debug dll version
    echo test_hl_cexamples /?          Help information
    echo.
    GOTO END

:END
