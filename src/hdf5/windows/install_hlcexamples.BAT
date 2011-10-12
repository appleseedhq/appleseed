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


rem This batch file is used to install HDF5 High Level C 
rem Examples' executable files.
rem Written by   : Fang GUO
rem Created on   : 07/25/2005
rem Last Modified: 2/18/2008

setlocal enabledelayedexpansion
pushd %~dp0

set exdir=hl\examples
set nerrors=0
goto main

rem Simply create the needed directories
:makedirs
    mkdir %exdir%\HLCexamplesRELEASE
    mkdir %exdir%\HLCexamplesRELEASEDLL
    exit /b

rem This function actally copies the file over, first making sure it exists.  If not, we increment nerrors
rem and print an error message
rem Expected parameters:
rem     %1 - name of file to copy
rem     %2 - destination to copy to
:safe_copy
    
    if exist %exdir%\%1 (
        copy /y %exdir%\%1 %exdir%\%2 > nul
    ) else (
        echo.Warning: Cannot find example file: %exdir%\%1
        set /a nerrors=%nerrors%+1
    )
    
    exit /b %nerrors%
    
    
:main
    if not exist %exdir% (
        echo.Error: Examples directory doesn't exist: %CD%\%exdir%
        set /a nerrors=!nerrors!+1
        goto :end
    )
    
    call :makedirs
    
    rem copy the files
    for %%a in (RELEASE) do (
        for %%b in (DLL static) do (
            set ver=%%b
            set ver=!ver:static=!
            
            call :safe_copy ex_ds1!ver!\%%a\ex_ds1!ver!.exe HLCexamples%%a!ver!
            
            for /l %%c in (1,1,2) do (
                call :safe_copy ex_image%%c!ver!\%%a\ex_image%%c!ver!.exe HLCexamples%%a!ver!
            )
            
            for /l %%c in (1,1,3) do (
                call :safe_copy ex_lite%%c!ver!\%%a\ex_lite%%c!ver!.exe HLCexamples%%a!ver!
            )
    
            for /l %%c in (1,1,9) do (
                call :safe_copy ex_table0%%c!ver!\%%a\ex_table0%%c!ver!.exe HLCexamples%%a!ver!
            )
            
            for /l %%c in (10,1,12) do (
                call :safe_copy ex_table%%c!ver!\%%a\ex_table%%c!ver!.exe HLCexamples%%a!ver!
            )
            
            for %%c in (FL VL) do (
                call :safe_copy ptExample%%c!ver!\%%a\ptExample%%c!ver!.exe HLCexamples%%a!ver!
            )
        )
    )
    
:end
popd
endlocal & exit /b %nerrors%