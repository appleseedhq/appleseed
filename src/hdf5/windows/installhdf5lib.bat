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


rem This batch file is used to install HDF5 libraries and tools
rem Last Updated: 3/3/08

setlocal enabledelayedexpansion
pushd %~dp0

set install_dir=%systemroot%\system

goto main

rem Create the directory structure that we'll need to install
:create_directories

    for %%a in (debug release) do (
        for %%b in (bin bindll dll lib include mods modsdll) do (
            if not exist hdf5lib\%%a\%%b (
                mkdir hdf5lib\%%a\%%b
            )
        )
    )
    
    exit /b

rem This function actally copies the file over, first making sure it exists.  If not, we increment nerrors
rem Expected parameters:
rem     %1 - name of file to copy
rem     %2 - destination to copy to
:safe_copy
    
    if exist %1 (
        copy /y %1 %2 > nul
    ) else (
        set /a nerrors=%nerrors%+1
    )
    
    exit /b
    

rem Only delete a file if it actually exists.  Return the status of delete if it was called
rem Expected paramters:
rem     %1 - name of file to delete
:safe_delete
    if exist %1 (
        del /f %1 > nul
    )
    
    exit /b


rem Install C Libraries and Tools
:install_c
    set nerrors=0
    
    rem ===DEBUG===
    rem include
    call :safe_copy src\*.h hdf5lib\debug\include
    call :safe_delete hdf5lib\debug\include\*private.h
    rem lib
    call :safe_copy proj\hdf5\debug\hdf5d.lib hdf5lib\debug\lib
    rem dll
    call :safe_copy proj\hdf5dll\debug\hdf5ddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5dll\debug\hdf5ddll.dll hdf5lib\debug\dll
    rem bin
    call :safe_copy hl\tools\gifconv\gif2h5\debug\gif2h5.exe hdf5lib\debug\bin
    call :safe_copy hl\tools\gifconv\h52gif\debug\h52gif.exe hdf5lib\debug\bin
    call :safe_copy tools\h5copy\debug\h5copy.exe hdf5lib\debug\bin
    call :safe_copy tools\h5debug\debug\h5debug.exe hdf5lib\debug\bin
    call :safe_copy tools\h5diff\debug\h5diff.exe hdf5lib\debug\bin
    call :safe_copy tools\h5dump\debug\h5dump.exe hdf5lib\debug\bin
    call :safe_copy tools\h5import\debug\h5import.exe hdf5lib\debug\bin
    call :safe_copy tools\h5jam\debug\h5jam.exe hdf5lib\debug\bin
    call :safe_copy tools\h5ls\debug\h5ls.exe hdf5lib\debug\bin
    call :safe_copy tools\h5mkgrp\debug\h5mkgrp.exe hdf5lib\debug\bin
    call :safe_copy tools\h5repack\debug\h5repack.exe hdf5lib\debug\bin
    call :safe_copy tools\h5repart\debug\h5repart.exe hdf5lib\debug\bin
    call :safe_copy tools\h5stat\debug\h5stat.exe hdf5lib\debug\bin
    call :safe_copy tools\h5unjam\debug\h5unjam.exe hdf5lib\debug\bin
    rem bindll
    call :safe_copy hl\tools\gifconvdll\h52gifdll\debug\h52gifdll.exe hdf5lib\debug\bindll
    call :safe_copy hl\tools\gifconvdll\gif2h5dll\debug\gif2h5dll.exe hdf5lib\debug\bindll
    call :safe_copy tools\h5debugdll\debug\h5debugdll.exe hdf5lib\debug\bindll
    call :safe_copy tools\h5diffdll\debug\h5diffdll.exe hdf5lib\debug\bindll
    call :safe_copy tools\h5dumpdll\debug\h5dumpdll.exe hdf5lib\debug\bindll
    call :safe_copy tools\h5importdll\debug\h5importdll.exe hdf5lib\debug\bindll
    call :safe_copy tools\h5lsdll\debug\h5lsdll.exe hdf5lib\debug\bindll
    call :safe_copy tools\h5repackdll\debug\h5repackdll.exe hdf5lib\debug\bindll
    call :safe_copy tools\h5repartdll\debug\h5repartdll.exe hdf5lib\debug\bindll
    
    rem ===RELEASE===
    rem include
    call :safe_copy src\*.h hdf5lib\release\include
    call :safe_delete hdf5lib\release\include\*private.h
    rem lib
    call :safe_copy proj\hdf5\release\hdf5.lib hdf5lib\release\lib
    rem dll
    call :safe_copy proj\hdf5dll\release\hdf5dll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5dll\release\hdf5dll.dll hdf5lib\release\dll
    rem bin
    call :safe_copy hl\tools\gifconv\gif2h5\release\gif2h5.exe hdf5lib\release\bin
    call :safe_copy hl\tools\gifconv\h52gif\release\h52gif.exe hdf5lib\release\bin
    call :safe_copy tools\h5copy\release\h5copy.exe hdf5lib\release\bin
    call :safe_copy tools\h5debug\release\h5debug.exe hdf5lib\release\bin
    call :safe_copy tools\h5diff\release\h5diff.exe hdf5lib\release\bin
    call :safe_copy tools\h5dump\release\h5dump.exe hdf5lib\release\bin
    call :safe_copy tools\h5import\release\h5import.exe hdf5lib\release\bin
    call :safe_copy tools\h5jam\release\h5jam.exe hdf5lib\release\bin
    call :safe_copy tools\h5ls\release\h5ls.exe hdf5lib\release\bin
    call :safe_copy tools\h5mkgrp\release\h5mkgrp.exe hdf5lib\release\bin
    call :safe_copy tools\h5repack\release\h5repack.exe hdf5lib\release\bin
    call :safe_copy tools\h5repart\release\h5repart.exe hdf5lib\release\bin
    call :safe_copy tools\h5stat\release\h5stat.exe hdf5lib\release\bin
    call :safe_copy tools\h5unjam\release\h5unjam.exe hdf5lib\release\bin
    rem bindll
    call :safe_copy hl\tools\gifconvdll\h52gifdll\release\h52gifdll.exe hdf5lib\release\bindll
    call :safe_copy hl\tools\gifconvdll\gif2h5dll\release\gif2h5dll.exe hdf5lib\release\bindll
    call :safe_copy tools\h5debugdll\release\h5debugdll.exe hdf5lib\release\bindll
    call :safe_copy tools\h5diffdll\release\h5diffdll.exe hdf5lib\release\bindll
    call :safe_copy tools\h5dumpdll\release\h5dumpdll.exe hdf5lib\release\bindll
    call :safe_copy tools\h5importdll\release\h5importdll.exe hdf5lib\release\bindll
    call :safe_copy tools\h5lsdll\release\h5lsdll.exe hdf5lib\release\bindll
    call :safe_copy tools\h5repackdll\release\h5repackdll.exe hdf5lib\release\bindll
    call :safe_copy tools\h5repartdll\release\h5repartdll.exe hdf5lib\release\bindll
    
    exit /b %nerrors%
    

rem Install HL Libraries and Tools
:install_hl
    set nerrors=0
    
    rem ===DEBUG===
    rem include
    call :safe_copy hl\src\*.h hdf5lib\debug\include
    rem lib
    call :safe_copy proj\hdf5_hl\debug\hdf5_hld.lib hdf5lib\debug\lib
    rem dll
    call :safe_copy proj\hdf5_hldll\debug\hdf5_hlddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5_hldll\debug\hdf5_hlddll.dll hdf5lib\debug\dll

    rem ===RELEASE===
    rem include
    call :safe_copy hl\src\*.h hdf5lib\release\include
    rem lib
    call :safe_copy proj\hdf5_hl\release\hdf5_hl.lib hdf5lib\release\lib
    rem dll
    call :safe_copy proj\hdf5_hldll\release\hdf5_hldll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5_hldll\release\hdf5_hldll.dll hdf5lib\release\dll

    exit /b %nerrors%


rem Install C++ Libraries and Tools
:install_cpp
    set nerrors=0

    REM ===DEBUG===
    rem include
    call :safe_copy "c++\src\*.h" hdf5lib\debug\include
    rem lib
    call :safe_copy proj\hdf5_cpp\debug\hdf5_cppd.lib hdf5lib\debug\lib
    rem dll
    call :safe_copy proj\hdf5_cppdll\debug\hdf5_cppddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5_cppdll\debug\hdf5_cppddll.dll hdf5lib\debug\dll

    rem ===RELEASE===
    rem include
    call :safe_copy "c++\src\*.h" hdf5lib\release\include
    rem lib
    call :safe_copy proj\hdf5_cpp\release\hdf5_cpp.lib hdf5lib\release\lib
    rem dll
    call :safe_copy proj\hdf5_cppdll\release\hdf5_cppdll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5_cppdll\release\hdf5_cppdll.dll hdf5lib\release\dll

    exit /b %nerrors%
    
    
rem Install HL C++ Libraries and Tools
:install_hlcpp
    set nerrors=0
    
    rem ===DEBUG===
    rem include
    call :safe_copy "hl\c++\src\*.h" hdf5lib\debug\include
    rem lib
    call :safe_copy proj\hdf5_hl_cpp\debug\hdf5_hl_cppd.lib hdf5lib\debug\lib
    rem dll
    call :safe_copy proj\hdf5_hl_cppdll\debug\hdf5_hl_cppddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5_hl_cppdll\debug\hdf5_hl_cppddll.dll hdf5lib\debug\dll

    rem ===RELEASE===
    rem include
    call :safe_copy "hl\c++\src\*.h" hdf5lib\release\include
    rem lib
    call :safe_copy proj\hdf5_hl_cpp\release\hdf5_hl_cpp.lib hdf5lib\release\lib
    rem dll
    call :safe_copy proj\hdf5_hl_cppdll\release\hdf5_hl_cppdll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5_hl_cppdll\release\hdf5_hl_cppdll.dll hdf5lib\release\dll
    
    exit /b %nerrors%
    
    
rem Install Fortran Libraries and Tools
:install_fortran
    set nerrors=0
    
    rem ===DEBUG===
    rem include
    call :safe_copy proj\hdf5_fortran\debug\*.mod hdf5lib\debug\mods
    rem lib
    call :safe_copy proj\hdf5_fortran\debug\hdf5_fortrand.lib hdf5lib\debug\lib
    call :safe_copy proj\hdf5_f90cstub\debug\hdf5_f90cstubd.lib hdf5lib\debug\lib
    rem modsdll
    call :safe_copy proj\hdf5_fortrandll\debug\*.mod hdf5lib\debug\modsdll
    rem dll
    call :safe_copy proj\hdf5_fortrandll\debug\hdf5_fortranddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5_fortrandll\debug\hdf5_fortranddll.dll hdf5lib\debug\dll
    call :safe_copy proj\hdf5_f90cstubdll\debug\hdf5_f90cstubddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5_f90cstubdll\debug\hdf5_f90cstubddll.dll hdf5lib\debug\dll

    rem ===RELEASE===
    rem include
    call :safe_copy proj\hdf5_fortran\release\*.mod hdf5lib\release\mods
    rem lib
    call :safe_copy proj\hdf5_fortran\release\hdf5_fortran.lib hdf5lib\release\lib
    call :safe_copy proj\hdf5_f90cstub\release\hdf5_f90cstub.lib hdf5lib\release\lib
    rem modsdll
    call :safe_copy proj\hdf5_fortrandll\release\*.mod hdf5lib\release\modsdll
    rem dll
    call :safe_copy proj\hdf5_fortrandll\release\hdf5_fortrandll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5_fortrandll\release\hdf5_fortrandll.dll hdf5lib\release\dll
    call :safe_copy proj\hdf5_f90cstubdll\release\hdf5_f90cstubdll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5_f90cstubdll\release\hdf5_f90cstubdll.dll hdf5lib\release\dll
    
    exit /b %nerrors%
    
    
rem Install HL Fortran Libraries and Tools
:install_hlfortran
    set nerrors=0
    
    rem ===DEBUG===
    rem include
    call :safe_copy proj\hdf5_hl_fortran\debug\*.mod  hdf5lib\debug\mods
    rem lib
    call :safe_copy proj\hdf5_hl_fortran\debug\hdf5_hl_fortrand.lib hdf5lib\debug\lib
    call :safe_copy proj\hdf5_hl_f90cstub\debug\hdf5_hl_f90cstubd.lib hdf5lib\debug\lib
    rem modsdll
    call :safe_copy proj\hdf5_hl_fortrandll\debug\*.mod  hdf5lib\debug\modsdll
    rem dll
    call :safe_copy proj\hdf5_hl_fortrandll\debug\hdf5_hl_fortranddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5_hl_fortrandll\debug\hdf5_hl_fortranddll.dll hdf5lib\debug\dll
    call :safe_copy proj\hdf5_hl_f90cstubdll\debug\hdf5_hl_f90cstubddll.lib hdf5lib\debug\dll
    call :safe_copy proj\hdf5_hl_f90cstubdll\debug\hdf5_hl_f90cstubddll.dll hdf5lib\debug\dll

    rem ===RELEASE===
    rem include
    call :safe_copy proj\hdf5_hl_fortran\release\*.mod  hdf5lib\release\mods
    rem lib
    call :safe_copy proj\hdf5_hl_fortran\release\hdf5_hl_fortran.lib hdf5lib\release\lib
    call :safe_copy proj\hdf5_hl_f90cstub\release\hdf5_hl_f90cstub.lib hdf5lib\release\lib
    rem modsdll
    call :safe_copy proj\hdf5_hl_fortrandll\release\*.mod  hdf5lib\release\modsdll
    rem dll
    call :safe_copy proj\hdf5_hl_fortrandll\release\hdf5_hl_fortrandll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5_hl_fortrandll\release\hdf5_hl_fortrandll.dll hdf5lib\release\dll
    call :safe_copy proj\hdf5_hl_f90cstubdll\release\hdf5_hl_f90cstubdll.lib hdf5lib\release\dll
    call :safe_copy proj\hdf5_hl_f90cstubdll\release\hdf5_hl_f90cstubdll.dll hdf5lib\release\dll
    
    exit /b %nerrors%
    
    
:main

    call :create_directories
    
    call :install_c
    if %errorlevel% equ 0 (
        echo.C libraries and tools installed
    ) else (
        echo.C libraries and tools NOT installed
    )
    
    call :install_hl
    if %errorlevel% equ 0 (
        echo.High Level C libraries and tools installed
    ) else (
        echo.High Level C libraries and tools NOT installed
    )
    
    call :install_cpp
    if %errorlevel% equ 0 (
        echo.C++ libraries and tools installed 
    ) else (
        echo.C++ libraries and tools NOT installed 
    )
    
    call :install_hlcpp
    if %errorlevel% equ 0 (
        echo.High Level C++ libraries and tools installed
    ) else (
        echo.High Level C++ libraries and tools NOT installed
    )

    call :install_fortran
    if %errorlevel% equ 0 (
        echo.Fortran libraries and tools installed
    ) else (
        echo.Fortran libraries and tools NOT installed
    )
    
    call :install_hlfortran
    if %errorlevel% equ 0 (
        echo.High Level Fortran libraries and tools installed
    ) else (
        echo.High Level Fortran libraries and tools NOT installed
    )
    
    popd
    endlocal & exit /b 0
