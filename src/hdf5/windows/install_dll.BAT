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
rem access to either file, you may request a copy from helphdfgroup.org.
rem
rem
rem File name: install_dll.bat
rem This batch file is used to copy ALL HDF5 DLLs into system folder.
rem By Xuan Bai
rem Created: Aug. 12, 2004
rem Last Scott Wegner, 9/10/07

setlocal enabledelayedexpansion
pushd %~dp0

set install_dir=%systemroot%\system

goto :main

rem This function actally copies the DLL over, and prints a status message
rem Expected parameters:
rem     %1 - name of DLL to copy
:copy_dll
    
    if exist %1 (
        echo.Installing %~nx1
        copy /y %1 %install_dir% > nul
    )
    
    exit /b

    
rem Add the DLLS to be copied here.
:main
    
    rem HDF5 Library
    call :copy_dll proj\hdf5dll\debug\hdf5ddll.dll
    call :copy_dll proj\hdf5dll\release\hdf5dll.dll
    call :copy_dll test\libtestdll\debug\libtestddll.dll
    call :copy_dll test\libtestdll\release\libtestdll.dll
    
    rem C++
    call :copy_dll proj\hdf5_cppdll\debug\hdf5_cppddll.dll
    call :copy_dll proj\hdf5_cppdll\release\hdf5_cppdll.dll
    
    rem Fortran
    call :copy_dll proj\hdf5_f90cstubdll\debug\hdf5_f90cstubddll.dll
    call :copy_dll proj\hdf5_f90cstubdll\release\hdf5_f90cstubdll.dll
    call :copy_dll proj\hdf5_fortrandll\debug\hdf5_fortranddll.dll
    call :copy_dll proj\hdf5_fortrandll\release\hdf5_fortrandll.dll
    call :copy_dll fortran\test\libtest_cstubdll\debug\libtest_cstubddll.dll
    call :copy_dll fortran\test\libtest_cstubdll\release\libtest_cstubdll.dll
    call :copy_dll fortran\test\libtest_fortrandll\debug\libtest_fortranddll.dll
    call :copy_dll fortran\test\libtest_fortrandll\release\libtest_fortrandll.dll
    
    rem HL
    call :copy_dll proj\hdf5_hldll\Debug\hdf5_hlddll.dll
    call :copy_dll proj\hdf5_hldll\Release\hdf5_hldll.dll
    
    rem HL C++
    call :copy_dll proj\hdf5_hl_cppdll\Release\hdf5_hl_cppdll.dll
    call :copy_dll proj\hdf5_hl_cppdll\Debug\hdf5_hl_cppddll.dll

    rem HL Fortran
    call :copy_dll proj\hdf5_hl_fortrandll\Debug\hdf5_hl_fortranddll.dll
    call :copy_dll proj\hdf5_hl_f90cstubdll\Debug\hdf5_hl_f90cstubddll.dll
    call :copy_dll proj\hdf5_hl_fortrandll\Release\hdf5_hl_fortrandll.dll
    call :copy_dll proj\hdf5_hl_f90cstubdll\Release\hdf5_hl_f90cstubdll.dll
    
    popd
    endlocal & exit /b

