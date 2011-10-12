@echo off
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

rem File Name   : copy_hdf.bat
rem Purpose     : Copy all Files in the following formats from Windows to 
rem               approapriate directory: .bat .c .f90 .h .txt .js 
rem             : 
rem Written By  : Muqun Yang
rem Last Update : 2/18/08 by Scott Wegner

pushd %~dp0

copy /y fortran\src\H5fortran_types.f90 ..\fortran\src > nul
xcopy /s /i /y *.bat ..\ > nul
xcopy /s /i /y *.h ..\ > nul
copy /y examples\testExamples_exp_output.txt ..\examples > nul

popd
