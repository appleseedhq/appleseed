@REM Copyright by The HDF Group.
@REM Copyright by the Board of Trustees of the University of Illinois.
@REM All rights reserved.
@REM
@REM This file is part of HDF5.  The full HDF5 copyright notice, including
@REM terms governing use, modification, and redistribution, is contained in
@REM the files COPYING and Copyright.html.  COPYING can be found at the root
@REM of the source code distribution tree; Copyright.html can be found at the
@REM root level of an installed copy of the electronic HDF5 document set and
@REM is linked from the top-level documents page.  It can also be found at
@REM http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
@REM access to either file, you may request a copy from help@hdfgroup.org.


@ECHO OFF
@:: This batch file is used to install HDF5 High Level C 
@:: Examples' executable files.
@:: Written by   : Fang GUO
@:: Created on   : 12/20/2005
@:: Last Modified: 

cd hl/fortran/examples

mkdir HLf90examplesRELEASE
mkdir HLf90examplesRELEASEDLL

cd ex_lite
copy release\ex_lite.exe ..\HLf90examplesRELEASE\
cd ..

cd ex_litedll
copy release\ex_litedll.exe ..\HLf90examplesRELEASEDLL\
cd ..

