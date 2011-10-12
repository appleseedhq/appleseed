$!#
$!# Copyright by The HDF Group.
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from help@hdfgroup.org.
$!#
$! Makefile for VMS systems.
$!
$! Make HDF5 Fortran tests
$!
$! define zlib_dir sys$sysusers:[pourmal.zlib-1_2_3]
$! ccopt = "/float=ieee_float/define=H5_VMS/debug/nooptimize/include=zlib_dir"
$! fcopt = "/float=ieee_float/define=H5_VMS/debug/nooptimize/include=zlib_dir"
$ ccc := cc 'ccopt /include=[-.-.src]
$ fff := fortran 'fcopt /module=[-.src]
$
$ type sys$input
	Creating  HDF5 Fortran tests
$!
$ cobj="t "
$ ffiles="fortranlib_test.f90, tH5F.f90, tH5D.f90, "+-
       "tH5R.f90, tH5S.f90, tH5T.f90, tH5VL.f90, tH5Z.f90, tH5Sselect.f90,"+-
       "tH5P.f90, tH5A.f90, tH5I.f90, tH5G.f90, tH5E.f90, tf.f90"
$ ffiles18="fortranlib_test_1_8.f90, tH5F.f90, tH5O.f90,"+-
       "tH5A_1_8.f90, tH5G_1_8.f90"

$ fobj="fortranlib_test, tH5F, tH5D, "+-
       "tH5R, tH5S, tH5T, tH5VL, tH5Z, tH5Sselect,"+-
       "tH5P, tH5A, tH5I, tH5G, tH5E, tf"
$ fobj18="fortranlib_test_1_8, tH5F, tH5O, tH5A_1_8, tH5G_1_8"
$!
$ ccc 'cobj 
$ fff 'ffiles
$ fff 'ffiles18
$ fff  fflush1.f90
$ fff  fflush2.f90
$ library/create []h5test_fortran  t, tf 
$ link fflush1,h5test_fortran.olb/lib,-
             [-.-.test]libh5test.olb/lib, -
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ link fflush2,h5test_fortran.olb/lib,-
             [-.-.test]libh5test.olb/lib, - 
             [-.src]hdf5_fortran.olb/lib,-
             [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ link/exec=fortranlib_test.exe -
  fortranlib_test.obj, tH5F.obj, tH5D.obj, -
  tH5R.obj, tH5S.obj, tH5T.obj, tH5VL.obj, tH5Z.obj, tH5Sselect.obj, -
  tH5P.obj, tH5A.obj, tH5I.obj, tH5G.obj, tH5E.obj,-
  h5test_fortran.olb/lib,[-.-.test]libh5test.olb/lib, -
  [-.src]hdf5_fortran.olb/lib,-
  [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ link/exec=fortranlib_test_1_8.exe -
  fortranlib_test_1_8.obj, tH5F.obj, tH5O.obj, tH5A_1_8.obj, tH5G_1_8.obj,-
  h5test_fortran.olb/lib,[-.-.test]libh5test.olb/lib, -
  [-.src]hdf5_fortran.olb/lib,-
  [-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
	Created HDF5 Fortran tests
$!
$ exit
