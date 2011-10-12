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
$! Make h5ls tool 
$!
$! The next two lines should be uncommented only when building by hand in the
$! current directory. Use build.com in the vms directory to build
$! the distribution. Make sure that location of the zlib library is correct.
$! define zlib_dir sys$sysusers:[pourmal.zlib-1_2_3]
$! ccopt = "/float=ieee_float/define=H5_VMS/include=zlib_dir"
$ ccc := cc 'ccopt /include=([-.-.src], [-.lib])
$ type sys$input
	Creating h5jam
$!
$ cobj= "h5jam, h5unjam, tellub, getub, h5jamgentest"

$!                               
$ ccc 'cobj 
$ type sys$input
       Creating tellub
$ link     tellub -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
	Created  tellub
$ type sys$input
       Creating getub
$ link     getub -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
	Created  getub
$!
$!
$ type sys$input
       Creating h5jamgentest
$ link     h5jamgentest, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
	Created  h5jamgentest
$!
$ type sys$input
       Creating h5jam
$ link     h5jam, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
	Created  h5jam
$!
$ type sys$input
       Creating h5junam
$ link     h5unjam, -
           [-.lib]libh5tools.olb/lib,[-.-.src]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input
	Created  h5unjam
$!
$ exit
