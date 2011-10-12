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
$! Make HDF5 C examples
$! Build examples after you install libraries and examples.
$! install.com installs binaries under the HDF5 directory in the top
$! source directory.
$!
$ define zlib_dir disk$user:[hdfgroup.zlib-1_2_5_ieee]
$ ccopt = "/float=ieee_float/nowarnings/define=H5_VMS/include=zlib_dir"
$ ccc := cc 'ccopt /include=([-.-.include])
$ type sys$input
 	Compiling  C examples
$!
$ cobj= "h5_attribute.c, h5_chunk_read.c, h5_compound.c, h5_drivers.c, h5_dtransform.c,"+-
        "h5_elink_unix2win.c, h5_extend_write.c, h5_extlink.c, h5_group.c,"+-
        "h5_interm_group.c, h5_mount.c, h5_read.c, h5_ref2reg.c, h5_reference.c,"+-
        "h5_select.c, h5_shared_mesg.c, h5_write.c"
$!                               
$ ccc 'cobj 
$
$ type sys$input
       Creating h5_attribute 
$ link     h5_attribute, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_chunk_read 
$ link     h5_chunk_read, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_compound 
$ link     h5_compound, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_drivers
$ link     h5_drivers, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_dtransform 
$ link     h5_dtransform, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_elink_unix2win 
$ link     h5_elink_unix2win, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_extend_write
$ link     h5_extend_write, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_extend_write 
$ link     h5_extend_write, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_extlink 
$ link     h5_extlink, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_group 
$ link     h5_group, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_interm_group 
$ link     h5_interm_group, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_mount 
$ link     h5_mount, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_read 
$ link     h5_read, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_ref2reg
$ link     h5_ref2reg, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_reference 
$ link     h5_reference, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_select 
$ link     h5_select, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_shared_mesg 
$ link     h5_shared_mesg, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ type sys$input
       Creating h5_write 
$ link     h5_write, -
            [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib

$ exit
