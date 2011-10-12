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
$! Make HDF5 Fortran examples
$!
$ fcopt = "/float=ieee_float/define=H5_VMS"
$ define zlib_dir disk$user:[hdfgroup.zlib-1_2_5_ieee]
$ fff := fortran 'fcopt /module=[-.-.include]
$
$ type sys$input
	Compiling HDF5 Fortran examples
$!
$ ffiles="dsetexample.f90, fileexample.f90, rwdsetexample.f90, "+-
         "attrexample.f90, groupexample.f90, grpsexample.f90, "+-
         "grpdsetexample.f90, hyperslab.f90, selectele.f90, grpit.f90,"+-
         "refobjexample.f90, refregexample.f90, mountexample.f90,"+-
         "compound.f90"
$!
$ fff 'ffiles
$ type sys$input

        Creating dsetexample 
$ link       dsetexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating fileexample
$ link       fileexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating rwdsetexample
$ link       rwdsetexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating attrexample
$ link       attrexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating groupexample
$ link       groupexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating grpsexample
$ link       grpsexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating grpdsetexample
$ link       grpdsetexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating hyperslab
$ link       hyperslab ,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating selectele
$ link       selectele,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating grpit
$ link       grpit,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating refobjexample
$ link       refobjexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating refregexample
$ link       refregexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating mountexample
$ link       mountexample,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ type sys$input

        Creating compound
$ link       compound,-
             [-.-.lib]hdf5_fortran.olb/lib,-
             [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$ exit
