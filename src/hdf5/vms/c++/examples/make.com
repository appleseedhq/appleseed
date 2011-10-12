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
$!
$! Make HDF5 C++ examples
$!
$ define zlib_dir disk$user:[hdfgroup.zlib-1_2_5_ieee]
$ cxxopt = "/float=ieee_float/standard=strict_ansi/define=H5_VMS/include=zlib_dir"
$ ccc := cxx 'cxxopt /include=([-.-.include])
$!
$!
$ cxxobj= "chunks.cxx, compound.cxx, create.cxx, extend_ds.cxx, h5group.cxx, "+-
          "readdata.cxx, writedata.cxx"
$! 
$!                              
$ ccc 'cxxobj
$ type sys$input

       Creating chunks 
$ cxxlink  chunks, -
           [-.-.lib]hdf5_cplus.olb/lib, -
           [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating compound 
$ cxxlink  compound, -
           [-.-.lib]hdf5_cplus.olb/lib, -
           [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating create 
$ cxxlink  create, -
           [-.-.lib]hdf5_cplus.olb/lib, -
           [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating extend_ds  
$ cxxlink  extend_ds, -
           [-.-.lib]hdf5_cplus.olb/lib, -
           [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating h5group 
$ cxxlink  h5group, -
           [-.-.lib]hdf5_cplus.olb/lib, -
           [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating readdata  
$ cxxlink  readdata, -
           [-.-.lib]hdf5_cplus.olb/lib, -
           [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!
$ type sys$input

       Creating writedata  
$ cxxlink  writedata, -
           [-.-.lib]hdf5_cplus.olb/lib, -
           [-.-.lib]hdf5.olb/lib,zlib_dir:libz.olb/lib
$!                              
$ type sys$input
$!
$ exit
