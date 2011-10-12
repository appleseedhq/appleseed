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
$!
$! This file builds C, Fortran, C++ HDF5 libraries and runs the tests
$! Specify location of the top HDF5 source directory
$
$ hdf5top == "disk$user:[hdfgroup.hdf5]"
$ len = F$LENGTH(hdf5top)
$ tmp = F$EXTRACT(0, len-1, hdf5top)
$ hdf5vms     = tmp + ".VMS]"
$ hdf5ctest   = tmp + ".TEST]"
$ hdf5f90test = tmp + ".FORTRAN.TEST]"
$ hdf5cxxtest = tmp  + ".C__.TEST]"
$ hdf5toolstest = tmp  + ".TOOLS.TESTFILES]"
$ hdf5toolstest_h5diff = tmp  + ".TOOLS.H5DIFF.TESTFILES]"
$ hdf5toolstest_h5repack = tmp  + ".TOOLS.H5REPACK.TESTFILES]"
$ hdf5toolstest_h5copy = tmp  + ".TOOLS.H5COPY.TESTFILES]"
$ hdf5toolstest_h5import  = tmp  + ".TOOLS.H5IMPORT.TESTFILES]"
$ hdf5toolstest_h5jam  = tmp  + ".TOOLS.H5JAM.TESTFILES]"
$ set def 'hdf5vms'
$@make
$ set def 'hdf5ctest'
$@check
$ set def 'hdf5f90test'
$@check
$ set def 'hdf5cxxtest'
$@check
$ set def 'hdf5toolstest'
$ copy [-.h5dump]check_h5dump.com     check_h5dump.com 
$ copy [-.h5ls]check_h5ls.com         check_h5ls.com
$@check_h5dump.com
$@check_h5ls.com
$!
$ set def 'hdf5toolstest_h5diff'
$ copy [-]check_h5diff.com     check_h5diff.com
$@check_h5diff.com
$!
$ set def 'hdf5toolstest_h5repack'
$ copy [-]check_h5repack.com check_h5repack.com
$@check_h5repack.com
$!
$ set def 'hdf5toolstest_h5copy'
$ copy [-]check_h5copy.com check_h5copy.com
$@check_h5copy.com
$!
$ set def 'hdf5toolstest_h5import'
$ copy [-]check_h5import.com check_h5import.com
$@check_h5import.com
$!
$ set def 'hdf5toolstest_h5jam'
$ copy [.-]check_h5jam.com check_h5jam.com
$@check_h5jam.com
$
$ exit
