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
$! This file copies all make files from the VMS directory to the 
$! source directories and builds libraries, tests, and utilties
$!
$ copy [.c__.src]make.com       [-.c__.src]
$ copy [.c__.test]*.com         [-.c__.test]
$ copy [.c__.test]H5srcdir_str.h    [-.c__.test]
$ copy [.fortran.src]make.com   [-.fortran.src]
$ copy [.fortran.test]*.com     [-.fortran.test]
$ copy [.src]make.com           [-.src]
$ copy [.src]h5pubconf.h        [-.src]
$ copy [.test]*.com             [-.test]
$ copy [.test]H5srcdir_str.h    [-.test]
$ copy [.tools.h5copy]*.com     [-.tools.h5copy]
$ copy [.tools.h5dump]*.com     [-.tools.h5dump]
$ copy [.tools.h5ls]*.com       [-.tools.h5ls]
$ copy [.tools.h5diff]*.com     [-.tools.h5diff]
$ copy [.tools.h5repack]*.com   [-.tools.h5repack]
$ copy [.tools.h5import]*.com   [-.tools.h5import]
$ copy [.tools.h5jam]*.com      [-.tools.h5jam]
$ copy [.tools.h5stat]*.com     [-.tools.h5stat]
$ copy [.tools.lib]make.com     [-.tools.lib]
$ copy [.tools.testfiles]*.ddl  [-.tools.testfiles]
$ copy [.tools.misc]make.com    [-.tools.misc]
$!
$! Define location of ZLIB library. If you do not have it on your system, download
$! source code from http://www.zlib.net/, build and install on your system
$ define zlib_dir disk$user:[hdfgroup.zlib-1_2_5_ieee]
$! define zlib_dir sys$sysusers:[pourmal.zlib-1_2_3-ieee]
$!
$! Set up compilation flags here
$! Do not remove define=H5_VMS and standard=strict_ansi qualifiers.
$!
$ ccopt == "/float=ieee_float/define=(_LARGEFILE,H5_VMS)/include=zlib_dir"
$ fcopt == "/float=ieee_float/define=(_LARGEFILE,H5_VMS)/include=zlib_dir"
$ cxxopt == "/float=ieee_float/define=(_LARGEFILE,H5_VMS)/"+-
            "standard=strict_ansi/include=zlib_dir"
$!
$!
$ hdf5top     = F$DIRECTORY()
$ len         = F$LENGTH(hdf5top)
$ hdf5top_dir = F$EXTRACT(0, len-4, hdf5top)
$!
$ hdf5src              = hdf5top_dir + "SRC]"
$ hdf5test             = hdf5top_dir + "TEST]"
$ hdf5examples         = hdf5top_dir + "EXAMPLES]"
$ hdf5tools_lib        = hdf5top_dir + "TOOLS.LIB]"
$ hdf5tools_h5copy     = hdf5top_dir + "TOOLS.H5COPY]"
$ hdf5tools_h5diff     = hdf5top_dir + "TOOLS.H5DIFF]"
$ hdf5tools_h5dump     = hdf5top_dir + "TOOLS.H5DUMP]"
$ hdf5tools_h5import   = hdf5top_dir + "TOOLS.H5IMPORT]"
$ hdf5tools_h5jam      = hdf5top_dir + "TOOLS.H5JAM]"
$ hdf5tools_h5ls       = hdf5top_dir + "TOOLS.H5LS]"
$ hdf5tools_h5repack   = hdf5top_dir + "TOOLS.H5REPACK]"
$ hdf5tools_h5stat     = hdf5top_dir + "TOOLS.H5STAT]"
$ hdf5tools_misc       = hdf5top_dir + "TOOLS.MISC]"
$ hdf5fortran_examples = hdf5top_dir + "FORTRAN.EXAMPLES]"
$ hdf5fortran_src      = hdf5top_dir + "FORTRAN.SRC]"
$ hdf5fortran_test     = hdf5top_dir + "FORTRAN.TEST]"
$ hdf5cxx_src          = hdf5top_dir + "C__.SRC]"
$ hdf5cxx_test         = hdf5top_dir + "C__.TEST]"
$ hdf5cxx_examples     = hdf5top_dir + "C__.EXAMPLES]"
$!
$ write sys$output "Building C library"
$ set def 'hdf5src'
$ @make.com
$!
$!
$ write sys$output "Building C library tests"
$ set def 'hdf5test'
$ @make.com
$!
$ write sys$output "Building tools library"
$ set def 'hdf5tools_lib'
$ @make.com
$!
$ write sys$output "Building h5copy"
$ set def 'hdf5tools_h5copy'
$ @make.com
$!
$ write sys$output "Building h5diff"
$ set def 'hdf5tools_h5diff'
$ @make.com
$!
$ write sys$output "Building h5dump"
$ set def 'hdf5tools_h5dump'
$ @make.com
$!
$ write sys$output "Building h5import"
$ set def 'hdf5tools_h5import'
$ @make.com
$!
$ write sys$output "Building h5jam"
$ set def 'hdf5tools_h5jam'
$ @make.com
$!
$ write sys$output "Building h5ls"
$ set def 'hdf5tools_h5ls'
$ @make.com
$!
$ write sys$output "Building h5repack"
$ set def 'hdf5tools_h5repack'
$ @make.com
$!
$ write sys$output "Building h5stat"
$ set def 'hdf5tools_h5stat'
$ @make.com
$!
$ write sys$output "Building misc"
$ set def 'hdf5tools_misc'
$ @make.com
$!
$ write sys$output "Building Fortran library"
$ set def 'hdf5fortran_src'
$ @make.com
$!
$ write sys$output "Building Fortran library tests"
$ set def 'hdf5fortran_test'
$ @make.com
$!
$ write sys$output "Building C++ library"
$ set def 'hdf5cxx_src'
$ copy *.cpp *.cxx
$ @make.com
$!
$ write sys$output "Building C++ library tests"
$ set def 'hdf5cxx_test'
$ copy *.cpp *.cxx
$ @make.com
$!
$ set def 'hdf5top'
$ exit
