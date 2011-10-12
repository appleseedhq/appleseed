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
$! This command file installs built binaries, header files, examples and
$! scripts under [.hdf5] directory in the main distribution
$!
$ 
$!
$!
$ hdf5top      = F$DIRECTORY()
$ len          = F$LENGTH(hdf5top)
$ hdf5top_dir  = F$EXTRACT(0, len-4, hdf5top)
$!
$ hdf5src               = hdf5top_dir + "SRC]"
$ hdf5examples          = hdf5top_dir + "EXAMPLES]"
$ hdf5tools_h5copy      = hdf5top_dir + "TOOLS.H5COPY]"
$ hdf5tools_h5diff      = hdf5top_dir + "TOOLS.H5DIFF]"
$ hdf5tools_h5dump      = hdf5top_dir + "TOOLS.H5DUMP]"
$ hdf5tools_h5ls        = hdf5top_dir + "TOOLS.H5LS]"
$ hdf5tools_h5repack    = hdf5top_dir + "TOOLS.H5REPACK]"
$ hdf5tools_h5jam       = hdf5top_dir + "TOOLS.H5JAM]"
$ hdf5tools_h5import    = hdf5top_dir + "TOOLS.H5IMPORT]"
$ hdf5tools_h5stat      = hdf5top_dir + "TOOLS.H5STAT]"
$ hdf5tools_h5misc      = hdf5top_dir + "TOOLS.MISC]"
$ hdf5fortran_examples  = hdf5top_dir + "FORTRAN.EXAMPLES]"
$ hdf5fortran_src       = hdf5top_dir + "FORTRAN.SRC]"
$ hdf5cxx_src           = hdf5top_dir + "C__.SRC]"
$ hdf5cxx_examples      = hdf5top_dir + "C__.EXAMPLES]"
$!
$ hdf5install            = hdf5top_dir + "HDF5]"
$ hdf5install_include    = hdf5top_dir + "HDF5.INCLUDE]"
$ hdf5install_examples   = hdf5top_dir + "HDF5.EXAMPLES]"
$ hdf5install_examples_c         = hdf5top_dir + "HDF5.EXAMPLES.C]"
$ hdf5install_examples_f90       = hdf5top_dir + "HDF5.EXAMPLES.F90]"
$ hdf5install_examples_cxx       = hdf5top_dir + "HDF5.EXAMPLES.CXX]"
$ hdf5install_lib        = hdf5top_dir + "HDF5.LIB]"
$ hdf5install_bin        = hdf5top_dir + "HDF5.BIN]"
$
$ if F$SEARCH(hdf5install) .NES. ""
$    then write sys$output "''hdf5install' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install'..."
$    create/dir 'hdf5install'
$ endif
$
$ if F$SEARCH(hdf5install_include) .NES. ""
$    then write sys$output "''hdf5install_include' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install_include'..."
$    create/dir 'hdf5install_include' 
$ endif
$ if F$SEARCH(hdf5install_lib) .NES. ""
$    then write sys$output "''hdf5install_lib' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install_lib'..."
$    create/dir 'hdf5install_lib' 
$ endif
$ if F$SEARCH(hdf5install_bin) .NES. ""
$    then write sys$output "''hdf5install_bin' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install_bin'..."
$    create/dir 'hdf5install_bin' 
$ endif
$ if F$SEARCH(hdf5install_examples) .NES. ""
$    then write sys$output "''hdf5install_examples' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install_examples'..."
$    create/dir 'hdf5install_examples' 
$ endif
$ if F$SEARCH(hdf5install_examples_c) .NES. ""
$    then write sys$output "''hdf5install_examples_c' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install_examples_c'..."
$    create/dir 'hdf5install_examples_c' 
$ endif
$ if F$SEARCH(hdf5install_examples_f90) .NES. ""
$    then write sys$output "''hdf5install_examples_f90' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install_examples_f90'..."
$    create/dir 'hdf5install_examples_f90' 
$ endif
$ if F$SEARCH(hdf5install_examples_cxx) .NES. ""
$    then write sys$output "''hdf5install_examples_cxx' directory exists, will not create"
$ else
$    write sys$output "Creating ''hdf5install_examples_cxx'..."
$    create/dir 'hdf5install_examples_cxx' 
$ endif

$!
$ write sys$output "Installing C library and header files"
$ set def 'hdf5src'
$ copy HDF5.olb 'hdf5install_lib'
$ copy *.h      'hdf5install_include'
$ del 'hdf5install_include'*private*.*;*
$!
$ write sys$output "Installing Fortran library module files"
$ set def 'hdf5fortran_src'
$ copy HDF5_FORTRAN.olb 'hdf5install_lib'
$ copy *.F90$MOD      'hdf5install_include'
$!
$ write sys$output "Installing C++ library and header files"
$ set def 'hdf5cxx_src'
$ copy HDF5_CPLUS.olb 'hdf5install_lib'
$ copy *.h      'hdf5install_include'
$!
$ write sys$output "Installing utilties"
$ set def 'hdf5tools_h5copy'
$ copy h5copy.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5diff'
$ copy h5diff.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5dump'
$ copy h5dump.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5repack'
$ copy h5repack.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5ls'
$ copy h5ls.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5jam'
$ copy h5jam.exe   'hdf5install_bin'
$ copy h5unjam.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5import'
$ copy h5import.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5stat'
$ copy h5stat.exe 'hdf5install_bin'
$!
$ set def 'hdf5tools_h5misc'
$ copy h5debug.exe 'hdf5install_bin'
$ copy h5mkgrp.exe 'hdf5install_bin'
$!
$ write sys$output "Installing examples"
$ set def 'hdf5examples'
$ copy *.c 'hdf5install_examples_c'
$!
$ set def 'hdf5fortran_examples'
$ copy *.f90 'hdf5install_examples_f90'
$!
$ set def 'hdf5cxx_examples'
$ copy *.cpp 'hdf5install_examples_cxx'
$ rename 'hdf5install_examples_cxx'*.cpp 'hdf5install_examples_cxx'*.cxx
$
$ 
$!
$ set def 'hdf5top'
$ copy [.examples]*.com 'hdf5install_examples_c'
$ copy [.fortran.examples]*.com 'hdf5install_examples_f90'
$ copy [.c__.examples]*.com 'hdf5install_examples_cxx'
$ exit
