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
$! Make HDF5 tools library
$!
$! ccopt = "/float=ieee_float"
$ ccc := cc 'ccopt /include=([-.-.src])
$!
$ type sys$input
	Creating  HDF5 Tools library
$!
$ cobj="h5tools, h5tools_str, h5tools_utils, h5diff, h5diff_array, "+-
        "h5diff_attr, h5diff_dset, h5diff_util, h5trav,"+- 
        "h5tools_filters, h5tools_ref, h5tools_type"
$!
$ ccc 'cobj 
$ library/create []libh5tools  'cobj
$ type sys$input
	Created HDF5 tools library
$!
$ exit
