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
$! Run HDF5 C tests
$!
$ type sys$input
       Running tests
$! define sys$output check_vms.out
$ 
$ type sys$input
-------			Running testhdf5 	-------
$ run  testhdf5
$ type sys$input

$ type sys$input
-------			Running accum     	-------
$ run  accum
$ type sys$input

$ type sys$input
-------			Running app_ref    	-------
$ run  app_ref 
$ type sys$input

$ type sys$input
-------			Running big     	-------
$ run  big
$ type sys$input

$ type sys$input
-------			Running bittests    	-------
$ run  bittests
$ type sys$input

$ type sys$input
-------			Running btree2    	-------
$ run  btree2
$ type sys$input

$ type sys$input
-------			Running cache_api   	-------
$ run  cache_api
$ type sys$input

$ type sys$input
-------			Running cache   	-------
$ run  cache
$ type sys$input

$ type sys$input
-------			Running cmpd_dset    	-------
$ run  cmpd_dset
$ type sys$input

$ type sys$input
-------			Running cross_read     	-------
$ run  cross_read
$ type sys$input

$ type sys$input
-------			Running dangle     	-------
$ run  dangle
$ type sys$input

$ type sys$input
-------			Running dsets   	-------
$ run  dsets
$ type sys$input

$ type sys$input
-------			Running dt_arith    	-------
$ run  dt_arith
$ type sys$input

$ type sys$input
-------			Running dtransform     	-------
$ run  dtransform
$ type sys$input

$ type sys$input
-------			Running dtypes    	-------
$ run  dtypes
$ type sys$input

$ type sys$input
-------			Running efc    		-------
$ run  efc
$ type sys$input

$ type sys$input
-------			Running enum     	-------
$ run  enum
$ type sys$input

$ type sys$input
-------			Running extend    	-------
$ run  extend
$ type sys$input

$ type sys$input
-------			Running external    	-------
$ run  external
$ type sys$input

$ type sys$input
-------			Running fheap    	-------
$ run  fheap
$ type sys$input

$ type sys$input
-------			Running fillval     	-------
$ run  fillval
$ type sys$input

$ type sys$input
-------			Running filter_fail    	-------
$ run  filter_fail
$ type sys$input

$ type sys$input
-------			Running flush1     	-------
$ run  flush1
$ type sys$input

$ type sys$input
-------			Running flush2     	-------
$ run  flush2
$ type sys$input

$ type sys$input
-------			Running freespace     	-------
$ run  freespace
$ type sys$input

$ type sys$input
-------			Running getname     	-------
$ run  getname
$ type sys$input

$ type sys$input
-------			Running gheap    	-------
$ run  gheap
$ type sys$input

$ type sys$input
-------			Running hyperslab    	-------
$ run  hyperslab
$ type sys$input

$ type sys$input
-------			Running istore    	-------
$ run  istore
$ type sys$input

$ type sys$input
-------			Running lheap    	-------
$ run  lheap
$ type sys$input

$ type sys$input
-------			Running links    	-------
$ run  links
$ type sys$input

$! Skip it because it needs a script file to run
$! type sys$input
$!-------			Running links_env    	-------
$! run  links_env
$! type sys$input

$ type sys$input
-------			Running mf    	-------
$ run  mf
$ type sys$input

$ type sys$input
-------			Running mount     	-------
$ run  mount
$ type sys$input

$ type sys$input
-------			Running mtime    	-------
$ run  mtime 
$ type sys$input

$ type sys$input
-------			Running ntypes     	-------
$ run  ntypes
$ type sys$input

$ type sys$input
-------			Running objcopy    	-------
$ run  objcopy 
$ type sys$input

$ type sys$input
-------			Running ohdr    	-------
$ run  ohdr 
$ type sys$input

$ type sys$input
-------			Running pool    	-------
$ run  pool
$ type sys$input

$ type sys$input
-------			Running reserved     	-------
$ run  reserved
$ type sys$input

$ type sys$input
-------			Running set_extent     	-------
$ run  set_extent
$ type sys$input

$ type sys$input
-------			Running space_overflow 	-------
$ run  space_overflow
$ type sys$input

$ type sys$input
-------			Running stab    	-------
$ run  stab
$ type sys$input

$ type sys$input
-------			Running testmeta    	-------
$ run  testmeta
$ type sys$input

$ type sys$input
-------			Running unlink    	-------
$ run  unlink
$ type sys$input

$ type sys$input
-------			Running vfd     	-------
$ run  vfd
$ type sys$input

-------			Testing completed       -------
$ exit 
