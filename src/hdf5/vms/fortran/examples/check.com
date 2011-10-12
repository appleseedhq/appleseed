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
$! Command file to run Fortran examples; examples need to run in a defined
$! order due to file dependencies.
$!
$ type sys$input


	Running Fortran examples

$ type sys$input

	Running dsetexample 
$ run dsetexample
$
$ type sys$input 

	Running fileexample
$ run fileexample
$
$ type sys$input

	Running rwdsetexample
$ run rwdsetexample
$
$ type sys$input

	Running attrexample
$ run attrexample
$
$ type sys$input

	Running groupexample
$ run groupexample
$
$ type sys$input

	Running grpsexample
$ run grpsexample
$
$ type sys$input

	Running grpdsetexample
$ run grpdsetexample
$
$ type sys$input

	Running hyperslab
$ run hyperslab
$
$ type sys$input

	Running selectele
$ run selectele
$
$ type sys$input

	Running grpit
$ run grpit
$
$ type sys$input

	Running refobjexample
$ run refobjexample
$
$ type sys$input

	Running refregexample
$ run refregexample
$
$ type sys$input

	Running mountexample
$ run mountexample
$
$ type sys$input

	Running compound
$ run compound
$ exit
