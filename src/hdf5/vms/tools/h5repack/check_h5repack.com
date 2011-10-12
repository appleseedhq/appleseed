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
$ !
$ ! This command file tests h5repack utility. The command file has to
$ ! run in the [hdf5-top.tools.h5repack.testfiles] directory.
$ !
$ !
$ type sys$input

===================================
       Testing h5repack utiltity
===================================

$ ! Define h5repack and h5diff symbols
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-11, current_dir)
$ temp1 = F$EXTRACT(0, len-19, current_dir)
$ h5diff_dir = temp1 + "H5DIFF]"
$ h5diff :== $sys$disk:'h5diff_dir'h5diff.exe
$ h5repack_dir = temp + "]"
$ h5repack :== $sys$disk:'h5repack_dir'h5repack.exe
$ !
$ !
$ ! h5repack tests
$ !
$

$!# copy files (these files have no filters) 
$ CALL TOOLTEST "" h5repack_fill.h5
$ CALL TOOLTEST "" h5repack_objs.h5
$ CALL TOOLTEST "" h5repack_attr.h5
$ CALL TOOLTEST "" h5repack_hlink.h5
$ CALL TOOLTEST "" h5repack_layout.h5
$ CALL TOOLTEST "" h5repack_early.h5
$ 
$! # use h5repack_layout.h5 to write some filters  (this file has  no filters)
$ 
$! # gzip with individual object
$ CALL TOOLTEST "-f dset1:"""GZIP"""=1  -l dset1:"""CHUNK"""=20x10" h5repack_layout.h5
$!   
$
$! # gzip for all 
$ CALL TOOLTEST "-f """GZIP"""=1" h5repack_layout.h5
$! 
$! # shuffle with individual object
$ CALL TOOLTEST "-f dset2:"""SHUF"""  -l dset2:"""CHUNK"""=20x10" h5repack_layout.h5
$ 
$!   
$! 
$! # shuffle for all
$ CALL TOOLTEST "-f """SHUF"""" h5repack_layout.h5
$!   
$! # fletcher32  with individual object
$ CALL TOOLTEST "-f dset2:"""FLET"""  -l dset2:"""CHUNK"""=20x10" h5repack_layout.h5
$! 
$! # fletcher32 for all
$ CALL TOOLTEST "-f """FLET"""" h5repack_layout.h5
$ 
$! ###########################################################
$! # the following tests assume the input files have filters
$! ###########################################################
$! 
$! # deflate copy
$ CALL TOOLTEST "" h5repack_deflate.h5
$! 
$! # deflate remove
$ CALL TOOLTEST "-f dset_deflate:"""NONE"""" h5repack_deflate.h5
$!     
$! # shuffle copy
$ CALL TOOLTEST "" h5repack_shuffle.h5
$! 
$! # shuffle remove
$ CALL TOOLTEST "-f dset_shuffle:"""NONE"""" h5repack_shuffle.h5
$! 
$! # fletcher32 copy
$ CALL TOOLTEST "" h5repack_fletcher.h5
$! 
$! # fletcher32 remove
$ CALL TOOLTEST "-f dset_fletcher32:"""NONE"""" h5repack_fletcher.h5
$! 
$! # nbit copy
$ CALL TOOLTEST "" h5repack_nbit.h5
$! 
$! # nbit remove
$ CALL TOOLTEST "-f dset_nbit:"""NONE"""" h5repack_nbit.h5
$! 
$! # nbit add
$ CALL TOOLTEST "-f dset_int31:"""NBIT"""" h5repack_nbit.h5
$! 
$! # scaleoffset add
$! CALL TOOLTEST "-f dset_none:"""S+O"""=31" h5repack_scaleoffset.h5
$! 
$! # scaleoffset copy
$! CALL TOOLTEST "" h5repack_scaleoffset.h5
$! 
$! # scaleoffset remove
$! CALL TOOLTEST "-f dset_scaleoffset:"""NONE"""" h5repack_scaleoffset.h5
$! 
$! #limit
$ CALL TOOLTEST "-f """GZIP"""=1 -m 1024" h5repack_layout.h5
$! 
$! 
$! 
$! #########################################################
$! # layout options (these files have no filters)
$! #########################################################
$!
$ CALL TOOLTEST "-l dset2:"""CHUNK"""=20x10" h5repack_layout.h5
$ CALL TOOLTEST "-l """CHUNK"""=20x10" h5repack_layout.h5
$ CALL TOOLTEST "-l dset2:"""CONTI"""" h5repack_layout.h5
$ CALL TOOLTEST "-l """CONTI"""" h5repack_layout.h5
$ CALL TOOLTEST "-l dset2:"""COMPA"""" h5repack_layout.h5
$ CALL TOOLTEST "-l """COMPA"""" h5repack_layout.h5
$! 
$! 
$! ################################################################
$! # layout conversions (file has no filters)
$! ###############################################################
$! 
$ CALL TOOLTEST "-l dset_compact:"""CONTI"""" h5repack_layout.h5
$ CALL TOOLTEST "-l dset_compact:"""CHUNK"""=2x5" h5repack_layout.h5 
$ CALL TOOLTEST "-l dset_compact:"""COMPA"""" h5repack_layout.h5
$ CALL TOOLTEST "-l dset_contiguous:"""COMPA"""" h5repack_layout.h5
$ CALL TOOLTEST "-l dset_contiguous:"""CHUNK"""=3x6" h5repack_layout.h5
$ CALL TOOLTEST "-l dset_contiguous:"""CONTI"""" h5repack_layout.h5
$ CALL TOOLTEST "-l dset_chunk:"""COMPA"""" h5repack_layout.h5
$ CALL TOOLTEST "-l dset_chunk:"""CONTI"""" h5repack_layout.h5
$ CALL TOOLTEST "-l dset_chunk:"""CHUNK"""=18x13" h5repack_layout.h5
$!
$!
$!
$TOOLTEST: SUBROUTINE

$ len =  F$LENGTH(P2)
$ base = F$EXTRACT(0,len-3,P2)
$ output_file = base + "_out.h5"
$ output_err = base + ".h5repackerr"
$ output_out = base + ".h5repackout"
$
$ begin = "Testing h5repack"
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$error  'output_err'
$ define/nolog sys$output 'output_out'
$
$ ON ERROR THEN CONTINUE
$ h5repack 'P1 'P2 'output_file'
$ h5diff 'P2 'output_file'
$ deassign sys$error
$ deassign sys$output
$ if F$SEARCH(output_err) .EQS. "" 
$ then
$    result = "PASSED"
$    line = F$FAO("!16AS !40AS !22AS !67AS", begin, P1, P2, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!16AS !40AS !22AS !66AS", begin, P1, P2, result) 
$ endif

$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ !
$ !
$ ! Cleanup temporary files
$ !
$ if F$SEARCH(output_err) .NES. ""
$ then 
$  del *.h5repackerr;*
$ endif 
$  del *.h5repackout;*
$  del  *_out.h5;*
$ENDSUBROUTINE


