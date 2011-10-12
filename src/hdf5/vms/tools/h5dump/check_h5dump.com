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
$ !
$ ! This command file tests h5dump utility. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ type sys$input

===================================
       Testing h5dump utiltity
===================================

$
$ !
$ ! Define h5dump symbol
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5dump_dir = temp + "H5DUMP]"
$ h5dump :== $sys$disk:'h5dump_dir'h5dump.exe
$ !
$ ! h5dump tests
$ !
$
$
$ 
$ ! Test for displaying groups
$ CALL TOOLTEST tgroup-1.ddl "tgroup.h5"
$ ! Test for displaying the selected groups
$ CALL TOOLTEST tgroup-2.ddl "--group=/g2 --group / -g /y tgroup.h5"
$ ! Test for displaying simple space datasets
$ CALL TOOLTEST tdset-1.ddl "tdset.h5"
$ ! Test for displaying selected datasets
$ CALL TOOLTEST tdset-2.ddl "-"""H""" -d dset1 -d /dset2 --dataset=dset3 tdset.h5"
$ ! Test for displaying attributes
$ CALL TOOLTEST tattr-1.ddl "tattr.h5"
$ ! Test for displaying the selected attributes of string type and scalar space
$ CALL TOOLTEST tattr-2.ddl "-a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5"
$ ! Test for header and error messages
$ CALL TOOLTEST tattr-3.ddl "--header -a /attr2 --attribute=/attr tattr.h5"
$ ! Test for displaying attributes in shared datatype (also in group and dataset)
$ CALL TOOLTEST tnamed_dtype_attr.ddl "tnamed_dtype_attr.h5"
$
$ ! Test for displaying soft links
$ CALL TOOLTEST tslink-1.ddl "tslink.h5"
$ ! Test for displaying the selected link
$ CALL TOOLTEST tslink-2.ddl "-l slink2 tslink.h5"
$
$ ! Tests for hard links
$ CALL TOOLTEST thlink-1.ddl "thlink.h5"
$ CALL TOOLTEST thlink-2.ddl "-d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5"
$ CALL TOOLTEST thlink-3.ddl "-d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5"
$ CALL TOOLTEST thlink-4.ddl "-g /g1 thlink.h5"
$ CALL TOOLTEST thlink-5.ddl "-d /dset1 -g /g2 -d /g1/dset2 thlink.h5"
$
$ ! Tests for compound data types
$ CALL TOOLTEST tcomp-1.ddl "tcompound.h5"
$ ! test for named data types
$ CALL TOOLTEST tcomp-2.ddl "-t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5"
$ ! Test for unamed type 
$ CALL TOOLTEST tcomp-3.ddl "-t /#6632 -g /group2 tcompound.h5"
$ ! Test complicated compound datatype
$ CALL TOOLTEST tcomp-4.ddl "tcompound_complex.h5"
$
$ ! Test for the nested compound type
$ CALL TOOLTEST tnestcomp-1.ddl "tnestedcomp.h5"
$
$ ! test for options
$ CALL TOOLTEST tall-1.ddl "tall.h5"
$ CALL TOOLTEST tall-2.ddl "--header -g /g1/g1.1 -a attr2 tall.h5"
$ CALL TOOLTEST tall-3.ddl "-d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5"
$
$ ! Test for loop detection
$ CALL TOOLTEST tloop-1.ddl "tloop.h5"
$
$ ! Test for string 
$ CALL TOOLTEST tstr-1.ddl "tstr.h5"
$ CALL TOOLTEST tstr-2.ddl "tstr2.h5"
$
$ ! Test for file created by Lib SAF team
$ CALL TOOLTEST tsaf.ddl "tsaf.h5"
$
$ ! Test for file with variable length data
$ CALL TOOLTEST tvldtypes1.ddl "tvldtypes1.h5"
$ CALL TOOLTEST tvldtypes2.ddl "tvldtypes2.h5"
$ CALL TOOLTEST tvldtypes3.ddl "tvldtypes3.h5"
$ CALL TOOLTEST tvldtypes4.ddl "tvldtypes4.h5"
$ CALL TOOLTEST tvldtypes5.ddl "tvldtypes5.h5"
$
$ ! Test for file with variable length string data
$ CALL TOOLTEST tvlstr.ddl "tvlstr.h5"
$
$ ! Test for files with array data
$ CALL TOOLTEST tarray1.ddl "tarray1.h5"
$ CALL TOOLTEST tarray2.ddl "tarray2.h5"
$ CALL TOOLTEST tarray3.ddl "tarray3.h5"
$ CALL TOOLTEST tarray4.ddl "tarray4.h5"
$ CALL TOOLTEST tarray5.ddl "tarray5.h5"
$ CALL TOOLTEST tarray6.ddl "tarray6.h5"
$ CALL TOOLTEST tarray7.ddl "tarray7.h5"
$
$ ! Test for files with empty data
$ CALL TOOLTEST tempty.ddl "tempty.h5"
$
$ ! Test for files with groups that have comments
$ CALL TOOLTEST tgrp_comments.ddl "tgrp_comments.h5"
$
$ ! Test the --filedriver flag
$ CALL TOOLTEST tsplit_file.ddl "--filedriver=split tsplit_file"
$ CALL TOOLTEST tfamily.ddl "--filedriver=family tfamily%05d.h5"
$ CALL TOOLTEST tmulti.ddl "--filedriver=multi tmulti"
$
$ ! Test for files with group names which reach > 1024 bytes in size
$ CALL TOOLTEST tlarge_objname.ddl "-w157 tlarge_objname.h5"
$
$ ! Test '-A' to suppress data but print attr's
$ CALL TOOLTEST tall-2A.ddl "-"""A""" tall.h5"
$
$ ! Test '-r' to print attributes in ASCII instead of decimal
$ CALL TOOLTEST tall-2B.ddl "-"""A""" -r tall.h5"
$
$ ! Test Subsetting
$ CALL TOOLTEST tall-4s.ddl "--dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5"
$ CALL TOOLTEST tall-5s.ddl "-d /g1/g1.1/dset1.1.2[0;2;10;] tall.h5"
$ CALL TOOLTEST tdset-3s.ddl "-d /dset1[1,1;;;] tdset.h5"
$! CALL TOOLTEST tdset-3s.ddl "-d """/"dset"1[;3,2;4,4;1,4]""" tdset2.h5"
$
$ ! Test printing characters in ASCII instead of decimal
$ CALL TOOLTEST tchar1.ddl "-r tchar.h5"
$
$ ! Test failure handling
$ ! Missing file name
$ CALL TOOLTEST "tnofilename-with-packed-bits.ddl"
$
$ ! rev. 2004
$
$ ! Tests for super block
$ CALL TOOLTEST tboot1.ddl "-"""H""" -"""B""" -d dset tfcontents1.h5"
$ CALL TOOLTEST tboot2.ddl "-"""B""" tfcontents2.h5"
$
$ ! Test -p with a non existing dataset
$ CALL TOOLTEST tperror.ddl "-p -d bogus tfcontents1.h5"
$
$ ! Test for file contents
$ CALL TOOLTEST tcontents.ddl "-n tfcontents1.h5"
$
$ ! Tests for storage layout
$ ! Compact
$ CALL TOOLTEST tcompact.ddl "-"""H""" -p -d compact tfilters.h5"
$ ! Contiguous
$ CALL TOOLTEST tcontiguos.ddl "-"""H""" -p -d contiguous tfilters.h5"
$ ! Chunked
$ CALL TOOLTEST tchunked.ddl "-"""H""" -p -d chunked tfilters.h5"
$ ! External 
$ CALL TOOLTEST texternal.ddl "-"""H""" -p -d external tfilters.h5"
$
$ ! Fill values
$ CALL TOOLTEST tfill.ddl "-p tfvalues.h5"
$
$ ! Several datatype, with references , print path
$ CALL TOOLTEST treference.ddl  "tattr2.h5"
$
$ ! Escape/not escape non printable characters
$ CALL TOOLTEST tstringe.ddl "-e tstr3.h5"
$ CALL TOOLTEST tstring.ddl "tstr3.h5"
$ ! Char data as ASCII with non escape
$ CALL TOOLTEST tstring2.ddl "-r -d str4 tstr3.h5"
$
$ ! Array indices print/not print
$ CALL TOOLTEST tindicesyes.ddl "taindices.h5"
$ CALL TOOLTEST tindicesno.ddl "-y taindices.h5"
$ ! User defined
$ CALL TOOLTEST tuserfilter.ddl "-"""H"""  -p -d myfilter  tfilters.h5"
$    
$ ! Test for displaying dataset and attribute of null space
$ CALL TOOLTEST tnullspace.ddl "tnullspace.h5"
$
$ ! Test for displaying objects with very long names
$ !CALL TOOLTEST tlonglinks.ddl "tlonglinks.h5"
$
$ ! Test for long double (some systems do not have long double)
$ ! CALL TOOLTEST tldouble.ddl "tldouble.h5"
$
$ ! Test for vms
$ CALL TOOLTEST tvms.ddl "tvms.h5"
$
$ !test for binary output
$ CALL TOOLTEST1 tbin1.ddl "-d integer -o out1.bin -b """LE""" tbinary.h5"
$ CALL TOOLTEST1 tbin2.ddl "-d float   -o out2.bin -b """BE""" tbinary.h5"
$ CALL TOOLTEST1 tbin4.ddl "-d double   -o out4.bin -b """FILE""" tbinary.h5"
$
$ ! Test for dataset region references
$ CALL TOOLTEST tdatareg.ddl "tdatareg.h5"
$
$ ! tests for group creation order "1" tracked, "2" name, root tracked
$ CALL TOOLTEST tordergr1.ddl "--group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5"
$ CALL TOOLTEST tordergr2.ddl "--group=1 --sort_by=creation_order --sort_order=descending tordergr.h5"
$ CALL TOOLTEST tordergr3.ddl "-g 2 -q name -z ascending tordergr.h5"
$ CALL TOOLTEST tordergr4.ddl "-g 2 -q name -z descending tordergr.h5"
$ CALL TOOLTEST tordergr5.ddl "-q creation_order tordergr.h5"
$
$ ! Tests for attribute order
$ CALL TOOLTEST torderattr1.ddl "-"""H""" --sort_by=name --sort_order=ascending torderattr.h5"
$ CALL TOOLTEST torderattr2.ddl "-"""H""" --sort_by=name --sort_order=descending torderattr.h5"
$ CALL TOOLTEST torderattr3.ddl "-"""H""" --sort_by=creation_order --sort_order=ascending torderattr.h5"
$ CALL TOOLTEST torderattr4.ddl "-"""H""" --sort_by=creation_order --sort_order=descending torderattr.h5"
$
$ ! Test for dataset packed bits
$ ! Limits:
$ ! Maximum number of packed bits is 8 (for now).
$ ! Maximum integer size is 8 (for now).
$ ! Maximun Offset is 7 (Maximum size - 1).
$ ! Maximum Offset+Length is 8 (Maximum size).
$ ! Test Normal operation on both signed and unsigned int datasets.
$ ! Their rawdata output should be the same.
$ CALL TOOLTEST tpbitsSigned.ddl "-d /"""DS08BITS""" -"""M""" 0,2,2,6 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsigned.ddl "-d /"""DU08BITS""" -"""M""" 0,2,2,6 packedbits.h5"
$ ! Overlapped packed bits.
$ CALL TOOLTEST tpbitsOverlapped.ddl "-d /"""DS08BITS""" -"""M""" 0,1,1,1,2,1,0,3 packedbits.h5"
$ ! Maximum number of packed bits.
$ CALL TOOLTEST tpbitsMax.ddl "-d /"""DS08BITS""" -"""M""" 0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5"
$ ! Compound type.
$ CALL TOOLTEST tpbitsCompound.ddl "-d /dset1 -"""M""" 0,1,1,1 tcompound.h5"
$ ! Array type.
$ CALL TOOLTEST tpbitsArray.ddl "-d /"""D"""ataset1 -"""M""" 0,1,1,1 tarray1.h5"
$ ! Test Error handling.
$ ! Too many packed bits requested. Max is 8 for now.
$ CALL TOOLTEST tpbitsMaxExceeded.ddl "-d /"""DS08BITS""" -"""M""" 0,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5"
$ ! Offset too large. Max is 7 (8-1) for now.
$ CALL TOOLTEST tpbitsOffsetExceeded.ddl "-d /"""DS08BITS""" -"""M""" 8,1 packedbits.h5"
$ ! Bad offset, must not be negative.
$ CALL TOOLTEST tpbitsOffsetNegative.ddl "-d /"""DS08BITS""" -"""M""" -1,1 packedbits.h5"
$ ! Bad length, must not be positive.
$ CALL TOOLTEST tpbitsLengthPositive.ddl "-d /"""DS08BITS""" -"""M""" 4,0 packedbits.h5"
$ ! Offset+Length is too large. Max is 8 for now.
$ CALL TOOLTEST tpbitsLengthExceeded.ddl "-d /"""DS08BITS""" -"""M""" 2,7 packedbits.h5"
$ ! Incomplete pair of packed bits request.
$ CALL TOOLTEST tpbitsIncomplete.ddl "-d /"""DS08BITS""" -"""M""" 0,2,2,1,0,2,2, packedbits.h5"
$
$ !
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ actual = base + "h5dumpout"
$ actual_err = base + "h5dumperr"
$
$ begin = "Testing h5dump "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ 
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ write  sys$output "#############################"
$ write  sys$output "Expected output for 'h5dump ''P2''"
$ write  sys$output "#############################"
$ ON ERROR THEN CONTINUE
$ h5dump 'P2
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_err' 'actual'
$ set message/text/facility/identification/severity
$ endif
$ !
$ ! Compare the results
$ !
$ diff/output=h5dump_temp/ignore=(spacing,trailing_spaces,blank_lines) 'actual' 'P1'
$ open/read temp_out h5dump_temp.dif
$ read temp_out record1
$ close temp_out
$ !
$ ! Extract error code and format output line
$ !
$ len = F$LENGTH(record1)
$ err_code = F$EXTRACT(len-1,1,record1)
$ if err_code .eqs. "0" 
$  then
$    result = "PASSED"
$    line = F$FAO("!15AS !50AS !70AS", begin, P2, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!15AS !49AS !69AS", begin, P2, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ ! Append the result to the log file 
$ !
$ append/new_version h5dump_temp.dif h5dump.log
$ append/new_version 'actual'        h5dump_output.txt
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH(actual_err) .NES. ""
$ then
$  del *.h5dumperr;*
$ endif
$  del *.h5dumpout;*
$  del h5dump_temp.dif;*
$ !
$ENDSUBROUTINE
$
$TOOLTEST1: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ actual = base + "h5dumpout"
$ actual_err = base + "h5dumperr"
$
$ begin = "Testing h5dump "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ ON ERROR THEN CONTINUE
$ h5dump 'P2
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_err' 'actual'
$ set message/text/facility/identification/severity
$ endif
$ !
$ ! Compare the results
$ !
$ diff/output=h5dump_temp/ignore=(spacing,trailing_spaces,blank_lines) 'actual' 'P1'
$ open/read temp_out h5dump_temp.dif
$ read temp_out record1
$ close temp_out
$ !
$ ! Extract error code and format output line
$ !
$ len = F$LENGTH(record1)
$ err_code = F$EXTRACT(len-1,1,record1)
$ if err_code .eqs. "0" 
$  then
$    result = "PASSED"
$    line = F$FAO("!15AS !50AS !70AS", begin, P2, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!15AS !49AS !69AS", begin, P2, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ ! Append the result to the log file 
$ !
$ append/new_version h5dump_temp.dif h5dump.log
$ append/new_version 'actual'        h5dump_output.txt
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH(actual_err) .NES. ""
$ then
$  del *.h5dumperr;*
$ endif
$  del *.h5dumpout;*
$  del h5dump_temp.dif;*
$ !
$ENDSUBROUTINE

