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
$ ! This command file tests h5copy utility. The command file has to
$ ! run in the [hdf5-top.tools.h5copy.testfiles] directory.
$ !
$ type sys$input

===================================
       Testing h5copy utiltity
===================================

$ !
$ ! Define h5copy symbols
$ !
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-11, current_dir)
$ h5copy_dir = temp + "]"
$ h5copy :== $sys$disk:'h5copy_dir'h5copy.exe
$ !
$ !
$ ! h5copy tests
$ !
$

$ !# copy files 
$ write sys$output "Test copying various forms of datasets"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s simple -d simple"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s chunk      -d chunk"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s compact    -d compact"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s compound   -d compound"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s compressed -d compressed"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s named_vl   -d named_vl"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s nested_vl  -d nested_vl"
$ !
$ write sys$output " "
$ write sys$output "Test copying dataset within group in source file to root of destination"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s grp_dsets/simple  -d simple_top"
$ write sys$output " "
$ write sys$output "Test copying & renaming dataset"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s compound   -d rename"
$!
$ write sys$output " "
$ write sys$output "Test copying empty, 'full' & 'nested' groups"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s grp_empty  -d grp_empty"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s grp_dsets  -d grp_dsets"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s grp_nested -d grp_nested"
$!
$ write sys$output " "
$ write sys$output "Test copying dataset within group in source file to group in destination"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -v -s /grp_dsets/simple  -d /grp_dsets/simple_group"
$! write sys$output  "Test copying & renaming group"
$! CALL TOOLTEST_FAIL "-i h5copytst.h5 -o out.h5 -v -s grp_dsets  -d grp_rename
$! write sys$output  "Test copying full group hierarchy into group in destination file"
$! CALL TOOLTEST_FAIL "-i h5copytst.h5 -o out.h5 -v -s grp_dsets  -d /grp_rename/grp_dsets"
$!
$ write sys$output " "
$ write sys$output "Test copying objects into group hier. that doesn't exist yet in destination file"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -vp -s simple -d /A/B1/simple"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -vp -s simple -d /A/B2/simple2"
$ CALL TOOLTEST "-i h5copytst.h5 -o out.h5 -vp -s /grp_dsets/simple -d /C/D/simple"
$!CALL TOOLTEST_FAIL "-i h5copytst.h5 -o out.h5 -vp -s /grp_dsets -d /E/F/grp_dsets"
$!CALL TOOLTEST_FAIL "-i h5copytst.h5 -o out.h5 -vp -s /grp_nested -d /G/H/grp_nested"
$!
$ del *out.h5;*
$ !
$TOOLTEST: SUBROUTINE

$
$ begin = "Testing h5copy"
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$error  h5copy_temp.err
$ ON ERROR THEN CONTINUE
$ h5copy 'P1 
$ deassign sys$error
$ if F$SEARCH("h5copy_temp.err") .EQS. "" 
$ then
$    result = "PASSED"
$    line = F$FAO("!16AS !53AS !70AS", begin, P1, result) 
$ else
$    result = "*FAILED*"
$    line = F$FAO("!16AS !52AS !69AS", begin, P1, result) 
$ endif
$ !
$ ! Delete error file if any print test result
$ !
$ if F$SEARCH ("*.err;*") .NES. ""
$ then
$     del *.err;*
$ endif
$ !
$  write sys$output line
$ !
$ENDSUBROUTINE
