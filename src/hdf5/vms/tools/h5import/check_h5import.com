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
$ ! This command file tests h5import utility. The command file has to
$ ! run in the [hdf5-top.tools.h5import.testfiles] directory.
$ !
$ !
$ type sys$input

===================================
       Testing h5import utiltity
===================================

$ ! Define symbols
$ !
$ 
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-19, current_dir)
$ h5dump_dir = temp + "H5DUMP]"
$ h5dump :== $sys$disk:'h5dump_dir'h5dump.exe
$ h5import_dir = temp + "H5IMPORT]"
$ h5import :== $sys$disk:'h5import_dir'h5import.exe
$ h5importtest :== $sys$disk:'h5import_dir'h5importtest.exe
$ !
$ ! Define output for diff command that compares expected and actual
$ ! outputs of h5dump
$ !
$ create h5import.log
$ !
$ ! h5import tests
$ !
$ ! Run h5importtest to create input files
$ !
$ run [.-]h5importtest
$ !
$ type sys$input 
                 Testing ASCII I32 rank 3 - Output BE 
$ CALL TOOLTEST "in32.txt -c textin32.conf -o" test1.h5
$ !
$ type sys$input 
                Testing ASCII I16 rank 3 - Output LE - CHUNKED - extended
$ CALL TOOLTEST  "in16.txt -c textin16.conf -o" test2.h5
$ !
$ type sys$input 
                Testing ASCII I8 - rank 3 - Output I16 LE-Chunked+Extended+Compressed 
$ CALL TOOLTEST  "in16.txt -c textin8.conf  -o" test3.h5
$ !
$ type sys$input 
               Testing ASCII UI32 - rank 3 - Output BE 
$ CALL TOOLTEST  "in1.txt -c textuin32.conf -o" test4.h5
$ !
$ type sys$input 
               Testing ASCII UI16 - rank 2 - Output LE+Chunked+Compressed 
$ CALL TOOLTEST  "in1.txt -c textuin16.conf -o" test5.h5
$ !
$ type sys$input 
               Testing ASCII F32 - rank 3 - Output LE 
$ CALL TOOLTEST  "fp1.txt -c textfp32.conf -o" test6.h5
$ !
$ type sys$input 
               Testing ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed 
$ CALL TOOLTEST  "fp2.txt -c textfp64.conf -o" test7.h5
$ !
$ type sys$input 
               Testing ASCII F64 - rank 1 - INPUT-CLASS TEXTFPE
$ CALL TOOLTEST  "in64.txt -c textpfe.conf -o" test15.h5
$ !
$ type sys$input 
               Testing BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed 
$ CALL TOOLTEST  "binfp64.bin -c binfp64.conf -o" test8.h5
$ !
$ type sys$input 
               Testing BINARY I16 - rank 3 - Output order LE + CHUNKED + extended 
$ CALL TOOLTEST  "binin16.bin -c binin16.conf -o" test9.h5
$ !
$ type sys$input 
               Testing BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed 
$ CALL TOOLTEST  "binin8.bin -c binin8.conf -o" test10.h5
$ !
$ type sys$input 
               Testing BINARY I32 - rank 3 - Output BE + CHUNKED 
$ CALL TOOLTEST  "binin32.bin -c binin32.conf -o" test11.h5
$ !
$ type sys$input 
               Testing BINARY UI16 - rank 3 - Output byte BE + CHUNKED 
$ CALL TOOLTEST  "binuin16.bin -c binuin16.conf -o" test12.h5
$ !
$ type sys$input 
               Testing BINARY UI32 - rank 3 - Output LE + CHUNKED 
$ CALL TOOLTEST  "binuin32.bin -c binuin32.conf -o" test13.h5
$ !
$ type sys$input 
               Testing STR
$ CALL TOOLTEST "str.txt -c textstr.conf -o" test14.h5

$
$ ! Delete temporary files
$ del *.h5;*
$ del *.h5importtxt;*
$ del *.bin;*
$ del *.txt;*
$ del *.dif;*
$ !
$ 
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P2)
$ base = F$EXTRACT(0,len-3,P2)
$ actual = base + "_out.h5"
$ actual_dump = base + "out.h5importtxt"
$ actual_dump_err = base + "out.h5importerr"
$ expected_dump = base + ".h5importtxt"
$ expected_dump_err = base + ".h5importerr"
$
$ begin = "Testing"
$ !
$ ! Run h5import with output in the 'actual' file
$ !
$ ON ERROR THEN CONTINUE
$ h5import 'P1 'actual'
$ define/nolog sys$output 'actual_dump'
$ define/nolog sys$error  'actual_dump_err'
$ !
$ ! Dump the atual and expected files
$ !
$ h5dump 'actual'
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_dump_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_dump_err' 'actual_dump'
$ set message/text/facility/identification/severity
$ endif
$ define/nolog sys$output 'expected_dump'
$ define/nolog sys$error  'expected_dump_err'
$ h5dump 'P2
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(expected_dump_err) .NES. ""
$ then
$ set message/notex/nofacility/noidentification/noseverity
$    append 'expected_dump_err' 'expected_dump'
$ set message/ntext/facility/identification/severity
$ endif
$ !
$ ! Compare the results
$ !
$ diff/output=h5dump_temp/ignore=(spacing,trailing_spaces,blank_lines)-
                                'actual_dump' -
                                'expected_dump'
$ open/read temp_out h5dump_temp.dif
$ !
$READ_DATA:
$ read/end_of_file=end_read temp_out record1
$ !
$ ! Skip blank lines
$ !
$ if record1 .EQS. "" then goto READ_DATA
$ !
$ ! Find record with "Number" and exit the loop
$ !
$ len = F$LENGTH(record1)
$ pos = F$LOCATE("Number", record1)
$ !
$ if pos .EQ. 0  
$ then 
$     err_code = F$EXTRACT(len-1,1,record1)
$     goto END_READ
$ endif
$ !
$ goto READ_DATA
$
$ !
$END_READ: 
$ close temp_out
$ !
$ ! File names are different, so we allow only one difference
$ ! in h5dump 
$ if err_code .eqs. "1" 
$  then
$    result = "PASSED"
$    line = F$FAO("!8AS !62AS !80AS", begin, P1, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!8AS !61AS !79AS", begin, P1, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ ! Append the result to the log file 
$ !
$ append h5dump_temp.dif h5import.log
$ !
$ !
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH(actual_dump_err) .NES. ""
$ then
$  del *out.h5importerr;*
$ endif
$ if F$SEARCH(expected_dump_err) .NES. ""
$ then
$  del *.h5importerr;*
$ endif
$ENDSUBROUTINE
