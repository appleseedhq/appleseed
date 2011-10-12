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
$ ! This command file tests h5diff utility. The command file has to
$ ! run in the [hdf5-top.tools.h5diff.testfiles] directory.
$ !
$ !
$ type sys$input

===================================
       Testing h5diff utiltity
===================================

$ ! Define h5diff symbol
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-11, current_dir)
$ h5diff_dir = temp + "]"
$ h5diff :== $sys$disk:'h5diff_dir'h5diff.exe
$ !

$ !
$ ! h5diff tests
$ !
$
$!# 1.0
$ CALL TOOLTEST h5diff_10.txt "-h"
$!
$!# 1.1 normal mode
$ CALL TOOLTEST h5diff_11.txt  "h5diff_basic1.h5 h5diff_basic2.h5" 
$!
$!# 1.2 normal mode with objects
$ CALL TOOLTEST h5diff_12.txt  "h5diff_basic1.h5 h5diff_basic2.h5  g1/dset1 g1/dset2"
$!
$!# 1.3 report mode
$ CALL TOOLTEST h5diff_13.txt "-r h5diff_basic1.h5 h5diff_basic2.h5"
$!
$!# 1.4 report  mode with objects
$ CALL TOOLTEST h5diff_14.txt  "-r h5diff_basic1.h5 h5diff_basic2.h5 g1/dset1 g1/dset2"
$!
$!# 1.5 with -d
$ CALL TOOLTEST h5diff_15.txt " --report --delta=5 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 1.6.1 with -p (int)
$ CALL TOOLTEST h5diff_16_1.txt "-v -p 0.02 h5diff_basic1.h5 h5diff_basic1.h5 g1/dset5 g1/dset6"
$!
$!# 1.6.2 with -p (unsigned long long)
$ CALL TOOLTEST h5diff_16_2.txt "--verbose --relative=0.02 h5diff_basic1.h5 h5diff_basic1.h5 g1/dset7 g1/dset8"
$!
$!# 1.6.3 with -p (int)
$ CALL TOOLTEST h5diff_16_3.txt "-v -p 0.02 h5diff_basic1.h5 h5diff_basic1.h5 g1/dset9 g1/dset10"
$!
$!# 1.7 verbose mode
$ CALL TOOLTEST h5diff_17.txt "-v h5diff_basic1.h5 h5diff_basic2.h5"  
$!
$!# 1.8 quiet mode 
$ CALL TOOLTEST h5diff_18.txt "-q h5diff_basic1.h5 h5diff_basic2.h5"
$!
$!
$!# ##############################################################################
$!# # not comparable types
$!# ##############################################################################
$!
$!# 2.0
$ CALL TOOLTEST h5diff_20.txt "-v h5diff_types.h5 h5diff_types.h5 dset g1"
$
$!# 2.1
$ CALL TOOLTEST h5diff_21.txt "-v h5diff_types.h5 h5diff_types.h5 dset l1"
$!
$!# 2.2
$ CALL TOOLTEST h5diff_22.txt "-v h5diff_types.h5 h5diff_types.h5 dset t1"
$!
$!# ##############################################################################
$!# # compare groups, types, links (no differences and differences)
$!# ##############################################################################
$!
$!# 2.3
$ CALL TOOLTEST h5diff_23.txt "-v h5diff_types.h5 h5diff_types.h5 g1 g1"
$!
$!# 2.4
$ CALL TOOLTEST h5diff_24.txt "-v h5diff_types.h5 h5diff_types.h5 t1 t1"
$!
$!# 2.5
$ CALL TOOLTEST h5diff_25.txt "-v h5diff_types.h5 h5diff_types.h5 l1 l1" 
$!
$!# 2.6
$ CALL TOOLTEST h5diff_26.txt "-v h5diff_types.h5 h5diff_types.h5 g1 g2"
$!
$!# 2.7
$ CALL TOOLTEST h5diff_27.txt "-v h5diff_types.h5 h5diff_types.h5 t1 t2"
$!
$!# 2.8
$ CALL TOOLTEST h5diff_28.txt "-v h5diff_types.h5 h5diff_types.h5 l1 l2"
$!
$!
$!
$!# ##############################################################################
$!# # Dataset types
$!# ##############################################################################
$
$!# 5.0
$ CALL TOOLTEST h5diff_50.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset0a dset0b"
$!
$!# 5.1
$ CALL TOOLTEST h5diff_51.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset1a dset1b"
$!
$!# 5.2
$ CALL TOOLTEST h5diff_52.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset2a dset2b"
$!
$!# 5.3
$ CALL TOOLTEST h5diff_53.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset3a dset4b"
$!
$!# 5.4
$ CALL TOOLTEST h5diff_54.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset4a dset4b"
$!
$!# 5.5
$ CALL TOOLTEST h5diff_55.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset5a dset5b"
$!
$!# 5.6
$ CALL TOOLTEST h5diff_56.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset6a dset6b"
$!
$!# 5.7
$ CALL TOOLTEST h5diff_57.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset7a dset7b"
$!
$!# 5.8 (region reference)
$ CALL TOOLTEST h5diff_58.txt "-v h5diff_dset1.h5 h5diff_dset2.h5 refreg"
$!
$!# ##############################################################################
$!# # Error messages
$!# ##############################################################################
$!
$!
$!# 6.0: Check if the command line number of arguments is less than 3
$ CALL TOOLTEST h5diff_600.txt "h5diff_basic1.h5" 
$!
$!# ##############################################################################
$!# # -d 
$!# ##############################################################################
$!
$!
$!# 6.3: negative value
$ CALL TOOLTEST h5diff_603.txt "-d -4 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.4: zero
$ CALL TOOLTEST h5diff_604.txt "-d 0 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.5: non number
$ CALL TOOLTEST h5diff_605.txt "-d u h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.6: hexadecimal
$ CALL TOOLTEST h5diff_606.txt "-d 0x1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.7: string
$ CALL TOOLTEST h5diff_607.txt "-d """1""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.8: repeated option
$ CALL TOOLTEST h5diff_608.txt "-d 1 -d 2 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.9: number larger than biggest difference
$ CALL TOOLTEST h5diff_609.txt "-d 200 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.10: number smaller than smallest difference
$ CALL TOOLTEST h5diff_610.txt "-d 1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!
$!# ##############################################################################
$!# # -p
$!# ##############################################################################
$!
$!
$!
$!# 6.12: negative value
$ CALL TOOLTEST h5diff_612.txt "-p -4 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.13: zero
$ CALL TOOLTEST h5diff_613.txt "-p 0 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.14: non number
$ CALL TOOLTEST h5diff_614.txt "-p u h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.15: hexadecimal
$ CALL TOOLTEST h5diff_615.txt "-p 0x1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.16: string
$! CALL TOOLTEST h5diff_616.txt "-p """0.21""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.17: repeated option
$ CALL TOOLTEST h5diff_617.txt "-p 0.21 -p 0.22 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.18: number larger than biggest difference
$ CALL TOOLTEST h5diff_618.txt "-p 2 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.19: number smaller than smallest difference
$ CALL TOOLTEST h5diff_619.txt "-p 0.005 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!
$!
$!# ##############################################################################
$!# # -n
$!# ##############################################################################
$!
$!
$!
$!# 6.21: negative value
$ CALL TOOLTEST h5diff_621.txt "-n -4 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.22: zero
$ CALL TOOLTEST h5diff_622.txt "-n 0 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.23: non number
$ CALL TOOLTEST h5diff_623.txt "-n u h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.24: hexadecimal
$ CALL TOOLTEST h5diff_624.txt "-n 0x1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.25: string
$ CALL TOOLTEST h5diff_625.txt "-n """2""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.26: repeated option
$ CALL TOOLTEST h5diff_626.txt "-n 2 -n 3 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.27: number larger than biggest difference
$ CALL TOOLTEST h5diff_627.txt "--count=200 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.28: number smaller than smallest difference
$ CALL TOOLTEST h5diff_628.txt "-n 1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# ##############################################################################
$!# 6.29  non valid files
$!# ##############################################################################
$! Disable this test as C script does
$! CALL TOOLTEST h5diff_629.txt "file1.h6 file2.h6"
$!
$!# ##############################################################################
$!# 7.  attributes
$!# ##############################################################################
$!
$ CALL TOOLTEST h5diff_70.txt "-v h5diff_attr1.h5 h5diff_attr2.h5"
$!
$!# ##############################################################################
$!# 8.  all dataset datatypes
$!# ##############################################################################
$!
$ CALL TOOLTEST h5diff_80.txt "-v h5diff_dset1.h5 h5diff_dset2.h5"
$!
$!# 9. compare a file with itself
$!
$ CALL TOOLTEST h5diff_90.txt "-v h5diff_basic2.h5 h5diff_basic2.h5"
$!
$! 10. read by hyperslab, print indexes
$ CALL TOOLTEST h5diff_100.txt "-v h5diff_hyper1.h5 h5diff_hyper2.h5"
$!
$! 11. floating point comparison
$ CALL TOOLTEST h5diff_101.txt "-v h5diff_basic1.h5 h5diff_basic1.h5 g1/d1  g1/d2"
$ CALL TOOLTEST h5diff_102.txt "-v h5diff_basic1.h5 h5diff_basic1.h5 g1/fp1  g1/fp2"
$!
$!
$!
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ actual = base + "h5diffout"
$ actual_err = base + "h5differr"
$
$ begin = "Testing h5diff "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ ON ERROR THEN CONTINUE
$ h5diff 'P2
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
$ diff/output=h5diff_temp/ignore=(spacing,trailing_spaces,blank_lines)/comment_delim=(%) 'actual' 'P1'
$ open/read temp_out h5diff_temp.dif
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
$ append/new_version h5diff_temp.dif h5diff.log
$ append/new_version 'actual'        h5diff_output.txt
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH(actual_err)  .NES. ""
$ then
$  del *.h5differr;*
$ endif
$  del *.h5diffout;*
$  del h5diff_temp.dif;*
$ !
$ENDSUBROUTINE

