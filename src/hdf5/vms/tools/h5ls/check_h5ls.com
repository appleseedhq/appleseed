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
$ ! This command file tests h5ls utility. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ type sys$input

===================================
       Testing h5ls utiltity
===================================

$
$ !
$ ! Define h5ls symbol
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5ls_dir = temp + "H5LS]"
$ h5ls :== $sys$disk:'h5ls_dir'h5ls.exe
$ !
$ ! Define output for diff command that compares expected and actual
$ ! outputs of h5ls
$ !
$ create h5ls.log
$ !
$ ! h5ls tests
$ !
$
$ CALL TOOLTEST help-1.ls "-w80 -h"
$ CALL TOOLTEST help-2.ls "-w80 -help"
$ CALL TOOLTEST help-3.ls "-w80 -?"

$! test simple command
$ CALL TOOLTEST tall-1.ls "-w80 tall.h5"
$ CALL TOOLTEST tall-2.ls "-w80 -r -d tall.h5"
$ CALL TOOLTEST tgroup.ls "-w80 tgroup.h5"

$! test for displaying groups
$ CALL TOOLTEST tgroup-1.ls "-w80 -r -g tgroup.h5"

$! test for displaying simple space datasets
$ CALL TOOLTEST tdset-1.ls "-w80 -r -d tdset.h5"

$! test for displaying soft links
$ CALL TOOLTEST tslink-1.ls "-w80 -r tslink.h5"

$! tests for hard links
$ CALL TOOLTEST thlink-1.ls "-w80 thlink.h5"

$! tests for compound data types
$ CALL TOOLTEST tcomp-1.ls "-w80 -r -d tcompound.h5"

$!test for the nested compound type
$ CALL TOOLTEST tnestcomp-1.ls "-w80 -r -d tnestedcomp.h5"

$! test for loop detection
$ CALL TOOLTEST tloop-1.ls "-w80 -r -d tloop.h5"

$! test for string 
$ CALL TOOLTEST tstr-1.ls "-w80 -r -d tstr.h5"

$! test test file created from lib SAF team
$ CALL TOOLTEST tsaf.ls "-w80 -r -d tsaf.h5"

$! test for variable length data types
$ CALL TOOLTEST tvldtypes1.ls "-w80 -r -d tvldtypes1.h5"

$! test for array data types
$ CALL TOOLTEST tarray1.ls "-w80 -r -d tarray1.h5"

$! test for empty data
$ CALL TOOLTEST tempty.ls "-w80 -d tempty.h5"

$! test for all dataset types written to attributes
$! enable -S for avoiding printing NATIVE types
$! This test will report failure for line 311 of the tattr2.ls file 
$! contains
$! Modified:  XXXX-XX-XX XX:XX:XX XXX
$! instead of
$! Modified:  2004-07-06 19:36:17 CST
$! UNIX shell script does replacement on the fly in the actual output
$! file; I do not know what can I do on VMS EIP 07/27/06
$ CALL TOOLTEST tattr2.ls "-w80 -v -"""S""" tattr2.h5"

$
$ 
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-2,P1)
$ actual = base + "h5lsout"
$ actual_err = base + "h5lserr"
$
$ begin = "Testing h5ls "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ write  sys$output "#############################"
$ write  sys$output " output for 'h5ls ''P2''"
$ write  sys$output "#############################"
$ ON ERROR THEN CONTINUE
$ h5ls 'P2
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
$ diff/output=h5ls_temp/ignore=(spacing,trailing_spaces,blank_lines) 'actual' 'P1'
$ open/read temp_out h5ls_temp.dif
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
$ append h5ls_temp.dif h5ls.log
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH("*.h5lserr;*")   then del *.h5lserr;*
$ if F$SEARCH("*.h5lsout;*")   then del *.h5lsout;*
$ if F$SEARCH("*.dif;*")   then del *.dif;*
$ !
$ENDSUBROUTINE

