#! /bin/sh
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
#
# Tests for the h5diff tool
#
# Modification:
#   Albert Cheng, 2005/08/17
#   Added the SKIP feature.
#   Albert Cheng, 2005/2/3
#   Added -p option for parallel h5diff tests.
#   Pedro Vicente Nunes:
#    10/25/2005: Added test #9
#    11/27/2006: Added test #10, #11
#   Jonathan Kim:
#    Improved to use single line
#    Improved to check exit code (only serial mode, not necessary for parallel)
#    Added test 400 - 425  (links with --follow-symlinks option)
#    Added test 450 - 459  (dangling links)


###############################################################################
## test file names 
###############################################################################

FILE1=h5diff_basic1.h5
FILE2=h5diff_basic2.h5
FILE3=h5diff_types.h5
FILE4=h5diff_dtypes.h5
FILE5=h5diff_attr1.h5
FILE6=h5diff_attr2.h5
FILE7=h5diff_dset1.h5
FILE8=h5diff_dset2.h5
FILE9=h5diff_hyper1.h5
FILE10=h5diff_hyper2.h5
FILE11=h5diff_empty.h5
FILE12=h5diff_links.h5
FILE13=h5diff_softlinks.h5
FILE14=h5diff_linked_softlink.h5
FILE15=h5diff_extlink_src.h5
FILE16=h5diff_extlink_trg.h5
FILE17=h5diff_ext2softlink_src.h5
FILE18=h5diff_ext2softlink_trg.h5
DANGLE_LINK_FILE1=h5diff_danglelinks1.h5
DANGLE_LINK_FILE2=h5diff_danglelinks2.h5
# group recursive
GRP_RECURSE_FILE1=h5diff_grp_recurse1.h5
GRP_RECURSE_FILE2=h5diff_grp_recurse2.h5
# group recursive - same structure via external links through files
GRP_RECURSE1_EXT=h5diff_grp_recurse_ext1.h5
GRP_RECURSE2_EXT1=h5diff_grp_recurse_ext2-1.h5
GRP_RECURSE2_EXT2=h5diff_grp_recurse_ext2-2.h5
GRP_RECURSE2_EXT3=h5diff_grp_recurse_ext2-3.h5
# same structure, same obj name with different value
EXCLUDE_FILE1_1=h5diff_exclude1-1.h5
EXCLUDE_FILE1_2=h5diff_exclude1-2.h5
# different structure and obj names
EXCLUDE_FILE2_1=h5diff_exclude2-1.h5
EXCLUDE_FILE2_2=h5diff_exclude2-2.h5
# compound type with multiple vlen string types
COMP_VL_STRS_FILE=h5diff_comp_vl_strs.h5

ATTR_VERBOSE_LEVEL_FILE1=h5diff_attr_v_level1.h5
ATTR_VERBOSE_LEVEL_FILE2=h5diff_attr_v_level2.h5

TESTNAME=h5diff
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5DIFF=h5diff               # The tool name
H5DIFF_BIN=`pwd`/$H5DIFF    # The path of the tool binary

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes
h5haveexitcode=yes	    # default is yes
pmode=			    # default to run h5diff tests
mydomainname=`domainname 2>/dev/null`

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d ./testfiles || mkdir ./testfiles

# Parse option
#   -p   run ph5diff tests
#   -h   print help page
while [ $# -gt 0 ]; do
    case "$1" in
    -p)	# reset the tool name and bin to run ph5diff tests
	TESTNAME=ph5diff
	H5DIFF=ph5diff               # The tool name
	H5DIFF_BIN=`pwd`/$H5DIFF
	pmode=yes
	shift
	;;
    -h) # print help page
	echo "$0 [-p] [-h]"
	echo "    -p   run ph5diff tests"
	echo "    -h   print help page"
	shift
	exit 0
	;;
    *)  # unknown option
        echo "$0: Unknown option ($1)"
	exit 1
	;;
    esac
done

# RUNSERIAL is used. Check if it can return exit code from executalbe correctly.
if [ -n "$RUNSERIAL_NOEXITCODE" ]; then
    echo "***Warning*** Serial Exit Code is not passed back to shell corretly."
    echo "***Warning*** Exit code checking is skipped."
    h5haveexitcode=no
fi

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Some systems will dump some messages to stdout for various reasons.
# Remove them from the stdout result file.
# $1 is the file name of the file to be filtered.
# Cases of filter needed:
# 1. Sandia Red-Storm
#    yod always prints these two lines at the beginning.
#    LibLustre: NAL NID: 0004a605 (5)
#    Lustre: OBD class driver Build Version: 1, info@clusterfs.com
# 2. LANL Lambda
#    mpijob mirun -np always add an extra line at the end like:
#    P4 procgroup file is /users/acheng/.lsbatch/host10524.l82
STDOUT_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5test_tmp_$$
    # Filter Sandia Red-Storm yod messages.
    cp $result_file $tmp_file
    sed -e '/^LibLustre:/d' -e '/^Lustre:/d' \
	< $tmp_file > $result_file
    # Filter LANL Lambda mpirun message.
    cp $result_file $tmp_file
    sed -e '/^P4 procgroup file is/d' \
	< $tmp_file > $result_file
    # cleanup
    rm -f $tmp_file
}

# Some systems will dump some messages to stderr for various reasons.
# Remove them from the stderr result file.
# $1 is the file name of the file to be filtered.
# Cases of filter needed:
# 1. MPE:
# In parallel mode and if MPE library is used, it prints the following
# two message lines whether the MPE tracing is used or not.
#    Writing logfile.
#    Finished writing logfile.
# 2. LANL MPI:
# The LANL MPI will print some messages like the following,
#    LA-MPI: *** mpirun (1.5.10)
#    LA-MPI: *** 3 process(es) on 2 host(s): 2*fln21 1*fln22
#    LA-MPI: *** libmpi (1.5.10)
#    LA-MPI: *** Copyright 2001-2004, ACL, Los Alamos National Laboratory
# 3. h5diff debug output:
#    Debug output all have prefix "h5diff debug: ".
# 4. AIX system prints messages like these when it is aborting:
#    ERROR: 0031-300  Forcing all remote tasks to exit due to exit code 1 in task 0
#    ERROR: 0031-250  task 4: Terminated
#    ERROR: 0031-250  task 3: Terminated
#    ERROR: 0031-250  task 2: Terminated
#    ERROR: 0031-250  task 1: Terminated
# 5. LLNL Blue-Gene mpirun prints messages like there when it exit non-zero:
#    <Apr 12 15:01:49.075658> BE_MPI (ERROR): The error message in the job record is as follows:
#    <Apr 12 15:01:49.075736> BE_MPI (ERROR):   "killed by exit(1) on node 0"


STDERR_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5test_tmp_$$
    # Filter LLNL Blue-Gene error messages in both serial and parallel modes
    # since mpirun is used in both modes.
    cp $result_file $tmp_file
    sed -e '/ BE_MPI (ERROR): /d' \
	< $tmp_file > $result_file
    # Filter MPE messages
    if test -n "$pmode"; then
	cp $result_file $tmp_file
	sed -e '/^Writing logfile./d' -e '/^Finished writing logfile./d' \
	    < $tmp_file > $result_file
    fi
    # Filter LANL MPI messages
    # and LLNL srun messages
    # and AIX error messages
    if test -n "$pmode"; then
	cp $result_file $tmp_file
	sed -e '/^LA-MPI:/d' -e '/^srun:/d' -e '/^ERROR:/d' \
	    < $tmp_file > $result_file
    fi
    # Filter h5diff debug output
	cp $result_file $tmp_file
	sed -e '/^h5diff debug: /d' \
	    < $tmp_file > $result_file
    # clean up temporary files.
    rm -f $tmp_file
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
#
# Need eval before the RUNCMD command because some machines like
# AIX, has RUNPARALLEL in the style as
#   MP_PROCS=3 MP_TASKS_PER_NODE=3 poe ./a.out
# that throws the shell script off.
#
TOOLTEST() {
    expect="$srcdir/testfiles/$1"
    actual="./testfiles/`basename $1 .txt`.out"
    actual_err="./testfiles/`basename $1 .txt`.err"
    actual_sav=${actual}-sav
    actual_err_sav=${actual_err}-sav
    shift
    if test -n "$pmode"; then
        RUNCMD=$RUNPARALLEL
    else
        RUNCMD=$RUNSERIAL
    fi

    # Run test.
    TESTING $H5DIFF $@
    (
	#echo "#############################"
	#echo "Expected output for '$H5DIFF $@'" 
	#echo "#############################"
	cd $srcdir/testfiles
	eval $RUNCMD $H5DIFF_BIN "$@"
    ) >$actual 2>$actual_err
    EXIT_CODE=$?
    # save actual and actual_err in case they are needed later.
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cp $actual_err $actual_err_sav
    STDERR_FILTER $actual_err
    cat $actual_err >> $actual
    # don't add exit code check in pmode, as it causes failure. (exit code 
    # is from mpirun not tool)
    # if any problem occurs relate to an exit code, it will be caught in 
    # serial mode, so the test is fullfilled.
    if test $h5haveexitcode = 'yes' -a -z "$pmode"; then
      echo "EXIT CODE: $EXIT_CODE" >> $actual
    fi

    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
    elif $CMP $expect $actual; then
        echo " PASSED"
    elif test $h5haveexitcode = 'yes' -a -z "$pmode"; then
        echo "*FAILED*"
        echo "    Expected result ($expect) differs from actual result ($actual)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    else
	    # parallel mode output are often of different ordering from serial
        # output.  If the sorted expected and actual files compare the same,
        # it is safe to assume the actual output match the expected file.
        expect_sorted=expect_sorted
        actual_sorted=actual_sorted
        sort $expect -o $expect_sorted
        sort $actual -o $actual_sorted
        # remove "EXIT CODE:" line from expect file. test for exit code
        # is done by serial mode.
        grep -v "EXIT CODE:" $expect_sorted > $expect_sorted.noexit
        mv $expect_sorted.noexit $expect_sorted
	if $CMP $expect_sorted $actual_sorted; then
	    echo " PASSED"
	else
	    echo "*FAILED*"
	    nerrors="`expr $nerrors + 1`"
	    if test yes = "$verbose"; then
		echo "====Expected result ($expect_sorted) differs from actual result ($actual_sorted)"
		$DIFF $expect_sorted $actual_sorted |sed 's/^/    /'
		echo "====The actual output ($actual_sav)"
		sed 's/^/    /' < $actual_sav 
		echo "====The actual stderr ($actual_err_sav)"
		sed 's/^/    /' < $actual_err_sav 
		echo "====End of actual stderr ($actual_err_sav)"
		echo ""
	    fi
	fi
    fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
	    rm -f $actual $actual_err $actual_sav $actual_err_sav
    	rm -f $actual_sorted $expect_sorted
    fi
}


# Print a "SKIP" message
SKIP() {
	 TESTING $H5DIFF $@
	  echo  " -SKIP-"
}



##############################################################################
# The tests 
# To avoid the printing of the complete full path of the test file, that hides
# all the other parameters for long paths, the printing of the command line 
# is done first in
# TESTING with the name only of the test file $TOOL, not its full path $TESTFILE
##############################################################################

# ############################################################################
# # Common usage
# ############################################################################

# 1.0
TOOLTEST h5diff_10.txt -h

# 1.1 normal mode
TOOLTEST h5diff_11.txt  $FILE1 $FILE2 

# 1.2 normal mode with objects
TOOLTEST h5diff_12.txt  $FILE1 $FILE2  g1/dset1 g1/dset2

# 1.3 report mode
TOOLTEST h5diff_13.txt -r $FILE1 $FILE2 

# 1.4 report  mode with objects
TOOLTEST h5diff_14.txt  -r $FILE1 $FILE2 g1/dset1 g1/dset2

# 1.5 with -d
TOOLTEST h5diff_15.txt --report --delta=5 $FILE1 $FILE2 g1/dset3 g1/dset4

# 1.6.1 with -p (int)
TOOLTEST h5diff_16_1.txt -v -p 0.02 $FILE1 $FILE1 g1/dset5 g1/dset6

# 1.6.2 with -p (unsigned long_long)
TOOLTEST h5diff_16_2.txt --verbose --relative=0.02 $FILE1 $FILE1 g1/dset7 g1/dset8

# 1.6.3 with -p (double)
TOOLTEST h5diff_16_3.txt -v -p 0.02 $FILE1 $FILE1 g1/dset9 g1/dset10

# 1.7 verbose mode
TOOLTEST h5diff_17.txt -v $FILE1 $FILE2   

# 1.7 test 32-bit INFINITY
TOOLTEST h5diff_171.txt -v $FILE1 $FILE1 /g1/fp19

# 1.7 test 64-bit INFINITY
TOOLTEST h5diff_172.txt -v $FILE1 $FILE1 /g1/fp20

# 1.8 quiet mode 
TOOLTEST h5diff_18.txt -q $FILE1 $FILE2 

# 1.8 -v and -q
TOOLTEST h5diff_18_1.txt -v -q $FILE1 $FILE2


# ##############################################################################
# # not comparable types
# ##############################################################################

# 2.0
TOOLTEST h5diff_20.txt -v $FILE3 $FILE3  dset g1

# 2.1
TOOLTEST h5diff_21.txt -v $FILE3 $FILE3 dset l1

# 2.2
TOOLTEST h5diff_22.txt -v  $FILE3 $FILE3 dset t1

# ##############################################################################
# # compare groups, types, links (no differences and differences)
# ##############################################################################

# 2.3
TOOLTEST h5diff_23.txt -v $FILE3 $FILE3 g1 g1

# 2.4
TOOLTEST h5diff_24.txt -v $FILE3 $FILE3 t1 t1

# 2.5
TOOLTEST h5diff_25.txt -v $FILE3 $FILE3 l1 l1 

# 2.6
TOOLTEST h5diff_26.txt -v $FILE3 $FILE3 g1 g2

# 2.7
TOOLTEST h5diff_27.txt -v $FILE3 $FILE3 t1 t2

# 2.8
TOOLTEST h5diff_28.txt -v $FILE3 $FILE3 l1 l2



# ##############################################################################
# # Dataset datatypes
# ##############################################################################

# 5.0
TOOLTEST h5diff_50.txt -v $FILE4 $FILE4 dset0a dset0b

# 5.1
TOOLTEST h5diff_51.txt -v $FILE4 $FILE4 dset1a dset1b

# 5.2
TOOLTEST h5diff_52.txt -v $FILE4 $FILE4 dset2a dset2b

# 5.3
TOOLTEST h5diff_53.txt -v $FILE4 $FILE4 dset3a dset4b

# 5.4
TOOLTEST h5diff_54.txt -v $FILE4 $FILE4 dset4a dset4b

# 5.5
TOOLTEST h5diff_55.txt -v $FILE4 $FILE4 dset5a dset5b

# 5.6
TOOLTEST h5diff_56.txt -v $FILE4 $FILE4 dset6a dset6b

# 5.7
TOOLTEST h5diff_57.txt -v $FILE4 $FILE4 dset7a dset7b

# 5.8 (region reference)
TOOLTEST h5diff_58.txt -v $FILE7 $FILE8 refreg

# ##############################################################################
# # Error messages
# ##############################################################################


# 6.0: Check if the command line number of arguments is less than 3
TOOLTEST h5diff_600.txt $FILE1 

# 6.1: Check if non-exist object name is specified 
TOOLTEST h5diff_601.txt $FILE1 $FILE1 nono_obj


# ##############################################################################
# # -d 
# ##############################################################################


# 6.3: negative value
TOOLTEST h5diff_603.txt -d -4 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.4: zero
TOOLTEST h5diff_604.txt -d 0 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.5: non number
TOOLTEST h5diff_605.txt -d u $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.6: hexadecimal
TOOLTEST h5diff_606.txt -d 0x1 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.7: string
TOOLTEST h5diff_607.txt -d "1" $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.8: use system epsilon 
TOOLTEST h5diff_608.txt --use-system-epsilon $FILE1 $FILE2  g1/dset3 g1/dset4

# 6.9: number larger than biggest difference
TOOLTEST h5diff_609.txt -d 200 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.10: number smaller than smallest difference
TOOLTEST h5diff_610.txt -d 1 $FILE1 $FILE2 g1/dset3 g1/dset4


# ##############################################################################
# # -p
# ##############################################################################


# 6.12: negative value
TOOLTEST h5diff_612.txt -p -4 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.13: zero
TOOLTEST h5diff_613.txt -p 0 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.14: non number
TOOLTEST h5diff_614.txt -p u $FILE1 $FILE2  g1/dset3 g1/dset4

# 6.15: hexadecimal
TOOLTEST h5diff_615.txt -p 0x1 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.16: string
TOOLTEST h5diff_616.txt -p "0.21" $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.17: repeated option
TOOLTEST h5diff_617.txt -p 0.21 -p 0.22 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.18: number larger than biggest difference
TOOLTEST h5diff_618.txt -p 2 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.19: number smaller than smallest difference
TOOLTEST h5diff_619.txt -p 0.005 $FILE1 $FILE2 g1/dset3 g1/dset4



# ##############################################################################
# # -n
# ##############################################################################

# 6.21: negative value
TOOLTEST h5diff_621.txt -n -4 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.22: zero
TOOLTEST h5diff_622.txt -n 0 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.23: non number
TOOLTEST h5diff_623.txt -n u $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.24: hexadecimal
TOOLTEST h5diff_624.txt -n 0x1 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.25: string
TOOLTEST h5diff_625.txt -n "2" $FILE1 $FILE2  g1/dset3 g1/dset4

# 6.26: repeated option
TOOLTEST h5diff_626.txt -n 2 -n 3 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.27: number larger than biggest difference
TOOLTEST h5diff_627.txt --count=200 $FILE1 $FILE2 g1/dset3 g1/dset4

# 6.28: number smaller than smallest difference
TOOLTEST h5diff_628.txt -n 1 $FILE1 $FILE2 g1/dset3 g1/dset4

# Disabling this test as it hangs - LRK 20090618
# 6.29  non valid files
#TOOLTEST h5diff_629.txt file1.h6 file2.h6


# ##############################################################################
# 7.  attributes
# ##############################################################################
TOOLTEST h5diff_70.txt -v $FILE5 $FILE6 

# ##################################################
#  attrs with verbose option level
# ##################################################

TOOLTEST h5diff_700.txt -v1 $FILE5 $FILE6 
TOOLTEST h5diff_701.txt -v2 $FILE5 $FILE6 
TOOLTEST h5diff_702.txt --verbose=1 $FILE5 $FILE6 
TOOLTEST h5diff_703.txt --verbose=2 $FILE5 $FILE6 

# same attr number , all same attr name
TOOLTEST h5diff_704.txt -v2 $ATTR_VERBOSE_LEVEL_FILE1 $ATTR_VERBOSE_LEVEL_FILE2 /g

# same attr number , some same attr name
TOOLTEST h5diff_705.txt -v2 $ATTR_VERBOSE_LEVEL_FILE1 $ATTR_VERBOSE_LEVEL_FILE2 /dset

# same attr number , all different attr name
TOOLTEST h5diff_706.txt -v2 $ATTR_VERBOSE_LEVEL_FILE1 $ATTR_VERBOSE_LEVEL_FILE2 /ntype

# different attr number , same attr name (intersected)
TOOLTEST h5diff_707.txt -v2 $ATTR_VERBOSE_LEVEL_FILE1 $ATTR_VERBOSE_LEVEL_FILE2 /g2

# different attr number , all different attr name 
TOOLTEST h5diff_708.txt -v2 $ATTR_VERBOSE_LEVEL_FILE1 $ATTR_VERBOSE_LEVEL_FILE2 /g3

# when no attributes exist in both objects
TOOLTEST h5diff_709.txt -v2 $ATTR_VERBOSE_LEVEL_FILE1 $ATTR_VERBOSE_LEVEL_FILE2 /g4

# file vs file
TOOLTEST h5diff_710.txt -v2 $ATTR_VERBOSE_LEVEL_FILE1 $ATTR_VERBOSE_LEVEL_FILE2

# ##############################################################################
# 8.  all dataset datatypes
# ##############################################################################
TOOLTEST h5diff_80.txt -v $FILE7 $FILE8 

# 9. compare a file with itself
TOOLTEST h5diff_90.txt -v $FILE2 $FILE2

# 10. read by hyperslab, print indexes
if test -n "$pmode" -a "$mydomainname" = hdfgroup.uiuc.edu; then
    # skip this test which sometimes hangs in some THG machines
    SKIP -v $FILE9 $FILE10
else
    TOOLTEST h5diff_100.txt -v $FILE9 $FILE10 
fi

# 11. floating point comparison
# double value
TOOLTEST h5diff_101.txt -v $FILE1 $FILE1 g1/d1  g1/d2 

# float value
TOOLTEST h5diff_102.txt -v $FILE1 $FILE1 g1/fp1 g1/fp2 

# with --use-system-epsilon for double value 
TOOLTEST h5diff_103.txt -v --use-system-epsilon $FILE1 $FILE1 g1/d1  g1/d2 

# with --use-system-epsilon for float value
TOOLTEST h5diff_104.txt -v --use-system-epsilon $FILE1 $FILE1 g1/fp1 g1/fp2 


# not comparable -c flag
TOOLTEST h5diff_200.txt $FILE2 $FILE2 g2/dset1  g2/dset2 

TOOLTEST h5diff_201.txt -c $FILE2 $FILE2 g2/dset1  g2/dset2 

TOOLTEST h5diff_202.txt -c $FILE2 $FILE2 g2/dset2  g2/dset3

TOOLTEST h5diff_203.txt -c $FILE2 $FILE2 g2/dset3  g2/dset4

TOOLTEST h5diff_204.txt -c $FILE2 $FILE2 g2/dset4  g2/dset5

TOOLTEST h5diff_205.txt -c $FILE2 $FILE2 g2/dset5  g2/dset6


# not comparable in compound
TOOLTEST h5diff_206.txt -c $FILE2 $FILE2 g2/dset7  g2/dset8

TOOLTEST h5diff_207.txt -c $FILE2 $FILE2 g2/dset8  g2/dset9

# ##############################################################################
# # Links compare without --follow-symlinks nor --no-dangling-links
# ##############################################################################
# test for bug1749
TOOLTEST h5diff_300.txt -v $FILE12 $FILE12 /link_g1 /link_g2

# ##############################################################################
# # Links compare with --follow-symlinks Only
# ##############################################################################
# soft links file to file
TOOLTEST h5diff_400.txt --follow-symlinks -v $FILE13 $FILE13

# softlink vs dset"
TOOLTEST h5diff_401.txt --follow-symlinks -v $FILE13 $FILE13 /softlink_dset1_1 /target_dset2

# dset vs softlink"
TOOLTEST h5diff_402.txt --follow-symlinks -v $FILE13 $FILE13 /target_dset2 /softlink_dset1_1

# softlink vs softlink"
TOOLTEST h5diff_403.txt --follow-symlinks -v $FILE13 $FILE13 /softlink_dset1_1 /softlink_dset2

# extlink vs extlink (FILE)"
TOOLTEST h5diff_404.txt --follow-symlinks -v $FILE15 $FILE15

# extlink vs dset"
TOOLTEST h5diff_405.txt --follow-symlinks -v $FILE15 $FILE16 /ext_link_dset1 /target_group2/x_dset

# dset vs extlink"
TOOLTEST h5diff_406.txt --follow-symlinks -v $FILE16 $FILE15 /target_group2/x_dset /ext_link_dset1

# extlink vs extlink"
TOOLTEST h5diff_407.txt --follow-symlinks -v $FILE15 $FILE15 /ext_link_dset1 /ext_link_dset2

# softlink vs extlink"
TOOLTEST h5diff_408.txt --follow-symlinks -v $FILE13 $FILE15 /softlink_dset1_1 /ext_link_dset2

# extlink vs softlink "
TOOLTEST h5diff_409.txt --follow-symlinks -v $FILE15 $FILE13 /ext_link_dset2 /softlink_dset1_1

# linked_softlink vs linked_softlink (FILE)"
TOOLTEST h5diff_410.txt --follow-symlinks -v $FILE14 $FILE14

# dset2 vs linked_softlink_dset1"
TOOLTEST h5diff_411.txt --follow-symlinks -v $FILE14 $FILE14 /target_dset2 /softlink1_to_slink2

# linked_softlink_dset1 vs dset2"
TOOLTEST h5diff_412.txt --follow-symlinks -v $FILE14 $FILE14 /softlink1_to_slink2 /target_dset2

# linked_softlink_to_dset1 vs linked_softlink_to_dset2"
TOOLTEST h5diff_413.txt --follow-symlinks -v $FILE14 $FILE14 /softlink1_to_slink2 /softlink2_to_slink2

# group vs linked_softlink_group1"
TOOLTEST h5diff_414.txt --follow-symlinks -v $FILE14 $FILE14 /target_group /softlink3_to_slink2

# linked_softlink_group1 vs group"
TOOLTEST h5diff_415.txt --follow-symlinks -v $FILE14 $FILE14 /softlink3_to_slink2 /target_group

# linked_softlink_to_group1 vs linked_softlink_to_group2"
TOOLTEST h5diff_416.txt --follow-symlinks -v $FILE14 $FILE14 /softlink3_to_slink2 /softlink4_to_slink2

# non-exist-softlink vs softlink"
TOOLTEST h5diff_417.txt --follow-symlinks -v $FILE13 $FILE13 /softlink_noexist /softlink_dset2

# softlink vs non-exist-softlink"
TOOLTEST h5diff_418.txt --follow-symlinks -v $FILE13 $FILE13 /softlink_dset2 /softlink_noexist

# non-exist-extlink_file vs extlink"
TOOLTEST h5diff_419.txt --follow-symlinks -v $FILE15 $FILE15 /ext_link_noexist2 /ext_link_dset2

# exlink vs non-exist-extlink_file"
TOOLTEST h5diff_420.txt --follow-symlinks -v $FILE15 $FILE15 /ext_link_dset2 /ext_link_noexist2

# extlink vs non-exist-extlink_obj"
TOOLTEST h5diff_421.txt --follow-symlinks -v $FILE15 $FILE15 /ext_link_dset2 /ext_link_noexist1

# non-exist-extlink_obj vs extlink"
TOOLTEST h5diff_422.txt --follow-symlinks -v $FILE15 $FILE15 /ext_link_noexist1 /ext_link_dset2

# extlink_to_softlink_to_dset1 vs dset2"
TOOLTEST h5diff_423.txt --follow-symlinks -v $FILE17 $FILE18 /ext_link_to_slink1 /dset2

# dset2 vs extlink_to_softlink_to_dset1"
TOOLTEST h5diff_424.txt --follow-symlinks -v $FILE18 $FILE17 /dset2 /ext_link_to_slink1

# extlink_to_softlink_to_dset1 vs extlink_to_softlink_to_dset2"
TOOLTEST h5diff_425.txt --follow-symlinks -v $FILE17 $FILE17 /ext_link_to_slink1 /ext_link_to_slink2


# ##############################################################################
# # Dangling links compare (--follow-symlinks and --no-dangling-links)
# ##############################################################################
# dangling links --follow-symlinks (FILE to FILE)
TOOLTEST h5diff_450.txt  --follow-symlinks -v $DANGLE_LINK_FILE1 $DANGLE_LINK_FILE2

# dangling links --follow-symlinks and --no-dangling-links (FILE to FILE)
TOOLTEST h5diff_451.txt  --follow-symlinks -v --no-dangling-links  $DANGLE_LINK_FILE1 $DANGLE_LINK_FILE2 

# try --no-dangling-links without --follow-symlinks options
TOOLTEST h5diff_452.txt  --no-dangling-links  $FILE13 $FILE13

# dangling link found for soft links (FILE to FILE)
TOOLTEST h5diff_453.txt  --follow-symlinks -v --no-dangling-links  $FILE13 $FILE13  

# dangling link found for soft links (obj to obj)
TOOLTEST h5diff_454.txt  --follow-symlinks -v --no-dangling-links  $FILE13 $FILE13 /softlink_dset2 /softlink_noexist 

# dangling link found for soft links (obj to obj) Both dangle links
TOOLTEST h5diff_455.txt  --follow-symlinks -v --no-dangling-links  $FILE13 $FILE13 /softlink_noexist /softlink_noexist 

# dangling link found for ext links (FILE to FILE)
TOOLTEST h5diff_456.txt  --follow-symlinks -v --no-dangling-links  $FILE15 $FILE15 

# dangling link found for ext links (obj to obj). target file exist
TOOLTEST h5diff_457.txt  --follow-symlinks -v --no-dangling-links  $FILE15 $FILE15 /ext_link_dset1 /ext_link_noexist1 

# dangling link found for ext links (obj to obj). target file NOT exist
TOOLTEST h5diff_458.txt  --follow-symlinks -v --no-dangling-links  $FILE15 $FILE15 /ext_link_dset1 /ext_link_noexist2  

# dangling link found for ext links (obj to obj). Both dangle links
TOOLTEST h5diff_459.txt  --follow-symlinks -v --no-dangling-links  $FILE15 $FILE15 /ext_link_noexist1 /ext_link_noexist2


# ##############################################################################
# # test for group diff recursivly
# ##############################################################################
# root 
TOOLTEST h5diff_500.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 / /
TOOLTEST h5diff_501.txt -v --follow-symlinks $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 / /

# root vs group
TOOLTEST h5diff_502.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 / /grp1/grp2/grp3

# group vs group (same name and structure)
TOOLTEST h5diff_503.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp1 /grp1

# group vs group (different name and structure)
TOOLTEST h5diff_504.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp1/grp2 /grp1/grp2/grp3

# groups vs soft-link
TOOLTEST h5diff_505.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp1 /slink_grp1
TOOLTEST h5diff_506.txt -v --follow-symlinks $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp1/grp2 /slink_grp2

# groups vs ext-link
TOOLTEST h5diff_507.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp1 /elink_grp1
TOOLTEST h5diff_508.txt -v --follow-symlinks $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp1 /elink_grp1

# soft-link vs ext-link
TOOLTEST h5diff_509.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /slink_grp1 /elink_grp1
TOOLTEST h5diff_510.txt -v --follow-symlinks $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /slink_grp1 /elink_grp1

# circled ext links
TOOLTEST h5diff_511.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp10 /grp11
TOOLTEST h5diff_512.txt -v --follow-symlinks $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /grp10 /grp11

# circled soft2ext-link vs soft2ext-link
TOOLTEST h5diff_513.txt -v $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /slink_grp10 /slink_grp11
TOOLTEST h5diff_514.txt -v --follow-symlinks $GRP_RECURSE_FILE1 $GRP_RECURSE_FILE2 /slink_grp10 /slink_grp11

###############################################################################
# Test for group recursive diff via multi-linked external links 
# With follow-symlinks, file $GRP_RECURSE1_EXT and $GRP_RECURSE2_EXT1 should
# be same with the external links.
###############################################################################
# file vs file
TOOLTEST h5diff_515.txt -v $GRP_RECURSE1_EXT $GRP_RECURSE2_EXT1
TOOLTEST h5diff_516.txt -v --follow-symlinks $GRP_RECURSE1_EXT $GRP_RECURSE2_EXT1
# group vs group
TOOLTEST h5diff_517.txt -v $GRP_RECURSE1_EXT $GRP_RECURSE2_EXT1 /g1
TOOLTEST h5diff_518.txt -v --follow-symlinks $GRP_RECURSE1_EXT $GRP_RECURSE2_EXT1 /g1

# ##############################################################################
# # Exclude objects (--exclude-path)
# ##############################################################################
#
# Same structure, same names and different value.
#
# Exclude the object with different value. Expect return - same
TOOLTEST h5diff_480.txt -v --exclude-path /group1/dset3 $EXCLUDE_FILE1_1 $EXCLUDE_FILE1_2
# Verify different by not excluding. Expect return - diff
TOOLTEST h5diff_481.txt -v $EXCLUDE_FILE1_1 $EXCLUDE_FILE1_2

#
# Different structure, different names. 
#
# Exclude all the different objects. Expect return - same
TOOLTEST h5diff_482.txt -v --exclude-path "/group1" --exclude-path "/dset1" $EXCLUDE_FILE2_1 $EXCLUDE_FILE2_2
# Exclude only some different objects. Expect return - diff
TOOLTEST h5diff_483.txt -v --exclude-path "/group1" $EXCLUDE_FILE2_1 $EXCLUDE_FILE2_2

# Exclude from group compare
TOOLTEST h5diff_484.txt -v --exclude-path "/dset3" $EXCLUDE_FILE1_1 $EXCLUDE_FILE1_2 /group1

# ##############################################################################
# # diff various multiple vlen and fixed strings in a compound type dataset
# ##############################################################################
TOOLTEST h5diff_530.txt -v  $COMP_VL_STRS_FILE $COMP_VL_STRS_FILE

# ##############################################################################
# # END
# ##############################################################################

if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
