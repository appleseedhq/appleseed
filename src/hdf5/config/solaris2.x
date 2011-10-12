#							-*- shell-script -*-
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


# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `sunpro cc'
if test "X-" =  "X-$CC"; then
  CC=cc
  CC_BASENAME=cc
fi

# Try gcc compiler flags
. $srcdir/config/gnu-flags

# Try solaris native compiler flags
if test "X-" = "X-$cc_flags_set"; then
  H5_CFLAGS="$H5_CFLAGS -erroff=%none -DBSD_COMP"
  # -g produces rather slow code. "-g -O" produces much faster code with some
  # loss of debugger functions such as not able to print local variables.
  DEBUG_CFLAGS="-g -O"
  DEBUG_CPPFLAGS=
  PROD_CFLAGS="-O -s"
  PROD_CPPFLAGS=
  PROFILE_CFLAGS=-xpg
  PROFILE_CPPFLAGS=
  cc_flags_set=yes
# Special linking flag is needed to build with Fortran on Solaris 5.9
    system_version="`uname -r`"
    case "$system_version" in
	5.9*)
	    # Need the xopenmp flag to build the Fortran library
	    if test X-$enable_fortran = X-yes; then
		AM_LDFLAGS="$AM_LDFLAGS -xopenmp=stubs"
	    fi
	    ;;
    esac

  # Turn off optimization flag for SUNpro compiler versions 4.x which
  # have an optimization bug.  Version 5.0 works.
  ($CC -V 2>&1) | grep -s 'cc: .* C 4\.' >/dev/null 2>&1 \
       && PROD_CFLAGS="`echo $PROD_CFLAGS | sed -e 's/-O//'`"
fi

LIBS="$LIBS"

# The default Fortran 90 compiler

if test "X-" = "X-$FC"; then
  FC=f90
fi

if test "X-" = "X-$f9x_flags_set"; then
  F9XSUFFIXFLAG=""
  FSEARCH_DIRS=""
  H5_FCFLAGS="$H5_FCFLAGS"
  # -g produces rather slow code. "-g -O" produces much faster code with some
  # loss of debugger functions such as not able to print local variables.
  DEBUG_FCFLAGS="-g -O"
  PROD_FCFLAGS="-O2"
  PROFILE_FCFLAGS=""
  f9x_flags_set=yes
fi

# The default C++ compiler

# The default compiler is `sunpro cc'
if test -z "$CXX"; then
  CXX=CC
  CXX_BASENAME=CC
fi

# Try gcc compiler flags
#. $srcdir/config/gnu-flags

cxx_version="`$CXX -V 2>&1 |grep 'WorkShop' |\
                sed 's/.*WorkShop.*C++ \([0-9\.]*\).*/\1/'`"

cxx_vers_major=`echo $cxx_version | cut -f1 -d.`
cxx_vers_minor=`echo $cxx_version | cut -f2 -d.`
cxx_vers_patch=`echo $cxx_version | cut -f3 -d.`

# Specify the "-features=tmplife" if the compiler can handle this...
if test -n "$cxx_version"; then
  if test $cxx_vers_major -ge 5 -a $cxx_vers_minor -ge 3 -o $cxx_vers_major -gt 5; then
    H5_CXXFLAGS="$H5_CXXFLAGS -features=tmplife"
  fi
fi

# Try solaris native compiler flags
if test -z "$cxx_flags_set"; then
  H5_CXXFLAGS="$H5_CXXFLAGS -instances=static"
  H5_CPPFLAGS="$H5_CPPFLAGS -LANG:std"
  # -g produces rather slow code. "-g -O" produces much faster code with some
  # loss of debugger functions such as not able to print local variables.
  DEBUG_CXXFLAGS="-g -O"
  DEBUG_CPPFLAGS=
  PROD_CXXFLAGS="-O -s"
  PROD_CPPFLAGS=
  PROFILE_CXXFLAGS=-xpg
  PROFILE_CPPFLAGS=
  cxx_flags_set=yes
fi

# compiler version strings
case $CC in
    *cc*)
        cc_version_info=`$CC $CFLAGS $H5_CFLAGS -V 2>&1 | grep 'Sun' |\
            sed 's/.*\(Sun.*\)/\1 /'`
        ;;

    *)
        echo "No match to get cc_version_info for $CC"
        ;;
esac
echo "C compiler '$CC' is $cc_version_info"

case $FC in
    # The PGI and Intel compilers are automatically detected below
    *f90*)
        fc_version_info=`$FC $FCFLAGS $H5_FCFLAGS -V 2>&1 | grep 'Sun' |\
            sed 's/.*\(Sun.*\)/\1 /'`
        ;;

     *)
        echo "No match to get fc_version_info for $FC"
        ;;
esac
echo "Fortran compiler '$FC' is $fc_version_info"

# get c++ version info
case $CXX in
    *CC*)
        cxx_version_info=`$CXX $CXXFLAGS $H5_CXXFLAGS -V 2>&1 | grep 'Sun' |\
            sed 's/.*\(Sun.*\)/\1 /'`
        ;;

    *)
        echo "No match to get cxx_version_info for $CXX"
        ;;
esac


