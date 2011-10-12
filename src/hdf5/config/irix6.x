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
# See BlankForm in this directory for details.

# Disable dependency tracking on IRIX unless the user specifically asks for
# it.
# IRIX's pmake confuses automake (as of version 1.9) if dependency tracking
# is enabled and it is not an in-place build.  Simply disabling dependency
# tracking on IRIX is simpler to implement than detecting pmake, detecting
# when a build is not in-place, and then disabling dependency tracking.
if test -z "${enable_dependency_tracking}"; then
  enable_dependency_tracking="no"
fi

# Use SGI supplied C compiler by default.  There is no ranlib
if test "X-" = "X-$CC"; then
    CC='cc'
    CC_BASENAME=cc
    # use c99 compiler if available.
    if `c99 -version >/dev/null 2>&1` ; then
        CC='c99'
    fi
fi
RANLIB=:

# Compiler flags
case "X-$CC_BASENAME" in
  X-gcc)
    . $srcdir/config/gnu-flags
    ;;

  *)
    if [ "$CC_BASENAME" = "cc" ] || ($CC -version 2>&1 | grep -s "MIPSpro Compilers") 2>&1 > /dev/null; then
      # use these flags if this is the SGI cc compiler or some compiler
      # command that eventually uses the SGI cc compiler.

      # Check for old versions of the compiler that don't work right.
      case "`$CC -version 2>&1 |head -1`" in
        "Mongoose Compilers: Version 7.00")
          echo "  +---------------------------------------------------+"
          echo "  | You have an old version of cc (Mongoose Compilers |"
          echo "  | version 7.00).  Please upgrade to MIPSpro version |"
          echo "  | 7.2.1.2m (patches are available from the SGI web  |"
          echo "  | site).  The 7.00 version may generate incorrect   |"
          echo "  | code, especially when optimizations are enabled.  |"
          echo "  +---------------------------------------------------+"
          sleep 5
          ;;
      esac

      # Always turn off these compiler warnings for the -64 compiler:
      #    1174:  function declared but not used
      #    1196:  __vfork() (this is an SGI config problem)
      #    1209:  constant expressions
      #    1429:  the `long long' type is not standard
      #    1685:  turn off warnings about turning off invalid warnings
      #    3201:  remark - parameter not referenced
      #H5_CFLAGS="$H5_CFLAGS -woff 1174,1429,1209,1196,1685,3201"
      H5_CFLAGS="$H5_CFLAGS -woff 1209,3201"

      # Always turn off these compiler warnings for the old compiler:
      #    799:   the `long long' type is not standard
      #    803:   turn off warnings about turning off invalid warnings
      #    835:   __vfork() (this is an SGI config problem)
      #H5_CFLAGS="$H5_CFLAGS -woff 799,803,835"

      # Always turn off these loader warnings:
      # (notice the peculiar syntax)
      #      47:  branch instructions that degrade performance on R4000
      #      84:  a library is not used
      #      85:  duplicate definition preemption (from -lnsl)
      #     134:  duplicate weak definition preemption (from -lnsl)
      H5_CFLAGS="$H5_CFLAGS -Wl,-woff,47,-woff,84,-woff,85,-woff,134"
    fi

    # Extra debugging flags
    DEBUG_CFLAGS="-g -fullwarn"
    DEBUG_CPPFLAGS=

    # Extra production flags
    PROD_CFLAGS="-O -OPT:Olimit=0 -s"
    PROD_CPPFLAGS=

    # Extra profiling flags
    PROFILE_CFLAGS=
    PROFILE_CPPFLAGS=
    ;;
esac

# The default Fortran 90 compiler

if test "X-" = "X-$FC"; then
  FC="f90"
fi

if test "X-" = "X-$f9x_flags_set"; then
  F9XSUFFIXFLAG=""
  FSEARCH_DIRS=""
  H5_FCFLAGS="$H5_FCFLAGS -mips4 -O -s"
  DEBUG_FCFLAGS="-mips4 -O -s"
  PROD_FCFLAGS="-mips4 -O -s"
  PROFILE_FCFLAGS="-mips4 -O -s"
  f9x_flags_set=yes
fi

# The default C++ compiler

# The default compiler is `MIPSpro CC'
if test -z "$CXX"; then
  CXX=CC
  CXX_BASENAME=CC
fi

# Try native compiler flags
if test -z "$cxx_flags_set"; then
  # -LANG:std required for std use; -ptused causes templates used to be
  # instantiated
  AM_CPPFLAGS="$AM_CPPFLAGS -LANG:std"
  H5_CPPFLAGS="$H5_CPPFLAGS -ptused"

  # libCio is a default library, since libtool before 1.5 doesn't fully 
  # support C++ yet, default libraries must be explicitly specified.
  # A new macro is used for this temporary and specific task so it 
  # won't polute the existing configuration 
  DEFAULT_LIBS="-lCio"

  DEBUG_CXXFLAGS=-g
  DEBUG_CPPFLAGS=
  PROD_CXXFLAGS="-O -s"
  PROD_CPPFLAGS=
  PROFILE_CXXFLAGS=-xpg
  PROFILE_CPPFLAGS=
  cxx_flags_set=yes
fi

# Hard set flag to indicate that the 'unsigned long long' to floating-point
# value conversion are broken by the compilers (as of 4/27/04 - QAK)
hdf5_cv_ulong_to_fp_bottom_bit_accurate=${hdf5_cv_ulong_to_fp_bottom_bit_accurate='no'}

# Set flags to avoid conversion between 'long double' and integers because of 
# SGI's compiler problems.  For both IRIX64 6.5 and IRIX 6.5, the compilers
# have the following problems,
#       long double -> signed char : incorrect rounding
#       long double -> unsigned char : incorrect rounding
#       long double -> short : incorrect rounding
#       long double -> unsigned short : incorrect rounding
#       long double -> long or long long: incorrect value
#       long double -> unsigned long or long long : incorrect value
#
#       long or long long -> long double : correct value but incorrect bit pattern
#       unsigned long or long long -> long double : correct value but incorrect bit pattern
# (1/5/05 - SLU)
hdf5_cv_ldouble_to_integer_accurate=${hdf5_cv_ldouble_to_integer_accurate='no'}
hdf5_cv_integer_to_ldouble_accurate=${hdf5_cv_integer_to_ldouble_accurate='no'}

# For IRIX 6.5, any version that is older than MIPSpro 7.3.1.3m, 
# the MPI derived datatype is not working.
# Versions 7.4.2m or newer work.
# Up to version 7.4.4m, it cannot handle collective IO with non-contribution 
# of some processes.
# Fix $hdf5_cv_mpi_complex_derived_datatype_works if it is not set and is using cc.
if [ -z "$hdf5_cv_mpi_complex_derived_datatype_works" -a $CC_BASENAME = cc ]; then
    ccversion=`$CC -version 2>&1 | sed -e 's/.*Version //p'`
    ccversion1=`echo $ccversion | cut -f1 -d.`
    ccversion2=`echo $ccversion | cut -f2 -d.`
    # Assume all versions 7.4.* or newer are okay
    # and assume ccversion2 is never larger than 99.
    ccversionval=`expr $ccversion1 \* 100 + $ccversion2`
    hdf5_cv_mpi_special_collective_io_works='no'
    if [ $ccversionval -lt 704 ]; then
        hdf5_cv_mpi_complex_derived_datatype_works='no'
#        hdf5_cv_mpi_special_collective_io_works='no'
    fi
fi

# Set flag to generate alternate code for H5V_log2_gen, to avoid
# problems with the MIPSpro compiler 7.30 and IRIX64 6.5 (ie. other
# combinations might work, but haven't been tested)
# (9/15/06 - QAK)
hdf5_cv_bad_log2_code_generated=${hdf5_cv_bad_log2_code_generated='yes'}
