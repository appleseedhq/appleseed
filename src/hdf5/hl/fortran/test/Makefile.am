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
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# HDF5 High-Level Fortran Makefile(.in)

include $(top_srcdir)/config/commence.am

AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_builddir)/src -I$(top_srcdir)/hl/src
AM_FCFLAGS+=-I$(top_builddir)/fortran/src -I$(top_builddir)/hl/fortran/src $(F9XMODFLAG)$(top_builddir)/fortran/src $(F9XMODFLAG)$(top_builddir)/hl/fortran/src

# Some Fortran compilers can't build shared libraries, so sometimes we
# need to make sure the Fortran programs link against the static version
# of the HDF5 library.  We do this by passing the -static flag to the linker.
if FORTRAN_SHARED_CONDITIONAL
else
   AM_LDFLAGS+=-static
endif

# Our main target, the test programs
TEST_PROG=tstlite tstimage tsttable
check_PROGRAMS=$(TEST_PROG)

LDADD= $(LIBH5F_HL) $(LIBH5F) $(LIBH5_HL)  $(LIBHDF5)

# Source files for the programs
tstlite_SOURCES=tstlite.f90
tstimage_SOURCES=tstimage.f90
tsttable_SOURCES=tsttable.f90

# Temporary files.
CHECK_CLEANFILES+=dsetf[1-4].h5 f1img.h5 f1tab.h5

# Mark this directory as part of the Fortran API
FORTRAN_API=yes

include $(top_srcdir)/config/conclude.am
