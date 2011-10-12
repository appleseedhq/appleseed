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
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5-C++ Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include src, test, and c++/src directories
INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/test -I$(top_srcdir)/c++/src

# Shared C++ libraries aren't universally supported.
if CXX_SHARED_CONDITIONAL
else
  AM_LDFLAGS+=-static
endif

# These are our main targets.  They should be listed in the order to be
# executed, generally most specific tests to least specific tests.
TEST_PROG=testhdf5
check_PROGRAMS=$(TEST_PROG)

# The tests depend on the hdf5 library, test library, and the c++ library
LDADD=$(LIBH5TEST) $(LIBH5CPP) $(LIBHDF5)

testhdf5_SOURCES=testhdf5.cpp dsets.cpp tattr.cpp tcompound.cpp	\
	tfile.cpp tfilter.cpp th5s.cpp tlinks.cpp trefer.cpp	\
	ttypes.cpp tvlstr.cpp h5cpputil.cpp

# Tell conclude.am that these are C++ tests.
CXX_API=yes

# Some C++ compilers/linkers (PGI?) create a directory named "ii_files" that
# holds *.ii files, which are template entity instantiations.
# This entire directory should be cleaned.
mostlyclean-local:
	@if test -d ii_files; then                                       \
	  $(RM) -rf ii_files;                                           \
	fi

include $(top_srcdir)/config/conclude.am
