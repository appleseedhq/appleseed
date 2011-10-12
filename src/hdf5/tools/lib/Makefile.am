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
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include files in /src directory
INCLUDES=-I$(top_srcdir)/src

# This is our main target, the h5tools library.
noinst_LTLIBRARIES=libh5tools.la

libh5tools_la_SOURCES=h5tools.c h5tools_str.c h5tools_utils.c h5diff.c \
    h5diff_array.c h5diff_attr.c h5diff_dset.c h5diff_util.c h5trav.c  \
    h5tools_filters.c h5tools_ref.c h5tools_type.c

# Test program.  Link using libhdf5 and libh5tools
TEST_PROG=
check_PROGRAMS=$(TEST_PROG)

# Name libh5tools.la so that dependencies work out. Automake knows how
# to build 'libh5tools.la', but not '../../tools/lib/libh5tools.la'.
LDADD=libh5tools.la $(LIBHDF5)

include $(top_srcdir)/config/conclude.am

