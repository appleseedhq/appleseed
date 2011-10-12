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

# Include src and tools/lib directories
INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib

# Test programs and scripts
TEST_SCRIPT=testh5ls.sh
check_SCRIPTS=$(TEST_SCRIPT)
SCRIPT_DEPEND=h5ls$(EXEEXT)

# This is our main target, the h5ls tool
bin_PROGRAMS=h5ls

# Add h5ls specific linker flags here
h5ls_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

# All programs depend on the hdf5 and h5tools libraries
LDADD=$(LIBH5TOOLS) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
