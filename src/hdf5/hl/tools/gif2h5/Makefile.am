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
INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib -I$(top_srcdir)/hl/src

# These are our main targets, the tools

TEST_SCRIPT=$(srcdir)/h52giftest.sh
check_SCRIPTS=$(TEST_SCRIPT)

bin_PROGRAMS=gif2h5 h52gif
noinst_PROGRAMS=h52gifgentst

# Add h52gif and gif2h5 specific linker flags here
h52gif_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)
gif2h5_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

gif2h5_SOURCES=gif2hdf.c gif2mem.c decompress.c gifread.c writehdf.c 

h52gif_SOURCES=hdf2gif.c hdfgifwr.c 

h52gifgentst_SOURCES=h52gifgentst.c 

# Programs all depend on the hdf5 library, the tools library, and the HL
# library.
LDADD=$(LIBH5_HL) $(LIBH5TOOLS) $(LIBHDF5)

CHECK_CLEANFILES+=*.h5
CHECK_CLEANFILES+=*.gif

include $(top_srcdir)/config/conclude.am
