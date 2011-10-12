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
##
#
# hdf5 Parallel Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/test

# Test programs and scripts.  These are our main targets.
#
TEST_PROG_PARA=t_mpi t_posix_compliant testphdf5 t_cache t_pflush1 t_pflush2 t_shapesame
TEST_SCRIPT_PARA=testph5.sh

check_PROGRAMS = $(TEST_PROG_PARA)
check_SCRIPTS= $(TEST_SCRIPT)

testphdf5_SOURCES=testphdf5.c t_dset.c t_file.c t_mdset.c t_ph5basic.c \
        t_coll_chunk.c t_span_tree.c t_chunk_alloc.c t_filter_read.c

# The tests all depend on the hdf5 library and the test library
LDADD = $(LIBH5TEST) $(LIBHDF5)

# Temporary files
# MPItest.h5 is from t_mpi
# Para*.h5 are from testphdf
# go is used for debugging. See testphdf5.c.
CHECK_CLEANFILES+=MPItest.h5 Para*.h5 CacheTestDummy.h5 go
DISTCLEANFILES=testph5.sh

include $(top_srcdir)/config/conclude.am
