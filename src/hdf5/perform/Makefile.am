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
# HDF5 Library Performance Makefile(.in)
#

include $(top_srcdir)/config/commence.am

INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/test -I$(top_srcdir)/tools/lib

# bin_PROGRAMS will be installed.
if BUILD_PARALLEL_CONDITIONAL
    bin_PROGRAMS=h5perf_serial h5perf
else
    bin_PROGRAMS=h5perf_serial
endif

# Add h5perf and h5perf_serial specific linker flags here
h5perf_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)
h5perf_serial_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

# Some programs are not built or run by default, but can be built by hand or by
# specifying --enable-build-all at configure time.
# Also, some of these programs should only be built in parallel.
if BUILD_PARALLEL_CONDITIONAL
    PARA_BUILD_ALL=benchpar mpi-perf
endif
if BUILD_ALL_CONDITIONAL
    BUILD_ALL_PROGS=$(PARA_BUILD_ALL)
endif

# Define programs that will be run in 'make check'
# List them in the order they should be run.
# Parallel test programs.
if BUILD_PARALLEL_CONDITIONAL
    TEST_PROG_PARA=h5perf perf
endif
# Serial test programs.
TEST_PROG = iopipe chunk overhead zip_perf perf_meta h5perf_serial $(BUILD_ALL_PROGS)

# check_PROGRAMS will be built but not installed.  Do not any executable
# that is in bin_PROGRAMS already. Otherwise, it will be removed twice in
# "make clean" and some systems, e.g., AIX, do not like it.
check_PROGRAMS= iopipe chunk overhead zip_perf perf_meta $(BUILD_ALL_PROGS) perf

h5perf_SOURCES=pio_perf.c pio_engine.c pio_timer.c
h5perf_serial_SOURCES=sio_perf.c sio_engine.c sio_timer.c

# These are the files that `make clean' (and derivatives) will remove from
# this directory.
CLEANFILES=*.h5 *.raw *.dat x-gnuplot perftest.out

# All of the programs depend on the main hdf5 library, and some of them
# depend on test or tools library.
LDADD=$(LIBHDF5)
h5perf_LDADD=$(LIBH5TOOLS) $(LIBH5TEST) $(LIBHDF5)
h5perf_serial_LDADD=$(LIBH5TOOLS) $(LIBH5TEST) $(LIBHDF5)
perf_LDADD=$(LIBH5TEST) $(LIBHDF5)
iopipe_LDADD=$(LIBH5TEST) $(LIBHDF5)
zip_perf_LDADD=$(LIBH5TOOLS) $(LIBH5TEST) $(LIBHDF5)
perf_meta_LDADD=$(LIBH5TEST) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
