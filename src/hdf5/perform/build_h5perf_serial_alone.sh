#! /bin/sh -x
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
# Name: build_h5perf_serial_alone.sh
#
# Puporse: Build the h5perf_serial tool by standalone mode.
#
# Created: Christian Chilan, 2008/09/02 
#
# Modification:
#

# Default to use h5cc to build h5perf_serial unless $H5CC is defined.
# 
h5cc=${H5CC:-h5cc}
$h5cc -DSTANDALONE sio_perf.c sio_engine.c sio_timer.c -o h5perf_serial
