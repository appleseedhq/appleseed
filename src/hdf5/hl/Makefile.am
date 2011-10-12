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
# 
# This makefile mostly just reinvokes make in the various subdirectories
# but does so in the correct order.  You can alternatively invoke make from
# each subdirectory manually.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# HDF5 High-Level Makefile(.in)

include $(top_srcdir)/config/commence.am

## Automake will automatically recurse into fortran directory for distclean
## if we define it conditionally.
if BUILD_FORTRAN_CONDITIONAL
  FORTRAN_DIR = fortran
endif
if BUILD_CXX_CONDITIONAL
  CXX_DIR = c++
endif

## Don't recurse into any subdirectories if HDF5 is not configured to
## use the HL library
if BUILD_HDF5_HL_CONDITIONAL
   SUBDIRS=src test tools $(CXX_DIR) $(FORTRAN_DIR)
endif
DIST_SUBDIRS=src test tools c++ fortran examples

# Install examples
install-examples uninstall-examples:
	@@SETX@; for d in examples $(HDF5_INTERFACES); do               \
	    (cd $$d && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;           \
	done

installcheck-local:
	@(cd examples && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;

# Clean examples when check-clean is invoked
check-clean ::
	@$(MAKE) $(AM_MAKEFLAGS) build-$@ || exit 1;
	@for d in X $(SUBDIRS) examples; do                                   \
	    if test $$d != X && test $$d != .; then                           \
	        (set -x; cd $$d && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;     \
	    fi;                                                               \
	done

build-check-clean:

include $(top_srcdir)/config/conclude.am
