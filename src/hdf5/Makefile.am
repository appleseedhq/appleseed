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
# This makefile mostly just reinvokes make in the various subdirectories
# but does so in the correct order.  You can alternatively invoke make from
# each subdirectory manually.
#
# Top-level HDF5 Makefile(.in)

# pmake has issues if variables are undefined.  Solve this problem in
# top-level Makefile by defining .MAKEFLAGS target to -V before pmake can
# encounter any undefined variables.
# Automake resists putting anything but variable definitions first in
# a Makefile.in, so we'll put a placebo comment here and use sed in
# bin/reconfigure to turn it into the .MAKEFLAGS target.  Sigh.  -JL 2005
# Configure should set AM_MAKEFLAGS to -V to solve this problem in
# subdirectories.
# NOTE: This means that invoking pmake in a subdirectory will not work.
#xxx.MAKEFLAGS:@AM_MAKEFLAGS@
#xxx	$(MAKE) all
#xxx

include $(top_srcdir)/config/commence.am

# Define subdirectories to build.
## Automake understands that `make distclean' should recurse into
## conditional subdirectories even if `make all' does not.
## We need to list the examples directory in the DIST_SUBDIRS variable
## so that it will be visited by `make distclean'
# Add this directory to SUBDIRS so that examples get built after tools
# but before examples in extra interfaces (c++ and fortran).
# Since we're explicitly listing DIST_SUBDIRS, we also need to list
# directories that are only conditionally built (so that their Makefiles
# are cleaned as well).
# Note that `make clean' will not affect the examples or doc directories. 

# Conditionals.  These conditionals are defined during configure
# Define each variable to empty if it is not used to placate pmake
if BUILD_PARALLEL_CONDITIONAL
   TESTPARALLEL_DIR =testpar
else
   TESTPARALLEL_DIR=
endif
if BUILD_CXX_CONDITIONAL
   CXX_DIR =c++
else
   CXX_DIR=
endif
if BUILD_FORTRAN_CONDITIONAL
   FORTRAN_DIR =fortran
else
   FORTRAN_DIR=
endif
if BUILD_HDF5_HL_CONDITIONAL
   HDF5_HL_DIR =hl
else
   HDF5_HL_DIR=
endif

SUBDIRS = src test $(TESTPARALLEL_DIR) tools . $(CXX_DIR) $(FORTRAN_DIR) \
          $(HDF5_HL_DIR) perform
DIST_SUBDIRS = src test testpar tools . c++ fortran hl perform examples

# Some files generated during configure that should be cleaned
DISTCLEANFILES=config/stamp1 config/stamp2

# Some files/directories generated during check that should be cleaned
CHECK_CLEANFILES+=*-tmp

# Define rules for lib, progs, check, and tests.
# These simply involve recursing into subdirectories.
test _test: check

lib progs check-p check-s:
	@@SETX@; for d in $(SUBDIRS); do                                \
	  if test $$d != .; then                                        \
	   (cd $$d && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;            \
	  fi;                                                           \
	done

# Make all, tests, and (un)install also recurse into perform directory
all-local:
	@cd perform && $(MAKE) $(AM_MAKEFLAGS) all

tests:
	@@SETX@; for d in $(SUBDIRS) perform; do                        \
	  if test $$d != .; then                                        \
	   (cd $$d && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;            \
	  fi;                                                           \
	done

install-exec-local:
	@cd perform && $(MAKE) $(AM_MAKEFLAGS) install
uninstall-local:
	@cd perform && $(MAKE) $(AM_MAKEFLAGS) uninstall

# Check-clean also recurses into examples directory
check-clean:
	@@SETX@; for d in $(SUBDIRS) examples perform; do               \
	  if test $$d != .; then                                        \
	   (cd $$d && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;            \
	  fi;                                                           \
	done
	$(RM) -rf prefix-tmp destdir-tmp

# Some C++ compilers/linkers will create a directory named ii_files in
# the root directory, which should be cleaned.
mostlyclean-local:
	if test -d ii_files; then                                       \
	  $(RM) -rf ii_files;                                           \
	fi

# 'make install' will now install examples, the same as 'make install-all'.
# 'make-install-all' will be redundant but will still work.
install: install-recursive install-examples
uninstall: uninstall-recursive uninstall-examples 

# 'make install-all' also installs examples
install-all:
	@$(MAKE) $(AM_MAKEFLAGS) install
uninstall-all:
	@$(MAKE) $(AM_MAKEFLAGS) uninstall

# Install examples in this directory and recursively
install-examples uninstall-examples:
	@@SETX@; for d in examples $(HDF5_INTERFACES) $(HL); do               \
	    (cd $$d && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;           \
	done

# Placeholder to remind users that docs are now in a separate repository.
install-doc:
	@echo "docs no longer live in this tree.  Use install-examples to install examples."

uninstall-doc:
	@echo "docs no longer live in this tree.  Use install-examples to install examples."

# `make check-install' or `make installcheck' checks that examples can 
#  be successfully built
installcheck-local:
	if test -n "${DESTDIR}"; then                                  \
          (cd ${DESTDIR}$(bindir) && pwd && ./h5redeploy -force);     \
        fi
	@(cd examples && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;

# check-install is just a synonym for installcheck
check-install: installcheck

# check-all-install tests all installation methods.
# Install via different mechanism and then compare against the default.
# Fine if only libXXX.a files are different since they may have been ranlib'ed.
check-all-install:
	@echo Installing to default location
	$(MAKE) install
	@echo Installing to different prefix location
	$(MAKE) prefix=${ROOT}/prefix-tmp install
	@echo Compare against the default installation.
	@echo Fine if only libXXX.a files are different.
	-diff -r prefix-tmp ${prefix}
	@echo Installing to different $$\DESTDIR location
	env DESTDIR=${ROOT}/destdir-tmp $(MAKE) install
	@echo Compare against the default installation.
	@echo Fine if only libXXX.a files are different.
	-diff -r destdir-tmp${prefix} ${prefix}

# Only source files in the src directory include tracing information,
# so 'make trace' only needs to recurse into that directory.
trace:
	@(cd src && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;

# Run tests with different Virtual File Drivers.
# Currently, only invoke check-vfd in the test directory.
check-vfd:
	@@SETX@; for d in src test; do                                 \
	  if test $$d != .; then                                       \
	    (cd $$d && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;          \
	  fi;                                                          \
	done

# Automake wants to make config.status depend on configure.  This
# makes sense, but config.status can't always be regenerated
# properly, which can cause builds to fail.
# This is a problem for our Daily Tests, which need to be able to
# 'make distclean' reliably before running configure.
# The simple solution is to override the dependency Automake supplies
# for config.status so that it will never be regenerated.
$(top_builddir)/config.status:

# Don't include conclude.am in root Makefile; tests target needs to
# recurse into perform directory as well as reguar subdirs.
