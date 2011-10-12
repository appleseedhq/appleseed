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
# HDF5-Fortran Examples Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Compile parallel fortran examples only if parallel is enabled
if BUILD_PARALLEL_CONDITIONAL
   TEST_PROG_PARA=
endif

# These are the programs that 'make all' or 'make tests' will build and
# that 'make check' will run.  List them in the order in which they should
# be run.
# We don't tell automake about these programs so that it doesn't try to
# compile them with the regular fortran compiler.
TEST_PROG=exlite

# List files to be installed here
INSTALL_FILES= exlite.f90
INSTALL_SCRIPT_FILES = run-hlfortran-ex.sh

# Mark this directory as part of the Fortran API
FORTRAN_API=yes

# Tell automake how to build examples using h5fc
if BUILD_PARALLEL_CONDITIONAL
$(EXTRA_PROG): $(H5FC_PP)
	$(H5FC_PP) $(H5CCFLAGS) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
else
$(EXTRA_PROG): $(H5FC)
	$(H5FC) $(H5CCFLAGS) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
endif

# Tell automake how to install examples
# Note: no '/' after DESTDIR.  Explanation in commence.am
EXAMPLEDIR=${DESTDIR}$(exec_prefix)/share/hdf5_examples/hl/fortran

# List dependencies for each example.  Normally, automake would take
# care of this for us, but if we tell automake about the programs it
# will try to build them with the normal C compiler, not h5cc.  This is
# an inelegant way of solving the problem.
# All programs share the same build rule and a dependency on the main hdf5
# and fortran libraries above.

include $(top_srcdir)/config/examples.am
include $(top_srcdir)/config/conclude.am
