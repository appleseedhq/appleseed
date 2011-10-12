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
   TEST_PROG_PARA=ph5example
endif

# These are the programs that 'make all' or 'make tests' will build and
# that 'make check' will run.  List them in the order in which they should
# be run.
# We don't tell automake about these programs so that it doesn't try to
# compile them with the regular fortran compiler.
TEST_PROG=dsetexample fileexample rwdsetexample attrexample groupexample    \
       grpsexample grpdsetexample hyperslab selectele grpit refobjexample   \
       refregexample mountexample compound
TEST_SCRIPT=testh5fc.sh

# List files to be installed here
INSTALL_FILES=dsetexample.f90 fileexample.f90 rwdsetexample.f90             \
       attrexample.f90 groupexample.f90 grpsexample.f90 grpdsetexample.f90  \
       hyperslab.f90 selectele.f90 grpit.f90 refobjexample.f90              \
       refregexample.f90 mountexample.f90 compound.f90 ph5example.f90
INSTALL_SCRIPT_FILES = run-fortran-ex.sh

# Mark this directory as part of the Fortran API
FORTRAN_API=yes

# Some examples depend on files created by other examples.
grpdsetexample.chkexe_: grpsexample.chkexe_
refregexample.chkexe_: refobjexample.chkexe_
# rwdsetexample and attrexample both modify the same file created by
# dsetexample. Serialize them.
rwdsetexample.chkexe_: dsetexample.chkexe_
attrexample.chkexe_: rwdsetexample.chkexe_

# Tell automake how to build examples using h5fc
# Additional dependencies for the examples are listed below
if BUILD_PARALLEL_CONDITIONAL
$(EXTRA_PROG): $(H5FC_PP)
	$(H5FC_PP) $(H5CCFLAGS) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
else
$(EXTRA_PROG): $(H5FC)
	$(H5FC) $(H5CCFLAGS) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
endif

# Tell automake how to install examples
# Note: no '/' after DESTDIR.  Explanation in commence.am
EXAMPLEDIR=${DESTDIR}$(exec_prefix)/share/hdf5_examples/fortran

# List dependencies for each example.  Normally, automake would take
# care of this for us, but if we tell automake about the programs it
# will try to build them with the normal C compiler, not h5cc.  This is
# an inelegant way of solving the problem.
# All programs share the same build rule and a dependency on the main hdf5
# and fortran libraries above.
dsetexample: dsetexample.f90
fileexample: fileexample.f90
rwdsetexample: rwdsetexample.f90
attrexample: attrexample.f90
groupexample: groupexample.f90
grpsexample: grpsexample.f90
grpdsetexample: grpdsetexample.f90
hyperslab: hyperslab.f90
selectele: selectele.f90
grpit: grpit.f90
refobjexample: refobjexample.f90
refregexample: refregexample.f90
mountexample: mountexample.f90
compound: compound.f90
ph5example: ph5example.f90

include $(top_srcdir)/config/examples.am
include $(top_srcdir)/config/conclude.am
