#!/usr/bin/perl -p
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

# Usage: $0 [<].depend

# Takes dependency info and generates on stdout dependencies suitable for
# distribution by removing all the system include files from the list and
# removing all but the base name of other include files (since the Makefiles
# contain the logic for searching).

($h,$_)=/\s*\\/?$h.$`:("",$h.$_);
s|( +/\S*)*( *)|$2?" \\\n   ":""|eg;
#s|(([-\w\.]+)/)+([-\w\.]+)|\3|g;
