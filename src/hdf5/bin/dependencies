#!/usr/bin/perl -w
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
my $depend_file;
my $new_depend_file;
my $srcdir;
my $top_srcdir;
my $top_builddir;

while ($_ = shift @ARGV) {
    if (/^--top_srcdir=([^ \t\n]*)/) {
        $top_srcdir = $1;
        $top_srcdir =~ s/\+/\\\+/g;
        $top_srcdir =~ s/\./\\\./g;
    } elsif (/^--top_builddir=([^ \t\n]*)/) {
        $top_builddir = $1;
        $top_builddir =~ s/\+/\\\+/g;
        $top_builddir =~ s/\./\\\./g;
    } else {
        $depend_file = $_;
        $new_depend_file = "$_.new";
        last;
    }
}

open(DEPEND, "<$depend_file") || die "cannot open file $depend_file: $!\n";
open(NEW, ">$new_depend_file") || die "cannot open file $new_depend_file: $!\n";

while (<DEPEND>) {
    s/\.o(\b)/\.lo$1/g;
    s/ $top_srcdir/ \$\(top_srcdir\)/g;
    s/ $top_builddir/ \$\(top_builddir\)/g;
    print NEW $_;
}

close(DEPEND);
close(NEW);

`mv $new_depend_file $depend_file`;
