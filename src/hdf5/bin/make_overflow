#!/usr/bin/perl -w
require 5.003;
use strict;

# Global settings

# List of supported C types to generate overflow assignment code for
my @ctypes = ( () );

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

# Create assignment overflow #ifdefs
#
# Programmer: Quincey Koziol
# Creation Date: 2009/04/09

##############################################################################
# Parse a meaningful line (not a comment or blank line) into the appropriate
# data structure
#
sub parse_line ($) {
    my $line = shift;   # Get the line to parse

    # Parse get the type name and whether it's signed or unsigned
#print "line=$line\n";
    if($line =~ /.*SIGNED\s*;\s*$/ || $line =~ /.*UNSIGNED\s*;\s*$/) {
        my $name;           # The name of the type
        my $signed;         # Whether the type is signed or not

        # Get the type's name & signed status
        ($name, $signed) = ($line =~ /^\s*(\w*)\s*,\s*(\w*)\s*;\s*$/);
#print "name = '$name', signed = '$signed'\n";

        # Append the type to the list of C types already parsed
        push @ctypes, [$name, $signed];

    }
    # Unknown keyword
    else {
        die "unknown keyword: $line";
    }
}

##############################################################################
# Print the copyright into an open file
#
sub print_copyright ($) {
    my $fh = shift;

    print $fh "/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n";
    print $fh " * Copyright by The HDF Group.                                               *\n";
    print $fh " * Copyright by the Board of Trustees of the University of Illinois.         *\n";
    print $fh " * All rights reserved.                                                      *\n";
    print $fh " *                                                                           *\n";
    print $fh " * This file is part of HDF5.  The full HDF5 copyright notice, including     *\n";
    print $fh " * terms governing use, modification, and redistribution, is contained in    *\n";
    print $fh " * the files COPYING and Copyright.html.  COPYING can be found at the root   *\n";
    print $fh " * of the source code distribution tree; Copyright.html can be found at the  *\n";
    print $fh " * root level of an installed copy of the electronic HDF5 document set and   *\n";
    print $fh " * is linked from the top-level documents page.  It can also be found at     *\n";
    print $fh " * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *\n";
    print $fh " * access to either file, you may request a copy from help\@hdfgroup.org.     *\n";
    print $fh " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */\n";
}

##############################################################################
# Print the "do not change this file" warning
#
sub print_warning ($) {
    my $fh = shift;

    print $fh "\n/* Generated automatically by bin/make_overflow -- do not edit */\n";
    print $fh "/* Add new types to H5overflow.txt file */\n\n";
}

##############################################################################
# Print start of ifdef's to prevent a file from being re-included
#
sub print_startprotect ($$) {
    my ($fh, $file) = @_;

    # Clip off the ".h" part of the name
    $file =~ s/(\w*)\.h/$1/;

    # Print the ifdef info
    print $fh "\n#ifndef _${file}_H\n";
    print $fh "#define _${file}_H\n";
}

##############################################################################
# Print assignment overflow macros for each type
#
sub print_typemacros ($) {
    my $fh = shift;             # File handle for output file
    my ($src_aref, $dst_aref);  # References for each type's information

    # Print the descriptive comment
    print $fh "\n\n/* Each type in this file is tested for assignment to the other types,\n";
    print $fh " *      and range checks are defined for bad assignments at run-time.\n";
    print $fh " */\n";

    for $src_aref (@ctypes) {
        # Print a descriptive comment
        print $fh "\n/* Assignment checks for @$src_aref[0] */\n\n";

        for $dst_aref (@ctypes) {
            if (@$src_aref[0] ne @$dst_aref[0]) {
                # Print a descriptive comment
                print $fh "/* src: @$src_aref[0], dst: @$dst_aref[0] */\n";

                # Print actual type size checks & macro definitions
                print $fh "#if H5_SIZEOF_", uc @$src_aref[0], " < H5_SIZEOF_", uc @$dst_aref[0], "\n";
                print $fh "    #define ASSIGN_", @$src_aref[0], "_TO_", @$dst_aref[0], "(dst, dsttype, src, srctype) \\\n";
                if ( @$src_aref[1] eq @$dst_aref[1]) {
                    print $fh "        ASSIGN_TO_LARGER_SIZE_SAME_SIGNED(dst, dsttype, src, srctype)\n";
                } elsif ( @$src_aref[1] eq "SIGNED") {
                    print $fh "        ASSIGN_TO_LARGER_SIZE_SIGNED_TO_UNSIGNED(dst, dsttype, src, srctype)\n";
                } else {
                    print $fh "        ASSIGN_TO_LARGER_SIZE_UNSIGNED_TO_SIGNED(dst, dsttype, src, srctype)\n";
                }
                print $fh "#elif H5_SIZEOF_", uc @$src_aref[0], " > H5_SIZEOF_", uc @$dst_aref[0], "\n";
                print $fh "    #define ASSIGN_", @$src_aref[0], "_TO_", @$dst_aref[0], "(dst, dsttype, src, srctype) \\\n";
                print $fh "        ASSIGN_TO_SMALLER_SIZE(dst, dsttype, src, srctype)\n";
                print $fh "#else /* H5_SIZEOF_", uc @$src_aref[0], " == H5_SIZEOF_", uc @$dst_aref[0], " */\n";
                print $fh "    #define ASSIGN_", @$src_aref[0], "_TO_", @$dst_aref[0], "(dst, dsttype, src, srctype) \\\n";
                if ( @$src_aref[1] eq @$dst_aref[1]) {
                    print $fh "        ASSIGN_TO_SAME_SIZE_SAME_SIGNED(dst, dsttype, src, srctype)\n";
                } elsif ( @$src_aref[1] eq "SIGNED") {
                    print $fh "        ASSIGN_TO_SAME_SIZE_SIGNED_TO_UNSIGNED(dst, dsttype, src, srctype)\n";
                } else {
                    print $fh "        ASSIGN_TO_SAME_SIZE_UNSIGNED_TO_SIGNED(dst, dsttype, src, srctype)\n";
                }
                print $fh "#endif /* src: @$src_aref[0] dst: @$dst_aref[0] */\n\n";
            }
        }
    }

}

##############################################################################
# Print end of ifdef's to prevent a file from being re-included
#
sub print_endprotect ($$) {
    my ($fh, $file) = @_;

    # Clip off the ".h" part of the name
    $file =~ s/(\w*)\.h/$1/;

    # Print the endif info
    print $fh "#endif /* ${file}_H */\n\n";
}

##############################################################################
# Create the generated portion of the public header file
#
sub create_public ($) {
    my $prefix = shift;         # Get the prefix for the generated file
    my $file = "H5overflow.h";   # Name of file to generate
    my $name;                   # Name of function

    # Rename previous file
#    rename "${prefix}${file}", "${prefix}${file}~" or die "unable to make backup";

    # Open new header file
    open HEADER, ">${prefix}${file}" or die "unable to modify source";

    # Create file contents
    print_copyright(*HEADER);
    print_warning(*HEADER);
    print_startprotect(*HEADER, $file);
    print_typemacros(*HEADER);
    print_endprotect(*HEADER, $file);

    # Close header file
    close HEADER;
}

##############################################################################
# Read symbol version file (given as command-line argument) in and process it
# into internal data structures, then create header files.
#
my $file;       # Filename of input file

for $file (@ARGV) {
    my $prefix;         # Local prefix for generated files
    my $line;           # Line from input file

#print "file = '$file'\n";
    ($prefix) = ($file =~ /(^.*\/)/);
#print "prefix = '$prefix'\n";
    # Read in the entire file
    open SOURCE, $file or die "$file: $!\n";
    while ( defined ($line=<SOURCE>) ) {
        # Skip blank lines and those lines whose first character is a '#'
        if(!($line =~ /(^\s*#.*$)|(^\s*$)/)) {
            # Construct data structures for later printing
            parse_line($line);
        }
    }
    close SOURCE;
  
    # Create header files
    print "Generating 'H5overflow.h'\n";
    create_public($prefix);
}

