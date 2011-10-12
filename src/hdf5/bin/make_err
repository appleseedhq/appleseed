#!/usr/bin/perl -w
require 5.003;
$indent=4;

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

# Create error headers
#
# Read in the error description text file and create the appropriate headers
# needed by the library.
#
# Programmer: Quincey Koziol
# Creation Date: 2003/08/12

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

    print $fh "\n/* Generated automatically by bin/make_err -- do not edit */\n";
    print $fh "/* Add new errors to H5err.txt file */\n\n";
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
# Print end of ifdef's to prevent a file from being re-included
#
sub print_endprotect ($$) {
    my ($fh, $file) = @_;

    # Clip off the ".h" part of the name
    $file =~ s/(\w*)\.h/$1/;

    # Print the endif info
    print $fh "\n#endif /* ${file}_H */\n";
}

##############################################################################
# Parse a meaningful line (not a comment or blank line) into the appropriate
# data structure
#
sub parse_line ($) {
    my $line = shift;   # Get the line to parse
    my $name;           # The name of the error message
    my $desc;           # The description of the error message

    # Parse major error lines
#print "line=$line\n";
    if($line =~ /^\s*MAJOR,/) {
        # Get the major error's name & description
        ($name, $desc) = ($line =~ /^\s*MAJOR,\s*(\w*),\s*(.*)\n/);
#print "MAJOR: name=$name, desc=$desc\n";
        
        # Check if the name already exists as a major or minor error message
        if(exists($major{$name}) || exists($minor{$name})) {
            die "duplicated name: $name";
        }

        # Store the major errors in a hash table, indexed by the name
        $major{$name}=$desc;
    }
    # Parse minor error lines
    elsif($line =~ /^\s*MINOR,/) {
        my $min_section;           # Minor errors have a section they below to also

        # Get the minor error's section, name & description
        ($min_section, $name, $desc) = ($line =~ /^\s*MINOR,\s*(\w*),\s*(\w*),\s*(.*)\n/);
#print "MINOR: min_section=$min_section, name=$name, desc=$desc\n";
        
        # Check for valid section
        if(!exists($section{$min_section})) {
            die "unknown section: $min_section";
        }

        # Check if the name already exists as a major or minor error message
        if(exists($major{$name}) || exists($minor{$name})) {
            die "duplicated name: $name";
        }

        # Store the minor errors in a hash table, indexed by the name
        $minor{$name}=$desc;
        
        # Add the minor error to the list for the section
        push @{$section_list{$min_section}}, $name;
    }
    # Parse section lines
    elsif($line =~ /^\s*SECTION,/) {
        # Get the section's name & description
        ($name, $desc) = ($line =~ /^\s*SECTION,\s*(\w*),\s*(.*)\n/);
#print "SECTION: name=$name, desc=$desc\n";
        
        # Check if the section has already been defined
        if(exists($section{$name})) {
            die "duplicated name: $name";
        }

        # Store the section in a hash table, indexed by the name
        $section{$name}=$desc;
    }
    # Unknown keyword
    else {
      die "unknown keyword: $line";
    }
}

##############################################################################
# Create the generated portion of the public header file
#
sub create_public ($) {
    my $prefix = shift;         # Get the prefix for the generated file
    my $file = "H5Epubgen.h";   # Name of file to generate
    my $name;                   # Name of error message
    my $desc;                   # Description of error message
    my $sect_name;              # Section of minor error messages
    my $sect_desc;              # Description of section

    # Rename previous file
#    rename "${prefix}${file}", "${prefix}${file}~" or die "unable to make backup";

    # Open new header file
    open HEADER, ">${prefix}${file}" or die "unable to modify source";

    # Create file contents

    print_copyright(*HEADER);
    print_warning(*HEADER);
    print_startprotect(*HEADER, $file);

    # Iterate over all the major errors
    print HEADER "\n/*********************/\n";
    print HEADER   "/* Major error codes */\n";
    print HEADER   "/*********************/\n\n";
    foreach $name (keys %major) {
        printf HEADER "#define %-20s (H5OPEN %s_g)\n",$name,$name;
    }
    foreach $name (keys %major) {
        printf HEADER "H5_DLLVAR hid_t %-20s /* %s */\n","${name}_g;",$major{$name};
    }

    # Iterate over all the minor error sections
    print HEADER "\n/*********************/\n";
    print HEADER   "/* Minor error codes */\n";
    print HEADER   "/*********************/\n";
    while ( ($sect_name, $sect_desc) = each (%section)) {
        print HEADER "\n/* $sect_desc */\n";

        # Iterate over all the minor errors in each section
        for $name ( @{$section_list{$sect_name}}) {
            printf HEADER "#define %-20s (H5OPEN %s_g)\n",$name,$name;
        }
        for $name ( @{$section_list{$sect_name}}) {
            printf HEADER "H5_DLLVAR hid_t %-20s /* %s */\n","${name}_g;",$minor{$name};
        }
    }

    print_endprotect(*HEADER, $file);

    # Close header file
    close HEADER;
}

##############################################################################
# Create the generated portion of the H5E initialization code
#
sub create_init ($) {
    my $prefix = shift;         # Get the prefix for the generated file
    my $file = "H5Einit.h";     # Name of file to generate
    my $name;                   # Name of error message
    my $desc;                   # Description of error message
    my $sect_name;              # Section of minor error messages
    my $sect_desc;              # Description of section

    # Rename previous file
#    rename "${prefix}${file}", "${prefix}${file}~" or die "unable to make backup";

    # Open new header file
    open HEADER, ">${prefix}${file}" or die "unable to modify source";

    # Create file contents

    print_copyright(*HEADER);
    print_warning(*HEADER);
    print_startprotect(*HEADER, $file);

    # Iterate over all the major errors
    print HEADER "\n/*********************/\n";
    print HEADER   "/* Major error codes */\n";
    print HEADER   "/*********************/\n\n";
    foreach $name (keys %major) {
        print HEADER " "x(0*$indent),"assert(${name}_g==(-1));\n";
        print HEADER " "x(0*$indent),"if((msg = H5E_create_msg(cls, H5E_MAJOR, \"${major{$name}}\"))==NULL)\n";
        print HEADER " "x(1*$indent),"HGOTO_ERROR(H5E_ERROR, H5E_CANTINIT, FAIL, \"error message initialization failed\")\n";
        print HEADER " "x(0*$indent),"if((${name}_g = H5I_register(H5I_ERROR_MSG, msg, FALSE))<0)\n";
        print HEADER " "x(1*$indent),"HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, \"can't register error message\")\n";
    }

    # Iterate over all the minor error sections
    print HEADER "\n/*********************/\n";
    print HEADER   "/* Minor error codes */\n";
    print HEADER   "/*********************/\n\n";
    while ( ($sect_name, $sect_desc) = each (%section)) {
        print HEADER "\n"," "x(0*$indent),"/* $sect_desc */\n";

        # Iterate over all the minor errors in each section
        for $name ( @{$section_list{$sect_name}}) {
            print HEADER " "x(0*$indent),"assert(${name}_g==(-1));\n";
            print HEADER " "x(0*$indent),"if((msg = H5E_create_msg(cls, H5E_MINOR, \"${minor{$name}}\"))==NULL)\n";
            print HEADER " "x(1*$indent),"HGOTO_ERROR(H5E_ERROR, H5E_CANTINIT, FAIL, \"error message initialization failed\")\n";
            print HEADER " "x(0*$indent),"if((${name}_g = H5I_register(H5I_ERROR_MSG, msg, FALSE))<0)\n";
            print HEADER " "x(1*$indent),"HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, \"can't register error message\")\n";
        }
    }

    print_endprotect(*HEADER, $file);

    # Close header file
    close HEADER;
}

##############################################################################
# Create the generated portion of the H5E termination code
#
sub create_term ($) {
    my $prefix = shift;         # Get the prefix for the generated file
    my $file = "H5Eterm.h";     # Name of file to generate
    my $name;                   # Name of error message
    my $desc;                   # Description of error message
    my $sect_name;              # Section of minor error messages
    my $sect_desc;              # Description of section

    # Rename previous file
#    rename "${prefix}${file}", "${prefix}${file}~" or die "unable to make backup";

    # Open new header file
    open HEADER, ">${prefix}${file}" or die "unable to modify source";

    # Create file contents

    print_copyright(*HEADER);
    print_warning(*HEADER);
    print_startprotect(*HEADER, $file);

    # Iterate over all the major errors
    print HEADER "\n/* Reset major error IDs */\n";
    foreach $name (keys %major) {
        print HEADER " "x($indent),"\n${name}_g=";
    }
    print HEADER " (-1);\n";

    # Iterate over all the minor error sections
    print HEADER "\n/* Reset minor error IDs */\n";
    while ( ($sect_name, $sect_desc) = each (%section)) {
        print HEADER "\n"," "x(0*$indent),"\n/* $sect_desc */";

        # Iterate over all the minor errors in each section
        for $name ( @{$section_list{$sect_name}}) {
            print HEADER " "x($indent),"\n${name}_g=";
        }
    }
    print HEADER " (-1);\n";

    print_endprotect(*HEADER, $file);

    # Close header file
    close HEADER;
}

##############################################################################
# Create the generated portion of the error code definitions
#
sub create_define ($) {
    my $prefix = shift;         # Get the prefix for the generated file
    my $file = "H5Edefin.h";    # Name of file to generate
    my $name;                   # Name of error message
    my $desc;                   # Description of error message
    my $sect_name;              # Section of minor error messages
    my $sect_desc;              # Description of section

    # Rename previous file
#    rename "${prefix}${file}", "${prefix}${file}~" or die "unable to make backup";

    # Open new header file
    open HEADER, ">${prefix}${file}" or die "unable to modify source";

    # Create file contents

    print_copyright(*HEADER);
    print_warning(*HEADER);
    print_startprotect(*HEADER, $file);

    # Iterate over all the major errors
    print HEADER "\n/* Major error IDs */\n";
    foreach $name (keys %major) {
        printf HEADER "hid_t %-20s = FAIL;      /* %s */\n","${name}_g",$major{$name};
    }

    # Iterate over all the minor error sections
    print HEADER "\n/* Minor error IDs */\n";
    while ( ($sect_name, $sect_desc) = each (%section)) {
        print HEADER "\n/* $sect_desc */\n";

        # Iterate over all the minor errors in each section
        for $name ( @{$section_list{$sect_name}}) {
            printf HEADER "hid_t %-20s = FAIL;      /* %s */\n","${name}_g",$minor{$name};
        }
    }

    print_endprotect(*HEADER, $file);

    # Close header file
    close HEADER;
}

##############################################################################
# Read error file (given as command-line argument) in and process it into
# internal data structures, then create error header files.
#
for $file (@ARGV) {
    my $prefix;         # Local prefix for generated files

    ($prefix) = ($file =~ /(^.*\/)/);
    # Read in the entire file
    open SOURCE, $file or die "$file: $!\n";
    while ( defined ($line=<SOURCE>) ) {
        if(!($line =~ /(^\s*#.*$)|(^\s*$)/)) {
            # Construct data structures for later printing
            parse_line($line);
        }
    }
    close SOURCE;
  
    # Create header files
    print "Generating 'H5Epubgen.h'\n";
    create_public($prefix);
    print "Generating 'H5Einit.h'\n";
    create_init($prefix);
    print "Generating 'H5Eterm.h'\n";
    create_term($prefix);
    print "Generating 'H5Edefin.h'\n";
    create_define($prefix);
}
