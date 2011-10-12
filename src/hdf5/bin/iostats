#!/usr/bin/perl
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

# Usage: pipe the output of Linux's `strace' program into the stdin of
# this command, and the output of this command into gnuplot.

my ($fast,$npasses);
if ($ARGV[0] =~ /^--?fast$/) {
  $fast = 1;
  shift;
}

my $filename = shift || "tstab2.h5";
my $total = 0;
my %What;			# What{pos}{nbytes}{r|w} = naccesses
my($total_writes, $total_bytes_out, $total_reads, $total_bytes_in);

while (<>) {
  if (!defined $fd) {
    if (/^open\("(.*?)".*=\s+(\d+)/ && $1 eq $filename) {
      $fd = $2;
      $pos = 0;
    }
  } elsif (/^close\((\d+)/ && $1==$fd) {
    $fd = undef;
  } elsif (!$fast &&
	   /^lseek\((\d+), -?\d+,.*= (\d+)/ &&
	   $1==$fd && $2>=0) {
    $pos = $2;
  } elsif (!$fast && /^lseek\((\d+),/ && $1==$fd) {
    die $_;
  } elsif (/^write\((\d+), ".*?"(\.\.\.)?, \d+\)\s*= (\d+)/ &&
	   $1==$fd && $3>=0) {
    my $nbytes = $3;
    if ($fast) {
      $total_writes++;
      $total_bytes_out += $nbytes;
    } else {
      $What{$pos}{$nbytes}{w}++;
      printf "%d %d\n", $total, $pos;
      $pos += $nbytes;
      $total += $nbytes;
    }
  } elsif (/^write\((\d+),/ && $1==$fd) {
    die $_;
  } elsif (/^read\((\d+), ".*?"(\.\.\.)?, \d+\)\s*= (\d+)/ &&
	   $1==$fd && $3>=0) {
    my $nbytes = $3;
    if ($fast) {
      $total_reads++;
      $total_bytes_in += $nbytes;
    } else {
      $What{$pos}{$nbytes}{r}++;
      printf "%d %d\n", $total, $pos;
      $pos += $nbytes;
      $total += $nbytes;
    }
  } elsif (/^read\((\d+),/ && $1==$fd) {
    die $_;
  }
}
     

if (!$fast) {
  print "="x36, "\n"; 
  printf "%8s %8s %8s %8s\n","Position","NBytes","NReads","NWrites";
  for $pos (sort {$a<=>$b} keys %What) {
    for $nbytes (sort {$a<=>$b} keys %{$What{$pos}}) {
      printf("%8d %8d %8d %8d\n", $pos, $nbytes,
	     $What{$pos}{$nbytes}{r},
	     $What{$pos}{$nbytes}{w});
      $total_writes += $What{$pos}{$nbytes}{w};
      $total_reads  += $What{$pos}{$nbytes}{r};
      $total_bytes_out += $What{$pos}{$nbytes}{w} * $nbytes;
      $total_bytes_in  += $What{$pos}{$nbytes}{r} * $nbytes;
    }
  }
}

print "="x36, "\n";
printf("Write: %8d calls, %10d total bytes, %10g average bytes\n",
       $total_writes, $total_bytes_out, $total_bytes_out/$total_writes)
  if $total_writes;
printf("Read:  %8d calls, %10d total bytes, %10g average bytes\n",
       $total_reads, $total_bytes_in, $total_bytes_in/$total_reads)
  if $total_reads;
