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

#
#    Generates an ASCII and Excel-importable file of tables representing
#    the output of running the "pio_perf" command. The name of the input
#    file is important. The name should reflect the command-line options
#    used in the performance test. It needs to be of the form:
#
#        f#[GMK].i#.d#.X#[GMK].x#[GMK]..*
#
#    For example:
#
#        PIO_output_f1G.i2.d1.X2M.x128K.frost
#
#    for a 1GB sized file ran for 2 iterations with 1 dataset from xfer
#    buffer size of 128KB to 2MB on the frost machine.
#
#    The output file will have the same name as the input, but will append
#    ".ascii" for the ASCII file and ".excel" for the Excel-importable
#    file.
#
#    The data structure used in this program looks like:
#
#        %results = {
#            num_proc => (
#                %xfer_size => (
#                    %posix = {
#                        'write-only' => ##,
#                        'write-close' => ##,
#                        'read-only' => ##,
#                        'read-close' => ##
#                    },
#                    %mpio = {
#                        'write-only' => ##,
#                        'write-close' => ##,
#                        'read-only' => ##,
#                        'read-close' => ##
#                    },
#                    %phdf = {
#                        'write-only' => ##,
#                        'write-close' => ##,
#                        'read-only' => ##,
#                        'read-close' => ##
#                    }
#                )
#            )
#        }

use IO::Handle;
use Getopt::Long;
use List::Util qw[max];

if ($#ARGV == -1) {
	usage();
}

my ($ascii_output, $excel_output);

GetOptions("data_type=s"=>\$data_type, 
            "buffer_size=i"=>\$transfer_buffer_size,
            "procs=i"=>\$num_procs_graph,
	    "help!"=>\$help,
	    "throughput=s"=>\$throughput_type,
	    "io_type=i"=>\$io_type,
            "3d!"=>\$plot_3d);

usage() if $help or !@ARGV;

$throughput_type = "average" if !$throughput_type;
$io_type = 7 if !$io_type;

foreach my $arg (@ARGV) {

	if ($arg !~ /^-/) {
		$arg =~ /f([0-9]+.)\.i([0-9]+)\.d([0-9]+)\.X([0-9]+.)\.x([0-9]+.)\.(.*)/;

		my $output = $arg . $1 . ".X" . $4 . ".x" . $5 . "." . $6;

		$ascii_output = $output . ".ascii";
		$excel_output = $output . ".excel";

		open(INPUT, "<$arg") or die "error: cannot open file $arg: $!\n";
		open(ASCII_OUTPUT, ">$ascii_output") or
		    die "error: cannot open file $ascii_output: $!\n";
		open(EXCEL_OUTPUT, ">$excel_output") or
		    die "error: cannot open file $excel_output: $!\n";
	} 
	else
	{
		die "error: unrecognized option: $arg\n";
	}
}

my %results;
my $num_procs = 0;
my ($xfer_size, $avg_type, $type);

my $posix = 0, $mpio = 1, $phdf5 = 2;

##"==== End of Parameters ===="

while (<INPUT>) {
	if (/Number of processors = ([0-9]+)/) {
		$num_procs = $1;
	}

	if (/Transfer Buffer Size: ([0-9]+)/) {
		$xfer_size = $1;
	}

	$type = $posix if /POSIX/;
	$type = $mpio if /MPIO/;
	$type = $phdf5 if /PHDF5/;

	if (/Write Open/) {
		$avg_type = "write-close";
	} elsif (/Write/) {
		$avg_type = "write-only";
	} elsif (/Read Open/) {
		$avg_type = "read-close";
	} elsif (/Read/) {
	    $avg_type = "read-only";
	}

	if($throughput_type eq "max")
	{
	    if (/Maximum Throughput: ( {0,2}[0-9]+\.[0-9]{2}) MB\/s/) {
		$results{$num_procs}{$xfer_size}[$type]{$avg_type} = $1;
	    }
	}
	elsif($throughput_type eq "min")
	{
	    if (/Minimum Throughput: ( {0,2}[0-9]+\.[0-9]{2}) MB\/s/) {
		$results{$num_procs}{$xfer_size}[$type]{$avg_type} = $1;
	    }
	}
	elsif($throughput_type eq "average")
	{
	    if (/Average Throughput: ( {0,2}[0-9]+\.[0-9]{2}) MB\/s/) {
		$results{$num_procs}{$xfer_size}[$type]{$avg_type} = $1;
	    }
	}
}

sub usage {
	print "Usage: gen_reporl.pl [options] FILE
	options are:\n
	-data_type \"data_type\" plots the results for \"write-only\",\"read-only\", \"write-close\", or \"read-close\" (default is write-only)\n
	-buffer_size \"buffer_size\" plots data from this buffer size (in kilobytes, default is 128)\n
	-procs \"num_procs\" plots data from the run with num_procs processors (default is the highest number of processors for which there is data).\n
	-throughput \"throughput_type\" plots either the \"max\", \"min\", or \"average\" throughput (default is average)\n
	-io_type  \"io_type\" where \"io_type\" is the bitwise or of the io_type for which plotting is desired (1 for POSIX, 2 for MPIO, 4 for PHDF5 (default is 7 (all))\n
	-3d	if present, does a 3d plot in addition to the normal ones\n";
			
	exit 1;
}

sub create_excel_output_header {
	my $output_header;
	my $kb = 1024;
	my $mb = $kb * $kb;

	foreach my $key (sort { $a <=> $b } keys(%{$results{$num_procs}})) {
		if ($key < $mb) {
			$key /= $kb;
			$output_header .= "\t". $key . "K";
		} else {
			$key /= $mb;
			$output_header .= "\t". $key . "M";
		}
	}

	$output_header;
}

sub create_excel_output_string {
	my ($t) = @_;
	my $output_content = "";

	foreach my $procs (sort { $b <=> $a } keys(%results)) {
		$output_content .= "\n$procs Procs";
		$output_content .= "\n" . create_excel_output_header;
		$output_content .= "\n  POSIX";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content .= "\t$results{$procs}{$xfer}[0]{$t}";
		}

		$output_content .= "\n  MPIO";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content .= "\t$results{$procs}{$xfer}[1]{$t}";
		}

		$output_content .= "\n  PHDF5";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content .= "\t$results{$procs}{$xfer}[2]{$t}";
		}

		$output_content .= "\n";
	}

	$output_content;
}

sub is_defined {
	my ($t) = @_;
	my $def = 1;

	foreach my $procs (sort { $b <=> $a } keys(%results)) {
		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			if (!defined($results{$procs}{$xfer}[0]{$t})) {
				$def = 0;
			}
		}

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			if (!defined($results{$procs}{$xfer}[0]{$t})) {
				$def = 0;
			}
		}

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			if (!defined($results{$procs}{$xfer}[0]{$t})) {
				$def = 0;
			}
		}
	}

	$def;
}

sub write_excel_file {
	print EXCEL_OUTPUT "\nWrite-Only\n";
	print EXCEL_OUTPUT create_excel_output_string("write-only");
	print EXCEL_OUTPUT "\nWrite-Close\n";
	print EXCEL_OUTPUT create_excel_output_string("write-close");

	if (is_defined("read-only")) {
		print EXCEL_OUTPUT "\nRead-Only\n";
		print EXCEL_OUTPUT create_excel_output_string("read-only");
		print EXCEL_OUTPUT "\nRead-Close\n";
		print EXCEL_OUTPUT create_excel_output_string("read-close");
	}
}

sub create_ascii_output_header {
	my $output_header = " " x 12 . "|";
	my $kb = 1024;
	my $mb = $kb * $kb;

	foreach my $key (sort { $a <=> $b } keys(%{$results{$num_procs}})) {
		if ($key < $mb) {
			$key /= $kb;
			$output_header = sprintf("$output_header   %-4s   |", $key .  "K");
		} else {
			$key /= $mb;
			$output_header = sprintf("$output_header   %-4s   |", $key .  "M");
		}
	}

	$output_header;
}

sub create_ascii_output_string {
	my ($t) = @_;
	my $output_content = "";
	my $output_header = create_ascii_output_header;

	foreach my $procs (sort { $b <=> $a } keys(%results)) {
		$output_content .= "\n$procs Procs";
		$output_content .= "\n$output_header\n";
		$output_content .= "-" x length($output_header);
		$output_content .= "\n     POSIX  |";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content = sprintf("$output_content   %-6s |",
									   $results{$procs}{$xfer}[0]{$t});
		}

		$output_content .= "\n    ";
		$output_content .= "-" x (length($output_header) - 4);
		$output_content .= "\n     MPI/IO |";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content = sprintf("$output_content   %-6s |",
									   $results{$procs}{$xfer}[1]{$t});
		}

		$output_content .= "\n    ";
		$output_content .= "-" x (length($output_header) - 4);
		$output_content .= "\n     PHDF5  |";

		foreach my $xfer (sort { $a <=> $b } keys(%{$results{$procs}})) {
			$output_content = sprintf("$output_content   %-6s |",
									   $results{$procs}{$xfer}[2]{$t});
		}

		$output_content .= "\n";
		$output_content .= "=" x length($output_header);
		$output_content .= "\n";
	}

	$output_content;
}

sub write_ascii_file {
	print ASCII_OUTPUT "\nWrite-Only";
	print ASCII_OUTPUT "\n----------\n";
	print ASCII_OUTPUT create_ascii_output_string("write-only");
	print ASCII_OUTPUT "\n\nWrite-Close";
	print ASCII_OUTPUT "\n-----------\n";
	print ASCII_OUTPUT create_ascii_output_string("write-close");

	if (is_defined("read-only")) {
		print ASCII_OUTPUT "\n\nRead-Only";
		print ASCII_OUTPUT "\n---------\n";
		print ASCII_OUTPUT create_ascii_output_string("read-only");
		print ASCII_OUTPUT "\n\nRead-Close";
		print ASCII_OUTPUT "\n----------\n";
		print ASCII_OUTPUT create_ascii_output_string("read-close");
	}
}

sub draw_plot
{
    my($p_3d) = @_;
    
    if($p_3d)
    {
	$counter = 3;
	print GNUPLOT_PIPE "splot ";
    }
    else
    {
	$counter = 2;
	print GNUPLOT_PIPE "plot ";
    }

    if($io_type & 1) {
	print GNUPLOT_PIPE " \"gnuplot.data\" using 1:";
	
	if($p_3d) { print GNUPLOT_PIPE "2:"; }       
	
	print GNUPLOT_PIPE $counter . " title 'POSIX' with linespoints";
	$counter = $counter + 1;
    }
    if($io_type & 2) {
	if($io_type & 1) { print GNUPLOT_PIPE ", "; }
	print GNUPLOT_PIPE  "\"gnuplot.data\" using 1:";
	
	if($p_3d) { print GNUPLOT_PIPE "2:"; }       
	
	print GNUPLOT_PIPE  $counter . " title 'MPIO' with linespoints";
	$counter = $counter + 1;
	if($io_type & 4) { print GNUPLOT_PIPE ", ";}
    }
    if($io_type & 4) {
	print GNUPLOT_PIPE "  \"gnuplot.data\" using 1:";
	
	if($p_3d) { print GNUPLOT_PIPE "2:"; }       
	
	print GNUPLOT_PIPE  $counter . " title 'PHDF5' with linespoints";

    }
    print GNUPLOT_PIPE "\n";
}


sub plot_default_graph1 {
	open(GNUPLOT_DATA_OUTPUT, ">gnuplot.data") or
		die "error: cannot open file gnuplot.data: $!\n";
	
	$transfer_buffer_size = 128 if !$transfer_buffer_size;
	$data_type = "write-only" if !$data_type;

	#set up the plot
	print GNUPLOT_PIPE  "set term x11 1\n";
	print GNUPLOT_PIPE  "set xlabel \"Number of Processors\"\n";
	print GNUPLOT_PIPE  "set title \"" . $data_type . " Performance (Speed vs. Num. Procs)\"\n";
	print GNUPLOT_PIPE  "set ylabel \"Bandwdith (MB/s)\"\n";
	print GNUPLOT_PIPE  "set label 1 \"Transfer buffer size: " . $transfer_buffer_size . "K\" at graph 0.7, graph 0.7 left \n";

#the next line attempts to hack gnuplot to get around it's inability to linearly scale, but logarithmically label an axis
	print GNUPLOT_PIPE  "set xtics (\"1\" 1, \"2\" 2, \"4\" 4, \"8\" 8, \"16\" 16, \"32\" 32, \"64\" 64, \"128\" 128, \"256\" 256, \"512\" 512, \"1024\" 1024)\n";


	foreach $proc (sort { $a <=> $b }( keys %results )) 
	{
	    print GNUPLOT_DATA_OUTPUT $proc . "\t";
	    if($io_type & 1) {
		print GNUPLOT_DATA_OUTPUT $results{$proc}{$transfer_buffer_size*1024}[0]{$data_type} . "\t";
	    }
	    if($io_type & 2) {
		print GNUPLOT_DATA_OUTPUT $results{$proc}{$transfer_buffer_size*1024}[1]{$data_type}. "\t";
	    }
	    if($io_type & 4) {
		print GNUPLOT_DATA_OUTPUT $results{$proc}{$transfer_buffer_size*1024}[2]{$data_type};
	    }
	    print GNUPLOT_DATA_OUTPUT "\n";

	}

	close(GNUPLOT_DATA_OUTPUT); 

	draw_plot(0);
	
	unlink(GNUPLOT_DATA_OUTPUT);

}


sub plot_default_graph2 {
	open(GNUPLOT_DATA_OUTPUT, ">gnuplot.data") or
		die "error: cannot open file gnuplot.data: $!\n";
	
	$num_procs_graph = max(sort { $a <=> $b }( keys %results )) if !$num_procs_graph;
	print "min-rpocs: " . $num_procs_graph;
	$data_type = "write-only" if !$data_type;

	#set up the plot
	print GNUPLOT_PIPE  "set term x11 2\n";
	print GNUPLOT_PIPE  "set xlabel \"Transfer Buffer Size (in bytes)\"\n";
	print GNUPLOT_PIPE  "set title \"" . $data_type . " Performance (Speed vs. Transfer Buffer Size)\"\n";
	print GNUPLOT_PIPE  "set ylabel \"Bandwdith (MB/s)\"\n";
	print GNUPLOT_PIPE  "set label 1 \"Procs: " . $num_procs_graph . "\" at graph 0.7, graph 0.7 left \n";

#the next line attempts to hack gnuplot to get around it's inability to linearly scale, but logarithmically label an axis
	print GNUPLOT_PIPE  "set xtics (\"4K\" 4*1024, \"8K\" 8*1024, \"16K\" 16*1024, \"32K\" 32*1024, \"64K\" 64*1024, \"128K\" 128*1024, \"256K\" 256*1024, \"512K\" 512*1024, \"1M\" 1024*1024, \"2M\" 2048*1024, \"4M\" 4096*1024, \"8M\" 8192*1024, \"16M\" 16384*1024)\n";

	foreach $xfer (sort {$a <=> $b} ( keys %{$results{$num_procs_graph}} )) 
	{
	    print GNUPLOT_DATA_OUTPUT $xfer . "\t";
	    if($io_type & 1) {
		print GNUPLOT_DATA_OUTPUT $results{$num_procs_graph}{$xfer}[0]{$data_type} . "\t";
	    }
	    if($io_type & 2) {
		print GNUPLOT_DATA_OUTPUT $results{$num_procs_graph}{$xfer}[1]{$data_type}. "\t";		
	    }
	    if($io_type & 4) {
		print GNUPLOT_DATA_OUTPUT $results{$num_procs_graph}{$xfer}[2]{$data_type};
	    }
	    print GNUPLOT_DATA_OUTPUT "\n";

     	}
	
	close(GNUPLOT_DATA_OUTPUT); 

	draw_plot(0);
	
	unlink(GNUPLOT_DATA_OUTPUT);
}

sub plot_3d_graph3 {
	open(GNUPLOT_DATA_OUTPUT, ">gnuplot.data") or
		die "error: cannot open file gnuplot.data: $!\n";

	#set up the plot
	print GNUPLOT_PIPE  "set term x11 3\n";
	print GNUPLOT_PIPE  "set xlabel \"Num. Processors\"\n";
	print GNUPLOT_PIPE  "set title \"Write Speed v. No. Procs v. Buffer Size\"\n";
	print GNUPLOT_PIPE  "set ylabel \"Buffer Size (bytes)\"\n";
	print GNUPLOT_PIPE  "set zlabel \"Bandwidth (in MB/s)\"\n";
	print GNUPLOT_PIPE  "set nolabel\n";
	print GNUPLOT_PIPE  "set dgrid3d 30,30\n";
	print GNUPLOT_PIPE  "set hidden3d\n";

#the next lines attempts to hack gnuplot to get around it's inability to linearly scale, but logarithmically label an axis
	print GNUPLOT_PIPE  "set xtics (\"1\" 1, \"2\" 2, \"4\" 4, \"8\" 8, \"16\" 16, \"32\" 32, \"64\" 64, \"128\" 128, \"256\" 256, \"512\" 512, \"1024\" 1024)\n";
	print GNUPLOT_PIPE  "set ytics (\"4K\" 4*1024, \"8K\" 8*1024, \"16K\" 16*1024, \"32K\" 32*1024, \"64K\" 64*1024, \"128K\" 128*1024, \"256K\" 256*1024, \"512K\" 512*1024, \"1M\" 1024*1024, \"2M\" 2048*1024, \"4M\" 4096*1024, \"8M\" 8192*1024, \"16M\" 16384*1024)\n";


#Read speed on z-axis, processors on x, buffer size on y.

	foreach $proc (sort { $a <=> $b }( keys %results )) 
	{
	    foreach $xfer (sort {$a <=> $b} ( keys %{$results{$proc}} )) 
	    {
		print GNUPLOT_DATA_OUTPUT $proc . "\t" . $xfer . "\t";
		if($io_type & 1) {
		    print GNUPLOT_DATA_OUTPUT $results{$proc}{$xfer}[0]{"write-only"} . "\t";		
		}
		if($io_type & 2) {
		    print GNUPLOT_DATA_OUTPUT $results{$proc}{$xfer}[1]{"write-only"}. "\t";		
		}
		if($io_type & 4) {
		    print GNUPLOT_DATA_OUTPUT $results{$proc}{$xfer}[2]{"write-only"};
		}
	    print GNUPLOT_DATA_OUTPUT "\n";

	    }
	}
	
	close(GNUPLOT_DATA_OUTPUT); 
	
	draw_plot(1);
	
	unlink(GNUPLOT_DATA_OUTPUT);
}

open(GNUPLOT_PIPE, "| tee gnuplot.script | gnuplot -persist") || die "Couldn't run gnuplot: $!\n";
GNUPLOT_PIPE->autoflush(1);

write_excel_file;
write_ascii_file;
plot_default_graph1;
sleep 1;
plot_default_graph2;
sleep 1;

plot_3d_graph3 if $plot_3d;
close(GNUPLOT_PIPE);
