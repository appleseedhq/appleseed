#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
# Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#

import argparse
import datetime
import fnmatch
import os
import subprocess

from utils import print_runtime_details  # local module


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "1.0"

DEFAULT_TOOL_FILENAME = "convertmeshfile.exe" if os.name == "nt" else "convertmeshfile"


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

def walk(directory, recursive):
    if recursive:
        for dirpath, dirnames, filenames in os.walk(directory):
            for filename in filenames:
                yield os.path.join(dirpath, filename)
    else:
        dirpath, dirnames, filenames = os.walk(directory).next()
        for filename in filenames:
            yield os.path.join(dirpath, filename)


# -------------------------------------------------------------------------------------------------
# Convert a given mesh file.
# -------------------------------------------------------------------------------------------------

def convert_mesh_file(input_filepath, output_filepath, tool_path):
    print("converting {0} to {1}...".format(input_filepath, output_filepath))
    subprocess.call([tool_path, input_filepath, output_filepath])


# -------------------------------------------------------------------------------------------------
# Convert all matching mesh files in a given directory (possibly recursively).
# Returns the number of converted mesh files.
# -------------------------------------------------------------------------------------------------

def convert_mesh_files(tool_path, directory, recursive, input_pattern, output_format, overwrite):
    converted_file_count = 0

    for filepath in walk(directory, recursive):
        filename = os.path.basename(filepath)
        if fnmatch.fnmatch(filename, input_pattern):
            output_filepath = os.path.splitext(filepath)[0] + "." + output_format
            if overwrite or not os.path.exists(output_filepath):
                convert_mesh_file(filepath, output_filepath, tool_path)
                converted_file_count += 1

    return converted_file_count


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="convert multiple mesh files from one format "
                                     "to another.")
    parser.add_argument("-t", "--tool-path", metavar="tool-path",
                        help="set the path to the convertmeshfile tool")
    parser.add_argument("-r", "--recursive", action='store_true', dest='recursive',
                        help="scan the specified directory and all its subdirectories")
    parser.add_argument("-n", "--no-overwrite", action='store_false', dest='overwrite',
                        help="do not overwrite destination files if they already exist.")
    parser.add_argument("directory", help="directory to scan")
    parser.add_argument("input_pattern", metavar="input-pattern", help="input files pattern (e.g. *.obj)")
    parser.add_argument("output_format", metavar="output-format", help="output file format (e.g. abc, binarymesh)")
    args = parser.parse_args()

    print_runtime_details("convertmany", VERSION, os.path.realpath(__file__))

    # If no tool path is provided, search for the tool in the same directory as this script.
    if args.tool_path is None:
        script_directory = os.path.dirname(os.path.realpath(__file__))
        args.tool_path = os.path.join(script_directory, DEFAULT_TOOL_FILENAME)
        print("setting tool path to {0}.".format(args.tool_path))

    start_time = datetime.datetime.now()
    converted_file_count = convert_mesh_files(args.tool_path, args.directory, args.recursive,
                                              args.input_pattern, args.output_format,
                                              args.overwrite)
    end_time = datetime.datetime.now()

    print("converted {0} mesh file(s) in {1}.".format(converted_file_count, end_time - start_time))


if __name__ == "__main__":
    main()
