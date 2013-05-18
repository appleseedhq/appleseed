#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

import datetime
import fnmatch
import os
import subprocess
import sys


#--------------------------------------------------------------------------------------------------
# Convert a given mesh file.
#--------------------------------------------------------------------------------------------------

def convert_mesh_file(input_filepath, output_filepath, tool_path):
    print("Converting {0} to {1}...".format(input_filepath, output_filepath))
    subprocess.call([tool_path, input_filepath, output_filepath])


#--------------------------------------------------------------------------------------------------
# Convert all matching mesh files in the current directory.
# Returns the number of converted mesh files.
#--------------------------------------------------------------------------------------------------

def convert_mesh_files(tool_path, input_pattern, output_format):
    converted_file_count = 0

    for dirpath, dirnames, filenames in os.walk("."):
        for filename in filenames:
            if fnmatch.fnmatch(filename, input_pattern):
                input_filepath = os.path.join(dirpath, filename)
                output_filepath = os.path.splitext(input_filepath)[0] + "." + output_format
                convert_mesh_file(input_filepath, output_filepath, tool_path)
                converted_file_count += 1

    return converted_file_count


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    if len(sys.argv) < 4:
        print("Usage: {0} <path-to-convertmesh-tool> <input-pattern> <output-format>".format(sys.argv[0]))
        sys.exit(1)

    tool_path = sys.argv[1]
    input_pattern = sys.argv[2]
    output_format = sys.argv[3]

    start_time = datetime.datetime.now()
    converted_file_count = convert_mesh_files(tool_path, input_pattern, output_format)
    end_time = datetime.datetime.now()

    print("\nConverted {0} mesh files in {1}.".format(converted_file_count, end_time - start_time))

if __name__ == '__main__':
    main()
