#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
# Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
import os
import subprocess


#--------------------------------------------------------------------------------------------------
# Constants.
#--------------------------------------------------------------------------------------------------

DEFAULT_TOOL_FILENAME = "projecttool.exe" if os.name == "nt" else "projecttool"


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

def walk(directory, recursive):
    if recursive:
        for dirpath, dirnames, filenames in os.walk(directory):
            for filename in filenames:
                yield os.path.join(dirpath, filename)
    else:
        dirpath, dirnames, filenames = os.walk(directory).next()
        for filename in filenames:
            yield os.path.join(dirpath, filename)


#--------------------------------------------------------------------------------------------------
# Update a given project file.
#--------------------------------------------------------------------------------------------------

def update_project_file(filepath, tool_path):
    print("updating {0}...".format(filepath))
    subprocess.call([tool_path, "update", filepath])


#--------------------------------------------------------------------------------------------------
# Update all project files in a given directory (possibly recursively).
# Returns the number of updated project files.
#--------------------------------------------------------------------------------------------------

def update_project_files(tool_path, directory, recursive):
    updated_file_count = 0

    for filepath in walk(directory, recursive):
        if os.path.splitext(filepath)[1] == ".appleseed":
            update_project_file(filepath, tool_path)
            updated_file_count += 1

    return updated_file_count


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="normalize multiple project files and update "
                                     "them to the latest format revision if necessary.")
    parser.add_argument("-t", "--tool-path", metavar="tool-path",
                        help="set the path to the projecttool binary")
    parser.add_argument("-r", "--recursive", action='store_true', dest="recursive",
                        help="scan the specified directory and all its subdirectories")
    parser.add_argument("directory", help="directory to scan")
    args = parser.parse_args()

    # If no tool path is provided, search for the tool in the same directory as this script.
    if args.tool_path is None:
        script_directory = os.path.dirname(os.path.realpath(__file__))
        args.tool_path = os.path.join(script_directory, DEFAULT_TOOL_FILENAME)
        print("setting tool path to {0}.".format(args.tool_path))

    start_time = datetime.datetime.now()
    updated_file_count = update_project_files(args.tool_path, args.directory, args.recursive)
    end_time = datetime.datetime.now()

    print("updated {0} project file(s) in {1}.".format(updated_file_count, end_time - start_time))

if __name__ == '__main__':
    main()
