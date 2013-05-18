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
import subprocess
import os
import sys


#--------------------------------------------------------------------------------------------------
# Update a given project file.
#--------------------------------------------------------------------------------------------------

def update_project_file(filepath, tool_path):
    subprocess.call([tool_path, filepath])


#--------------------------------------------------------------------------------------------------
# Update all project files in the current directory.
# Returns the number of updated project files.
#--------------------------------------------------------------------------------------------------

def update_project_files(tool_path):
    updated_file_count = 0

    for dirpath, dirnames, filenames in os.walk("."):
        for filename in filenames:
            if os.path.splitext(filename)[1] == ".appleseed":
                update_project_file(os.path.join(dirpath, filename), tool_path)
                updated_file_count += 1

    return updated_file_count


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    if len(sys.argv) < 2:
        print("Usage: {0} <path-to-updateprojectfile-tool>".format(sys.argv[0]))
        sys.exit(1)

    tool_path = sys.argv[1]

    start_time = datetime.datetime.now()
    updated_file_count = update_project_files(tool_path)
    end_time = datetime.datetime.now()

    print("\nUpdated {0} project files in {1}.".format(updated_file_count, end_time - start_time))

if __name__ == '__main__':
    main()
