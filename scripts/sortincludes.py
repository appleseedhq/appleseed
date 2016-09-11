#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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
import os
import sys


#--------------------------------------------------------------------------------------------------
# Processing code.
#--------------------------------------------------------------------------------------------------

def process_file(filepath):
    print("processing {0}...".format(filepath))

    with open(filepath) as f:
        lines = f.readlines()

    section_begin = -1

    for index in range(len(lines)):
        line = lines[index]

        if section_begin == -1 and line.startswith("#include"):
            section_begin = index

        if section_begin != -1 and line in ["\n", "\r\n"]:
            if all(clause.startswith("#include") for clause in lines[section_begin:index]):
                lines[section_begin:index] = sorted(lines[section_begin:index], key=lambda s: s.lower())
            section_begin = -1

    with open(filepath + ".processed", "wt") as f:
        for line in lines:
            f.write(line)

    os.remove(filepath)
    os.rename(filepath + ".processed", filepath)
    
def process_recursively():
    for dirpath, dirnames, filenames in os.walk("."):
        for filename in filenames:
            ext = os.path.splitext(filename)[1]
            if ext == ".h" or ext == ".cpp":
                process_file(os.path.join(dirpath, filename))


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="sort #include clauses in c++ source code.")
    parser.add_argument("-r", "--recursive", action='store_true', dest='recursive',
                        help="process all files in the specified directory and all its subdirectories")
    parser.add_argument("filepath", nargs="?", help="file to process")
    args = parser.parse_args()

    if args.filepath:
        if args.recursive:
            print("cannot simultaneously specify a file path and use the --recursive flag.")
            sys.exit(1)
        print(args.filepath)
    else:
        if not args.recursive:
            print("either a file path or the --recursive flag must be specified.")
            sys.exit(1)
        process_recursively()

if __name__ == '__main__':
    main()
