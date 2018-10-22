#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2018 Jonathan Dent, The appleseedhq Organization
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
import fileinput


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


def process_file(filepath):
    print("processing {0}...".format(filepath))

    header_name = os.path.basename(filepath).upper().replace(".", "_")

    for line in fileinput.input((filepath), inplace=True):
        line = line.rstrip()
        if "#ifndef" in line and header_name in line:
            print("#pragma once")
        elif "#define" in line and header_name in line:
            continue
        elif "#endif" in line and header_name in line:
            break
        else:
            print(line)

    fileinput.close()


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="sort #include clauses in c++ source code.")
    parser.add_argument("-r", "--recursive", action='store_true', dest='recursive',
                        help="process all files in the specified directory and all its subdirectories")
    parser.add_argument("path", help="file or directory to process")
    args = parser.parse_args()

    if os.path.isfile(args.path):
        process_file(args.path)
    else:
        for filepath in walk(args.path, args.recursive):
            ext = os.path.splitext(filepath)[1]
            if ext == ".h":
                process_file(filepath)

if __name__ == '__main__':
    main()
