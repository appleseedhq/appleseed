#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
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

from __future__ import print_function
import argparse
import os
import shutil

from utils import print_runtime_details  # local module


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "1.0"


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

def safe_mkdir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)


def walk(directory, recursive):
    if recursive:
        for dirpath, dirnames, filenames in os.walk(directory):
            yield dirpath, dirnames, filenames
    else:
        yield os.walk(directory).next()


# -------------------------------------------------------------------------------------------------
# Update reference images in a given test suite directory.
# -------------------------------------------------------------------------------------------------

def update_ref_images(parent_dir):
    renders_dir = os.path.join(parent_dir, "renders")
    ref_dir = os.path.join(parent_dir, "ref")

    safe_mkdir(ref_dir)

    for filename in os.listdir(renders_dir):
        if os.path.splitext(filename)[1] == ".png":
            src_path = os.path.join(renders_dir, filename)
            dst_path = os.path.join(ref_dir, filename)
            print("  copying {0} to {1}...".format(src_path, dst_path))
            shutil.copyfile(src_path, dst_path)


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="update functional test suite reference images.")
    parser.add_argument("-r", "--recursive", action='store_true', dest="recursive",
                        help="scan the specified directory and all its subdirectories")
    parser.add_argument("directory", nargs='?', default=".", help="directory to scan")
    args = parser.parse_args()

    print_runtime_details("updatetestsuiterefimages", VERSION, os.path.realpath(__file__))

    for dirpath, dirnames, filenames in walk(args.directory, args.recursive):
        if "renders" in dirnames:
            update_ref_images(dirpath)


if __name__ == "__main__":
    main()
