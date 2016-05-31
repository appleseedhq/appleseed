
#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2015-2016 Esteban Tovagliari, The appleseedhq Organization
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
import os
import sys

if len(sys.argv) != 2:
    print("Usage: {0} [path-to-oslc]".format(sys.argv[0]))
    sys.exit(0)

oslc_cmd = sys.argv[1]
include_dir = os.path.join(os.path.abspath(os.path.dirname(__file__)), "include")

for dirpath, dirnames, filenames in os.walk("."):
    for filename in filenames:
        if filename.endswith(".osl"):
            src_filepath = os.path.join(dirpath, filename)

            dest_dir = os.path.join("..", dirpath)
            dst_filename = filename.replace(".osl", ".oso")
            dst_filepath = os.path.join(dest_dir, dst_filename)

            if not os.path.exists(dest_dir):
                os.makedirs(dest_dir)

            retcode = os.system("{0} -v -I{1} -o {2} {3}".format(oslc_cmd, include_dir, dst_filepath, src_filepath))

            if retcode != 0:
                print("Compilation of {0} failed with error code {1}. Stopping.".format(src_filepath, retcode))
                sys.exit(retcode)
