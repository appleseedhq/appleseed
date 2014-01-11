#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
# Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

import os.path

def make_relative_path(file_path, base):
    npath = os.path.normpath(os.path.normcase(file_path))
    nbase = os.path.normpath(os.path.normcase(base))
    if npath.startswith(nbase):
        result = npath[len(nbase):]
        if result.startswith("/") or result.startswith("\\"):
            result = result[1:]
        return result
    else:
        return file_path

def expect(expected, received):
    if expected != received:
        print("Unit test failed!")
        print("  Expected: {0}".format(expected))
        print("  Received: {0}".format(received))

def run_tests():
    expect("c:\\dir\\file.ext", make_relative_path("c:\\dir\\file.ext", ""))
    expect("/dir/file.ext", make_relative_path("/dir/file.ext", ""))
    expect("dir\\file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\"))
    expect("dir\\file.ext", make_relative_path("/dir/file.ext", "/"))
    expect("dir\\file.ext", make_relative_path("c:\\dir\\file.ext", "c:/"))
    expect("dir\\file.ext", make_relative_path("/dir/file.ext", "\\"))
    expect("file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\dir"))
    expect("file.ext", make_relative_path("/dir/file.ext", "/dir"))
    expect("file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\dir\\"))
    expect("file.ext", make_relative_path("/dir/file.ext", "/dir/"))
    expect("c:\\dir\\file.ext", make_relative_path("c:\\dir\\file.ext", "c:\\rep"))
    expect("/dir/file.ext", make_relative_path("/dir/file.ext", "/rep"))

run_tests()
