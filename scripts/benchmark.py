#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2012 Francois Beaune
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

import os
import re
import subprocess
import sys

def run(project_path, appleseed_path, appleseed_args):
    project_name = os.path.splitext(os.path.split(project_path)[1])[0]
    
    command_line = [appleseed_path, project_path] + appleseed_args
    command_line += [ "-o", project_name + ".png" ]

    print("Running {0}".format(" ".join(command_line)))

    output = subprocess.check_output(command_line, stderr=subprocess.STDOUT)

    if was_successful(output):
        process_output(output)
    else:
        print(output)

def was_successful(output):
    return get_value(output, "result") == "success"

def process_output(output):
    setup_time = float(get_value(output, "setup_time"))
    render_time = float(get_value(output, "render_time"))
    total_time = float(get_value(output, "total_time"))

    print("  Setup Time: {0} seconds".format(setup_time))
    print("  Render Time: {0} seconds".format(render_time))
    print("  Total Time: {0} seconds".format(total_time))
    print

def get_value(output, key):
    pattern = r"^{0}=(.*)[\r\n]+$".format(key)
    match = re.search(pattern, output, re.MULTILINE)
    return match.group(1) if match else None

def main():
    if len(sys.argv) < 2:
        print("Usage: {0} <path-to-appleseed.cli> [arguments]".format(sys.argv[0]))
        sys.exit(1)

    appleseed_path = sys.argv[1]
    appleseed_args = sys.argv[2:]

    for dirpath, dirnames, filenames in os.walk("."):
        if dirpath.endswith(".skip"):
            print("Skipping {0}...\n".format(dirpath))
            continue

        for filename in filenames:
            if os.path.splitext(filename)[1] == ".appleseed":
                run(os.path.join(dirpath, filename), appleseed_path, appleseed_args)

main()
