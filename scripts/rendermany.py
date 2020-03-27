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

from __future__ import print_function
import argparse
import datetime
import os
import subprocess
import sys

from utils import print_runtime_details  # local module


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "1.0"

DEFAULT_TOOL_FILENAME = "appleseed.cli.exe" if os.name == "nt" else "appleseed.cli"


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


def should_skip(path):
    return path.startswith("skip - ")


def format_duration(duration):
    total_seconds = duration.total_seconds()
    hours = int(total_seconds / 3600)
    minutes = int((total_seconds % 3600) / 60)
    seconds = total_seconds % 60
    return "{0:02}:{1:02}:{2:09.6f}".format(hours, minutes, seconds)


# -------------------------------------------------------------------------------------------------
# Render a given project file.
# -------------------------------------------------------------------------------------------------

def render_project_file(args, project_directory, project_filename):
    project_filepath = os.path.join(project_directory, project_filename)

    output_directory = os.path.join(project_directory, 'renders')
    safe_mkdir(output_directory)

    output_filename = os.path.splitext(project_filename)[0] + '.' + args.output_format
    output_filepath = os.path.join(output_directory, output_filename)

    log_filename = os.path.splitext(project_filename)[0] + '.txt'
    log_filepath = os.path.join(output_directory, log_filename)

    with open(log_filepath, "w", 0) as log_file:
        print("rendering: {0}: ".format(project_filepath), end='')

        command = '"{0}" -o "{1}" "{2}"'.format(args.tool_path, output_filepath, project_filepath)
        if args.args:
            command += ' {0}'.format(" ".join(args.args))

        log_file.write("Command line:\n    {0}\n\n".format(command))

        start_time = datetime.datetime.now()
        result = subprocess.call(command, stderr=log_file, shell=True)
        end_time = datetime.datetime.now()

        if result == 0:
            print("{0} [ok]".format(format_duration(end_time - start_time)))
        else:
            print("[failed]")


# -------------------------------------------------------------------------------------------------
# Render all project files in a given directory (possibly recursively).
# Returns the number of rendered project files.
# -------------------------------------------------------------------------------------------------

def render_project_files(args):
    rendered_file_count = 0

    for dirpath, dirnames, filenames in walk(args.directory, args.recursive):
        if should_skip(os.path.basename(dirpath)):
            print("skipping:  {0}...".format(dirpath))
            continue

        for filename in filenames:
            if os.path.splitext(filename)[1] == '.appleseed':
                if should_skip(filename):
                    print("skipping:  {0}...".format(os.path.join(dirpath, filename)))
                    continue

                render_project_file(args, dirpath, filename)
                rendered_file_count += 1

    return rendered_file_count


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="render multiple project files.")
    parser.add_argument("-t", "--tool-path", metavar="tool-path",
                        help="set the path to the appleseed.cli tool")
    parser.add_argument("-f", "--format", dest="output_format", metavar="FORMAT", default="exr",
                        help="set output format (e.g. png, exr)")
    parser.add_argument("-r", "--recursive", action='store_true', dest="recursive",
                        help="scan the specified directory and all its subdirectories")
    parser.add_argument("-p", "--parameter", dest="args", metavar="ARG", nargs="*",
                        help="forward additional arguments to appleseed")
    parser.add_argument("directory", help="directory to scan")
    args = parser.parse_args()

    print_runtime_details("rendermany", VERSION, os.path.realpath(__file__))

    # If no tool path is provided, search for the tool in the same directory as this script.
    if args.tool_path is None:
        script_directory = os.path.dirname(os.path.realpath(__file__))
        args.tool_path = os.path.join(script_directory, DEFAULT_TOOL_FILENAME)
        print("setting tool path to {0}.".format(args.tool_path))

    start_time = datetime.datetime.now()
    rendered_file_count = render_project_files(args)
    end_time = datetime.datetime.now()

    print("rendered {0} project file(s) in {1}."
          .format(rendered_file_count, format_duration(end_time - start_time)))


if __name__ == "__main__":
    main()
