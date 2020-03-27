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
import re
import subprocess
import sys

from utils import print_runtime_details  # local module


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "1.0"


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

def safe_make_directory(path):
    if not os.path.isdir(path):
        os.makedirs(path)


# -------------------------------------------------------------------------------------------------
# Logger.
# -------------------------------------------------------------------------------------------------

class Logger:

    def __init__(self, directory):
        now = datetime.datetime.now()
        self.filename = now.strftime("benchmark.%Y%m%d.%H%M%S.txt")
        self.filepath = os.path.join(directory, self.filename)
        self.file = open(self.filepath, "w", 0)     # 0: no buffering

    def get_log_file_path(self):
        return self.filepath

    def write(self, s=""):
        self.file.write(s + "\n")
        print(s)


# -------------------------------------------------------------------------------------------------
# Benchmarking and reporting code.
# -------------------------------------------------------------------------------------------------

def benchmark_projects(appleseed_path, appleseed_args, logger):
    for dirpath, dirnames, filenames in os.walk("."):
        if dirpath.endswith(".skip"):
            continue

        for filename in filenames:
            if os.path.splitext(filename)[1] == ".appleseed":
                benchmark_project(os.path.join(dirpath, filename), appleseed_path, appleseed_args, logger)


def benchmark_project(project_path, appleseed_path, appleseed_args, logger):
    project_name = os.path.splitext(os.path.split(project_path)[1])[0]

    logger.write("Benchmarking {0} scene...".format(project_name))

    command_line = [appleseed_path, project_path] + appleseed_args
    command_line += ["--benchmark-mode"]
    command_line += ["-o", os.path.join("renders", project_name + ".png")]

    output = subprocess.check_output(command_line, stderr=subprocess.STDOUT)

    if was_successful(output):
        process_output(output, logger)
    else:
        logger.write(output)


def was_successful(output):
    return get_value(output, "result") == "success"


def process_output(output, logger):
    setup_time = float(get_value(output, "setup_time"))
    render_time = float(get_value(output, "render_time"))
    total_time = float(get_value(output, "total_time"))

    logger.write("  Setup Time  : {0} seconds".format(setup_time))
    logger.write("  Render Time : {0} seconds".format(render_time))
    logger.write("  Total Time  : {0} seconds".format(total_time))
    logger.write()


def get_value(output, key):
    pattern = r"^{0}=(.*)[\r\n]+$".format(key)
    match = re.search(pattern, output, re.MULTILINE)
    return match.group(1) if match else None


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def print_configuration(appleseed_path, appleseed_args, logger):
    logger.write("Configuration:")
    logger.write("  Log file               : {0}".format(logger.get_log_file_path()))
    logger.write("  Path to appleseed      : {0}".format(appleseed_path))
    logger.write("  appleseed command line : {0}".format(" ".join(appleseed_args)))
    logger.write()


def main():
    parser = argparse.ArgumentParser(description="benchmark appleseed")

    parser.add_argument("appleseed_path", help="set the path to the appleseed.cli tool")
    parser.add_argument("appleseed_args", nargs=argparse.REMAINDER,
                        help="forward additional arguments to appleseed")

    args = parser.parse_args()
    appleseed_path = args.appleseed_path
    appleseed_args = args.appleseed_args

    safe_make_directory("logs")
    logger = Logger("logs")

    print_runtime_details("appleseed.benchmark", VERSION, os.path.realpath(__file__), print_function=logger.write)
    print_configuration(appleseed_path, appleseed_args, logger)

    safe_make_directory("renders")

    start_time = datetime.datetime.now()
    benchmark_projects(appleseed_path, appleseed_args, logger)
    elapsed_time = datetime.datetime.now() - start_time

    logger.write("\nTotal suite time: {0}\n".format(elapsed_time))


if __name__ == "__main__":
    main()
