#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
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

from __future__ import print_function
import argparse
import datetime
import os
import png
import subprocess
import sys
import urllib


#--------------------------------------------------------------------------------------------------
# Constants.
#--------------------------------------------------------------------------------------------------

DEFAULT_TOOL_FILEPATH = "..\\sandbox\\bin\\Release\\appleseed.cli.exe" if os.name == "nt" else \
                        "../sandbox/bin/Release/appleseed.cli"

VALUE_THRESHOLD = 0             # max absolute difference between two pixel components in [0, 255]
MAX_DIFFERING_COMPONENTS = 0    # max number of pixel components that differ significantly


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

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

def read_png_file(filepath):
    return png.Reader(filename=filepath).asRGBA8()[2]


#--------------------------------------------------------------------------------------------------
# Utility class to log progress.
#--------------------------------------------------------------------------------------------------

class Logger:
    def __init__(self, args):
        self.args = args

    def start_rendering(self, scene):
        self.scene = scene
        if self.args.verbose:
            self.__print_scene(self.scene)

    def pass_rendering(self, rendering_time):
        if self.args.verbose:
            self.__print_result(rendering_time, "passed")

    def fail_rendering(self, rendering_time, error_message):
        if not self.args.verbose:
            self.__print_scene(self.scene)
        self.__print_result(rendering_time, error_message)

    def __print_scene(self, scene):
        print("{0}: ".format(scene), end='')

    def __print_result(self, rendering_time, message):
        print("{0} [{1}]".format(format_duration(rendering_time), message))


#--------------------------------------------------------------------------------------------------
# Utility class to generate an HTML report.
#--------------------------------------------------------------------------------------------------

class ReportWriter:
    def open(self, args, filepath):
        self.args = args
        self.file = open(filepath, 'w')
        self.__write_header(args)

    def close(self):
        self.__write_footer()
        self.file.close()

    def report_failure(self, scene, reference_filepath, output_filepath, max_diff=None, diff_comps=None, total_comps=None):
        self.file.write("""            <div class="result">
                <table>
                    <tr>
                        <td class="title" colspan="2">{0}</td>
                    </tr>
                    <tr>
                        <td class="reference">
                            <a href="{1}">
                                <img src="{1}" alt="Reference Image" title="Reference Image">
                            </a>
                        </td>
                        <td class="output">
                            <a href="{2}">
                                <img src="{2}" alt="Output Image" title="Output Image">
                            </a>
                        </td>
                    </tr>
""".format(scene,
           urllib.quote(reference_filepath),
           urllib.quote(output_filepath)))

        if max_diff is not None and diff_comps is not None:
            diff_percents = 100.0 * diff_comps / total_comps
            self.file.write("""                    <tr>
                        <td colspan="2">
                            <table class="details">
                                <tr>
                                    <td>Maximum Absolute Component Difference</td>
                                    <td>{0}</td>
                                </tr>
                                <tr>
                                    <td>Number of Differing Components</td>
                                    <td>{1} ({2:.2f} %)</td>
                                </tr>
                            </table>
                        </td>
                    </tr>
""".format(max_diff, diff_comps, diff_percents))

        self.file.write("""                </table>
            </div>
""")

        self.file.flush()

    def __write_header(self, args):
        self.file.write("""<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8">
        <title>appleseed Test Suite Report</title>
        <style type="text/css">
            table.details
            {{
                border-collapse: collapse;
            }}

            table.details td
            {{
                padding: 5px;
            }}

            table.details,
            table.details td
            {{
                border: 1px solid #ddd;
            }}

            .result
            {{
                margin-bottom: 40px;
            }}

            .result .title
            {{
                font-family: "Courier New", Courier, monospace;
                font-size: 18px;
                font-weight: bold;
                padding-bottom: 15px;
            }}

            .result .reference img,
            .result .output img
            {{
                max-width: 500px;
            }}
        </style>
    </head>
    <body>
        <div>
            <h1>Information</h1>
            <table class="details">
                <tr>
                    <td>Test Date</td>
                    <td>{0}</td>
                </tr>
                <tr>
                    <td>appleseed Binary</td>
                    <td>{1}</td>
                </tr>
                <tr>
                    <td>Maximum Absolute Component Difference</td>
                    <td>{2}</td>
                </tr>
                <tr>
                    <td>Maximum Number of Differing Components</td>
                    <td>{3}</td>
                </tr>
            </table>
        </div>
        <div>
            <h1>Results</h1>
""".format(datetime.datetime.now(),
           args.tool_path,
           VALUE_THRESHOLD,
           MAX_DIFFERING_COMPONENTS))
        self.file.flush()
        
    def __write_footer(self):
        self.file.write("""        </div>
    </body>
</html>
""")


#--------------------------------------------------------------------------------------------------
# Render a given project file.
#--------------------------------------------------------------------------------------------------

def render_project_file(args, project_filepath, output_filepath, log_filepath):
    with open(log_filepath, "w", 0) as log_file:
        command = '"{0}" -o "{1}" "{2}"'.format(args.tool_path, output_filepath, project_filepath)
        if args.args:
            command += ' {0}'.format(" ".join(args.args))

        log_file.write("Command line:\n    {0}\n\n".format(command))

        start_time = datetime.datetime.now()
        result = subprocess.call(command, stderr=log_file, shell=True)
        end_time = datetime.datetime.now()

        return result == 0, end_time - start_time


#--------------------------------------------------------------------------------------------------
# Compare two PNG images.
# Returns a triplet max_diff, diff_comps, total_comps where:
#   max_diff is the maximum absolute difference between two components
#   diff_comps is the number of components whose absolute difference is larger than value_threshold
#   total_comps is the total number of components in the image
#--------------------------------------------------------------------------------------------------

def compare_png_images(filepath1, filepath2, value_threshold):
    max_diff = 0
    diff_comps = 0
    total_comps = 0

    image1 = read_png_file(filepath1)
    image2 = read_png_file(filepath2)

    for row1, row2 in zip(image1, image2):
        for val1, val2 in zip(row1, row2):
            diff = abs(val1 - val2)
            max_diff = max(max_diff, diff)

            if diff > value_threshold:
                diff_comps += 1

            total_comps += 1

    return max_diff, diff_comps, total_comps


#--------------------------------------------------------------------------------------------------
# Render a given test scene.
# Return True if the test scene passed, False if it failed to render, or if the output does not
# match the reference image.
#--------------------------------------------------------------------------------------------------

def render_test_scene(args, logger, report_writer, project_directory, project_filename):
    project_basename = os.path.splitext(project_filename)[0]
    project_filepath = os.path.join(project_directory, project_filename)

    output_directory = os.path.join(project_directory, 'renders')
    ref_directory = os.path.join(project_directory, 'ref')

    output_filename = project_basename + '.png'
    output_filepath = os.path.join(output_directory, output_filename)
    ref_filepath = os.path.join(ref_directory, output_filename)

    log_filename = project_basename + '.txt'
    log_filepath = os.path.join(output_directory, log_filename)

    safe_mkdir(output_directory)

    logger.start_rendering(project_filepath)

    rendering_success, rendering_time = render_project_file(args,
                                                            project_filepath,
                                                            output_filepath,
                                                            log_filepath)

    if not rendering_success:
        logger.fail_rendering(rendering_time, "FAILED")
        report_writer.report_failure(project_filepath, ref_filepath, output_filepath)
        return False

    if not os.path.exists(output_filepath):
        if not os.path.exists(ref_filepath):
            logger.pass_rendering(rendering_time)
            return True
        else:
            logger.fail_rendering(rendering_time, "MISSING OUTPUT")
            report_writer.report_failure(project_filepath, ref_filepath, output_filepath)
            return False
    else:
        if not os.path.exists(ref_filepath):
            logger.fail_rendering(rendering_time, "MISSING REFERENCE")
            report_writer.report_failure(project_filepath, ref_filepath, output_filepath)
            return False

    max_diff, diff_comps, total_comps = compare_png_images(output_filepath, ref_filepath, VALUE_THRESHOLD)

    if diff_comps > MAX_DIFFERING_COMPONENTS:
        logger.fail_rendering(rendering_time, "OUTPUT/REFERENCE MISMATCH")
        report_writer.report_failure(project_filepath, ref_filepath, output_filepath, max_diff, diff_comps, total_comps)
        return False

    logger.pass_rendering(rendering_time)
    return True


#--------------------------------------------------------------------------------------------------
# Render all test scenes in a given directory (possibly recursively).
# Returns the number of rendered and passing test scenes.
#--------------------------------------------------------------------------------------------------

def render_test_scenes(args):
    rendered_scene_count = 0
    passing_scene_count = 0

    logger = Logger(args)

    report_writer = ReportWriter()
    report_writer.open(args, "report.html")

    for dirpath, dirnames, filenames in walk(args.directory, args.recursive):
        if should_skip(os.path.basename(dirpath)):
            if args.verbose:
                print("skipping:  {0}...".format(dirpath))
            continue

        for filename in filenames:
            if os.path.splitext(filename)[1] == '.appleseed':
                if should_skip(filename):
                    if args.verbose:
                        print("skipping:  {0}...".format(os.path.join(dirpath, filename)))
                    continue

                rendered_scene_count += 1

                if render_test_scene(args, logger, report_writer, dirpath, filename):
                    passing_scene_count += 1

    report_writer.close()

    return rendered_scene_count, passing_scene_count


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="run the functional test suite.")
    parser.add_argument("-t", "--tool-path", metavar="tool-path",
                        help="set the path to the appleseed.cli tool")
    parser.add_argument("-r", "--recursive", action='store_true', dest="recursive",
                        help="scan the specified directory and all its subdirectories")
    parser.add_argument("-a", "--verbose", action='store_true', dest="verbose",
                        help="show skipped and passing test scenes")
    parser.add_argument("-p", "--parameter", dest="args", metavar="ARG", nargs="*",
                        help="forward additional arguments to appleseed")
    parser.add_argument("directory", nargs='?', default=".", help="directory to scan")
    args = parser.parse_args()

    if args.tool_path is None:
        script_directory = os.path.dirname(os.path.realpath(__file__))
        args.tool_path = os.path.join(script_directory, DEFAULT_TOOL_FILEPATH)

    print("running test suite using {0}".format(args.tool_path))

    start_time = datetime.datetime.now()
    rendered_scene_count, passing_scene_count = render_test_scenes(args)
    end_time = datetime.datetime.now()

    success = 100.0 * passing_scene_count / rendered_scene_count if rendered_scene_count > 0 else 0.0

    print("{0} out of {1} test scene(s) passed ({2:.2f} %), total rendering time {3}." \
        .format(passing_scene_count,
                rendered_scene_count,
                success,
                format_duration(end_time - start_time)))

if __name__ == '__main__':
    main()
