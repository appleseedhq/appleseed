#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

from __future__ import division
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

VALUE_THRESHOLD = 2                 # max allowed absolute diff between two pixel components, in [0, 255]
MAX_DIFFERING_COMPONENTS = 4 * 2    # max number of pixel components that are allowed to differ significantly


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

def safe_mkdir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)

def safe_remove(filepath):
    try:
        os.remove(filepath)
    except OSError:
        pass

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
    data = png.Reader(filename=filepath).asRGBA8()
    width = data[0]
    height = data[1]
    rows = data[2]
    return width, height, rows

def write_rgba_png_file(filepath, rows):
    width = len(rows[0]) / 4
    height = len(rows)
    writer = png.Writer(width=width, height=height, alpha=True)
    with open(filepath, 'wb') as file:
        writer.write(file, rows)

    
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
        self.failures = 0
        self.all_commands = []

    def close(self):
        self.__write_footer()
        self.file.close()

    def report_failure(self, scene, reference_filepath, output_filepath, log_filepath, error_message, num_diff=None, max_diff=None, num_comps=None, diff_filepath=None):
        self.failures += 1
        self.file.write("""            <div class="result">
                <table>
                    <tr>
                        <td class="title" colspan="3">{0}</td>
                    </tr>
                    <tr>
                        <td class="reference">
                            <a href="{1}">
                                <img src="{1}" alt="Reference Image" title="Reference Image">
                            </a>
                        </td>
                        <td class="output">
                            <a href="{2}">
                                <img src="{2}" alt="Difference Image" title="Difference Image">
                            </a>
                        </td>
                        <td class="diff">
                            <a href="{3}">
                                <img src="{3}" alt="Output Image" title="Output Image">
                            </a>
                        </td>
                    </tr>
                    <tr>
                        <td colspan="3">
                            <table class="details">
                                <tr>
                                    <td>Reason</td>
                                    <td>{4}</td>
                                </tr>
                                <tr>
                                    <td>Log File</td>
                                    <td><a href="{5}">{6}</a></td>
                                </tr>
""".format(scene,
           urllib.quote(reference_filepath),
           urllib.quote(diff_filepath) if diff_filepath is not None else "",
           urllib.quote(output_filepath),
           error_message,
           urllib.quote(log_filepath),
           os.path.basename(log_filepath)))

        if max_diff is not None and num_diff is not None:
            diff_percents = 100.0 * num_diff / num_comps
            command = '{0} "{1}" "{2}"'.format("copy" if os.name == 'nt' else "cp", output_filepath, reference_filepath)
            self.all_commands.append(command)
            self.file.write("""                                <tr>
                                    <td>Maximum Absolute Component Difference (as an 8-bit integer)</td>
                                    <td>{0}</td>
                                </tr>
                                <tr>
                                    <td>Number of Differing Components</td>
                                    <td>{1} ({2:.2f} %)</td>
                                </tr>
                                <tr>
                                    <td>Update Reference Image</td>
                                    <td class="command">{3}</td>
                                </tr>
""".format(max_diff,
           num_diff,
           diff_percents,
           command))

        self.file.write("""                            </table>
                        </td>
                    </tr>
""")

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
                margin-bottom: 100px;
            }}

            .result .title
            {{
                font-family: "Courier New", Courier, monospace;
                font-size: 18px;
                font-weight: bold;
                padding-bottom: 15px;
            }}

            .result .reference,
            .result .output,
            .result .diff
            {{
                width: 510px;
            }}

            .result img
            {{
                max-width: 500px;
                border: 1px solid #ddd;
            }}

            ul.commands
            {{
                padding: 0;
                list-style-type: none;
            }}

            .command
            {{
                font-family: "Courier New", Courier, monospace;
                font-size: 15px;
                background-color: #eee;
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
            <h1>Failures</h1>
""".format(datetime.datetime.now(),
           args.tool_path,
           VALUE_THRESHOLD,
           MAX_DIFFERING_COMPONENTS))
        self.file.flush()

    def __write_footer(self):
        if self.failures == 0:
            self.file.write("            <p>None.</p>")
        self.file.write("""        </div>
        <div>
            <h1>All Update Commands</h1>
            <div>
                <ul class="commands">
""")
        for command in self.all_commands:
            self.file.write("""                <li class="command">{0}</li>""".format(command))
        self.file.write("""                </ul>
            </div>
        </div>
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
# Compare two images.
# Returns a (num_diff, max_diff, diff_image) where:
#   num_diff is the number of components whose absolute difference is larger than value_threshold
#   max_diff is the maximum absolute difference between two components
#   diff_image is the difference between the two images
#--------------------------------------------------------------------------------------------------

def compare_images(rows1, rows2, value_threshold):
    num_diff = 0
    max_diff = 0
    diff_image = []

    for row1, row2 in zip(rows1, rows2):
        diff_row = [ abs(val1 - val2) for val1, val2 in zip(row1, row2) ]
        num_diff += sum(d > value_threshold for d in diff_row)
        max_diff = max(max_diff, max(diff_row))
        diff_image.append(diff_row)

    return num_diff, max_diff, diff_image

def fit(x, min_x, max_x, min_y, max_y):
    assert min_x != max_x
    k = (x - min_x) / (max_x - min_x)
    return min_y * (1 - k) + max_y * k

def transform_to_false_color(rows):
    image_min = 255
    image_max = 0

    for row in rows:
        for i in range(0, len(row) - 1, 4):
            r = row[i + 0]
            g = row[i + 1]
            b = row[i + 2]
            a = row[i + 3]

            m = max(r, g, b, a)

            image_min = min(image_min, m)
            image_max = max(image_max, m)

    assert image_min <= image_max

    for row in rows:
        for i in range(0, len(row) - 1, 4):
            r = row[i + 0]
            g = row[i + 1]
            b = row[i + 2]
            a = row[i + 3]

            m = max(r, g, b, a)
            
            if m == 0:
                row[i + 0] = 0
                row[i + 1] = 0
                row[i + 2] = 0
                row[i + 3] = 255
                continue

            fm = int(fit(m, image_min, image_max, 0, 255))

            row[i + 0] = fm
            row[i + 1] = 0
            row[i + 2] = 255 - fm
            row[i + 3] = 255


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

    if not args.skip_rendering:
        safe_remove(output_filepath)
        safe_remove(log_filepath)
        rendering_success, rendering_time = render_project_file(args,
                                                                project_filepath,
                                                                output_filepath,
                                                                log_filepath)
    else:
        rendering_success = True
        rendering_time = datetime.timedelta(0)

    if not rendering_success:
        logger.fail_rendering(rendering_time, "FAILED")
        report_writer.report_failure(project_filepath, ref_filepath, output_filepath, log_filepath, "Rendering failed")
        return False

    if not os.path.exists(output_filepath):
        if not os.path.exists(ref_filepath):
            logger.pass_rendering(rendering_time)
            return True
        else:
            logger.fail_rendering(rendering_time, "MISSING OUTPUT")
            report_writer.report_failure(project_filepath, ref_filepath, output_filepath, log_filepath, "Output image is missing")
            return False
    else:
        if not os.path.exists(ref_filepath):
            logger.fail_rendering(rendering_time, "MISSING REFERENCE")
            report_writer.report_failure(project_filepath, ref_filepath, output_filepath, log_filepath, "Reference image is missing")
            return False

    out_width, out_height, out_rows = read_png_file(output_filepath)
    ref_width, ref_height, ref_rows = read_png_file(ref_filepath)

    if out_width != ref_width or out_height != ref_height:
        logger.fail_rendering(rendering_time, "OUTPUT/REFERENCE SIZE MISMATCH")
        report_writer.report_failure(project_filepath, ref_filepath, output_filepath, log_filepath,
                                     "Output and reference images have different sizes")
        return False

    num_diff, max_diff, diff_image = compare_images(out_rows, ref_rows, VALUE_THRESHOLD)

    if num_diff > MAX_DIFFERING_COMPONENTS:
        diff_filename = project_basename + '.diff.png'
        diff_filepath = os.path.join(output_directory, diff_filename)
        transform_to_false_color(diff_image)
        write_rgba_png_file(diff_filepath, diff_image)

        num_comps = ref_width * ref_height * 4
        logger.fail_rendering(rendering_time, "DIFFERENCES")
        report_writer.report_failure(project_filepath, ref_filepath, output_filepath, log_filepath,
                                     "Output and reference images are significantly different",
                                     num_diff, max_diff, num_comps, diff_filepath)
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
    parser.add_argument("-s", "--skip-rendering", action='store_true', dest="skip_rendering",
                        help="skip actual rendering, only generate the HTML report")
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
