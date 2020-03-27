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

from __future__ import division
from __future__ import print_function
import argparse
import colorama
import datetime
import os
import png
import subprocess
import sys
import urllib

# We can't import modules from parent directories without modifying sys.path
sys.path.append(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import utils


# --------------------------------------------------------------------------------------------------
# Constants.
# --------------------------------------------------------------------------------------------------

VERSION = "1.0"

APPLESEED_BASE_ARGS = ""

VALUE_THRESHOLD = 2                 # max allowed absolute diff between two pixel components, in [0, 255]
MAX_DIFFERING_COMPONENTS = 4 * 2    # max number of pixel components that are allowed to differ significantly

CURRENT_TIME = datetime.datetime.now()


# --------------------------------------------------------------------------------------------------
# Utilities.
# --------------------------------------------------------------------------------------------------

def remove_prefix(text, prefix):
    return text[len(prefix):] if text.startswith(prefix) else text


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


def load_file(filepath):
    with open(filepath, "rt") as file:
        return file.read()


def read_png_file(filepath):
    data = png.Reader(filename=filepath).asRGBA8()
    width = data[0]
    height = data[1]
    rows = list(data[2])
    return width, height, rows


def write_rgba_png_file(filepath, rows):
    width = len(rows[0]) / 4
    height = len(rows)
    writer = png.Writer(width=width, height=height, alpha=True)
    with open(filepath, 'wb') as file:
        writer.write(file, rows)


# --------------------------------------------------------------------------------------------------
# Utility class to log progress.
# --------------------------------------------------------------------------------------------------

class Logger:

    SCENE_COLUMN_WIDTH = 82
    TIME_COLUMN_WIDTH = 16
    RESULT_COLUMN_WIDTH = 13

    def begin_table(self):
        self.print_separator()
        self.print_row("Scene", "Time", "Result")
        self.print_separator()

    def end_table(self):
        self.print_separator()

    def print_row(self, scene, time, result):
        print("| {0} | {1} | {2} |".format(scene.ljust(self.SCENE_COLUMN_WIDTH),
                                           time.rjust(self.TIME_COLUMN_WIDTH),
                                           result.rjust(self.RESULT_COLUMN_WIDTH)))

    def print_separator(self):
        print("+-{0}-+-{1}-+-{2}-+".format("-" * self.SCENE_COLUMN_WIDTH,
                                           "-" * self.TIME_COLUMN_WIDTH,
                                           "-" * self.RESULT_COLUMN_WIDTH))

    def skip_rendering(self, scene):
        DARK_GRAY = colorama.Style.BRIGHT + colorama.Fore.BLACK
        self.__print_scene(scene, DARK_GRAY)
        self.__print_result("", "Skipped", DARK_GRAY)

    def start_rendering(self, scene):
        self.__print_scene(scene)

    def pass_rendering(self, rendering_time):
        self.__print_result(format_duration(rendering_time), "Passed", colorama.Fore.GREEN)

    def fail_rendering(self, rendering_time, message):
        self.__print_result(format_duration(rendering_time), message, colorama.Fore.RED)

    def __print_scene(self, scene, color=colorama.Fore.RESET):
        scene = remove_prefix(scene, "./")
        scene = remove_prefix(scene, ".\\")
        if len(scene) > self.SCENE_COLUMN_WIDTH:
            scene = scene[:self.SCENE_COLUMN_WIDTH - 3] + "..."
        print("| {0}{1}{2} | ".format(color,
                                      scene.ljust(self.SCENE_COLUMN_WIDTH),
                                      colorama.Fore.RESET), end='')

    def __print_result(self, time, message, color=colorama.Fore.RESET):
        print("{0} | {1}{2}{3} |".format(time.rjust(self.TIME_COLUMN_WIDTH),
                                         color,
                                         message.rjust(self.RESULT_COLUMN_WIDTH),
                                         colorama.Fore.RESET))


# --------------------------------------------------------------------------------------------------
# Utility class to generate an HTML report.
# --------------------------------------------------------------------------------------------------

class ReportWriter:

    def __init__(self, template_directory):
        self.header_template = load_file(os.path.join(template_directory, "header_template.html"))
        self.footer_template = load_file(os.path.join(template_directory, "footer_template.html"))
        self.simple_failure_template = load_file(os.path.join(template_directory, "simple_failure_template.html"))
        self.detailed_failure_template = load_file(os.path.join(template_directory, "detailed_failure_template.html"))

    def open(self, args, filepath):
        self.args = args
        self.file = open(filepath, 'w')
        self.__write_header(args)
        self.failures = 0
        self.all_commands = []

    def close(self):
        self.__write_footer()
        self.file.close()

    def report_simple_failure(self, scene, reference_filepath, output_filepath, log_filepath, error_message):
        self.failures += 1

        command = self.__make_update_command(output_filepath, reference_filepath)
        self.all_commands.append(command)

        self.file.write(self.__render(self.simple_failure_template,
                                      {'project-path': scene,
                                       'ref-image-url': urllib.pathname2url(reference_filepath),
                                       'output-image-url': urllib.pathname2url(output_filepath),
                                       'failure-reason': error_message,
                                       'log-file-url': urllib.pathname2url(log_filepath),
                                       'log-file-path': os.path.basename(log_filepath),
                                       'update-command': command}))
        self.file.flush()

    def report_detailed_failure(self, scene, reference_filepath, output_filepath, log_filepath, error_message, num_diff, max_diff, num_comps, diff_filepath):
        self.failures += 1

        command = self.__make_update_command(output_filepath, reference_filepath)
        self.all_commands.append(command)

        self.file.write(self.__render(self.detailed_failure_template,
                                      {'project-path': scene,
                                       'ref-image-url': urllib.pathname2url(reference_filepath),
                                       'diff-image-url': urllib.pathname2url(diff_filepath) if diff_filepath is not None else "",
                                       'output-image-url': urllib.pathname2url(output_filepath),
                                       'failure-reason': error_message,
                                       'log-file-url': urllib.pathname2url(log_filepath),
                                       'log-file-path': os.path.basename(log_filepath),
                                       'max-abs-diff': max_diff,
                                       'diff-comps-count': num_diff,
                                       'diff-comps-percents': "{0:.2f}".format(100.0 * num_diff / num_comps),
                                       'update-command': command}))
        self.file.flush()

    def __write_header(self, args):
        script_path = os.path.realpath(__file__)

        self.file.write(self.__render(self.header_template,
                                      {'test-date': CURRENT_TIME,
                                       'python-version': utils.get_python_version(),
                                       'script-path': script_path,
                                       'script-version': VERSION,
                                       'appleseed-binary-path': args.tool_path,
                                       'max-abs-diff-allowed': VALUE_THRESHOLD,
                                       'max-diff-comps-count-allowed': MAX_DIFFERING_COMPONENTS}))
        self.file.flush()

    def __write_footer(self):
        self.file.write(self.__render(self.footer_template, {}))

    def __render(self, template, variables):
        html = template
        for name, value in variables.iteritems():
            html = html.replace('{' + name + '}', str(value))
        return html

    def __make_update_command(self, output_filepath, reference_filepath):
        if os.name == 'nt':
            return 'copy /Y "{0}" "{1}"'.format(output_filepath, reference_filepath)
        else:
            return 'cp "{0}" "{1}"'.format(output_filepath, reference_filepath)


# --------------------------------------------------------------------------------------------------
# Render a given project file.
# --------------------------------------------------------------------------------------------------

def render_project_file(args, project_filepath, output_filepath, log_filepath):
    with open(log_filepath, "w", 0) as log_file:
        # Base command line.
        command = '"{0}" -o "{1}" "{2}"'.format(args.tool_path, output_filepath, project_filepath)

        # Built-in additional arguments.
        command += " " + APPLESEED_BASE_ARGS

        # Additional arguments passed on runtestsuite.py's command line.
        if args.args:
            command += " {0}".format(" ".join(args.args))

        log_file.write("Command line:\n    {0}\n\n".format(command))

        # Invoke appleseed.
        start_time = datetime.datetime.now()
        result = subprocess.call(command, stderr=log_file, shell=True)
        end_time = datetime.datetime.now()

        return result == 0, end_time - start_time


# --------------------------------------------------------------------------------------------------
# Compare two images.
# Returns a (num_diff, max_diff, diff_image) where:
#   num_diff is the number of components whose absolute difference is larger than value_threshold
#   max_diff is the maximum absolute difference between two components
#   diff_image is the difference between the two images
# --------------------------------------------------------------------------------------------------

def compare_images(rows1, rows2, value_threshold):
    num_diff = 0
    max_diff = 0
    diff_image = []

    for row1, row2 in zip(rows1, rows2):
        diff_row = [abs(val1 - val2) for val1, val2 in zip(row1, row2)]
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

            if image_min != image_max:
                fm = int(fit(m, image_min, image_max, 0, 255))
            else:
                fm = m

            row[i + 0] = fm
            row[i + 1] = 0
            row[i + 2] = 255 - fm
            row[i + 3] = 255


# --------------------------------------------------------------------------------------------------
# Render a given test scene.
# Return True if the test scene passed, False if it failed to render, or if the output does not
# match the reference image.
# --------------------------------------------------------------------------------------------------

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
        logger.fail_rendering(rendering_time, "Failed")
        report_writer.report_simple_failure(project_filepath, ref_filepath, output_filepath, log_filepath, "Rendering failed")
        return False

    if not os.path.exists(output_filepath):
        if not os.path.exists(ref_filepath):
            logger.pass_rendering(rendering_time)
            return True
        else:
            logger.fail_rendering(rendering_time, "No Output")
            report_writer.report_simple_failure(project_filepath, ref_filepath, output_filepath, log_filepath, "Output image is missing")
            return False
    else:
        if not os.path.exists(ref_filepath):
            logger.fail_rendering(rendering_time, "No Reference")
            report_writer.report_simple_failure(project_filepath, ref_filepath, output_filepath, log_filepath, "Reference image is missing")
            return False

    out_width, out_height, out_rows = read_png_file(output_filepath)
    ref_width, ref_height, ref_rows = read_png_file(ref_filepath)

    if out_width != ref_width or out_height != ref_height:
        logger.fail_rendering(rendering_time, "Size Mismatch")
        report_writer.report_simple_failure(project_filepath, ref_filepath, output_filepath, log_filepath,
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
        report_writer.report_detailed_failure(project_filepath, ref_filepath, output_filepath, log_filepath,
                                              "Output and reference images are significantly different",
                                              num_diff, max_diff, num_comps, diff_filepath)
        return False

    logger.pass_rendering(rendering_time)

    return True


# --------------------------------------------------------------------------------------------------
# Render all test scenes in a given directory (possibly recursively).
# Returns the number of rendered and passing test scenes.
# --------------------------------------------------------------------------------------------------

def render_test_scenes(script_directory, args):
    rendered_scene_count = 0
    passing_scene_count = 0

    logger = Logger()
    logger.begin_table()

    report_writer = ReportWriter(script_directory)
    report_writer.open(args, "report.html")

    for dirpath, dirnames, filenames in walk(args.directory, args.recursive):
        if should_skip(os.path.basename(dirpath)):
            logger.skip_rendering(dirpath)
            continue

        for filename in filenames:
            if os.path.splitext(filename)[1] == '.appleseed':
                if should_skip(filename):
                    logger.skip_rendering(os.path.join(dirpath, filename))
                    continue

                rendered_scene_count += 1

                if render_test_scene(args, logger, report_writer, dirpath, filename):
                    passing_scene_count += 1

    report_writer.close()

    logger.end_table()

    return rendered_scene_count, passing_scene_count


# --------------------------------------------------------------------------------------------------
# Entry point.
# --------------------------------------------------------------------------------------------------

def print_configuration(appleseed_path, appleseed_args):
    print("Configuration:")
    print("  Binary         : {0}".format(appleseed_path))
    print("  Arguments      : {0}".format(appleseed_args))
    print()


def main():
    colorama.init()

    parser = argparse.ArgumentParser(description="run the functional test suite.")
    parser.add_argument("-t", "--tool-path", required=True, metavar="tool-path",
                        help="set the path to the appleseed.cli tool")
    parser.add_argument("-r", "--recursive", action='store_true', dest="recursive",
                        help="scan the specified directory and all its subdirectories")
    parser.add_argument("-s", "--skip-rendering", action='store_true', dest="skip_rendering",
                        help="skip actual rendering, only generate the HTML report")
    parser.add_argument("-p", "--parameter", dest="args", metavar="ARG", nargs="*",
                        help="forward additional arguments to appleseed")
    parser.add_argument("directory", nargs='?', default=".", help="directory to scan")
    args = parser.parse_args()

    if not os.path.isfile(args.tool_path):
        sys.exit("Error: The given path to appleseed.cli does not exist.")

    script_directory = os.path.dirname(os.path.realpath(__file__))

    appleseed_args = APPLESEED_BASE_ARGS
    if args.args:
        appleseed_args += " {0}".format(" ".join(args.args))

    utils.print_runtime_details("runtestsuite", VERSION, os.path.realpath(__file__), CURRENT_TIME)
    print_configuration(args.tool_path, appleseed_args)

    start_time = datetime.datetime.now()
    rendered_scene_count, passing_scene_count = render_test_scenes(script_directory, args)
    end_time = datetime.datetime.now()

    success = 100.0 * passing_scene_count / rendered_scene_count if rendered_scene_count > 0 else 0.0

    print()
    print("Results:")
    print("  Success Rate   : {0}{1:.2f} %{2}"
          .format(colorama.Fore.RED if passing_scene_count < rendered_scene_count else colorama.Fore.GREEN,
                  success,
                  colorama.Fore.RESET))
    print("  Failures       : {0}{1} out of {2} test scene(s){3}"
          .format(colorama.Fore.RED if passing_scene_count < rendered_scene_count else colorama.Fore.GREEN,
                  rendered_scene_count - passing_scene_count,
                  rendered_scene_count,
                  colorama.Fore.RESET))
    print("  Total Time     : {0}".format(format_duration(end_time - start_time)))


if __name__ == "__main__":
    main()
