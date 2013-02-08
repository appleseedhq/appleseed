#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2012-2013 Jonathan Topf
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
import datetime
import os
import random
import shutil
import subprocess
import sys
import time
import xml.dom.minidom


#--------------------------------------------------------------------------------------------------
# Constants.
#--------------------------------------------------------------------------------------------------

OUTPUT_DIR = "_output"
COMPLETED_DIR = "_completed"
LOGS_DIR = "_logs"
APPLESEED_BIN = "appleseed.cli"


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

def xstr(s):
    return "N/A" if s is None else str(s)

def safe_mkdir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)


#--------------------------------------------------------------------------------------------------
# Console class to write to the console, using colors on systems that support them.
#--------------------------------------------------------------------------------------------------

class Console:
    @staticmethod
    def is_coloring_supported():
        return os.system == 'darwin'

    @staticmethod
    def format_message(msg):
        return "[{0}] {1}".format(datetime.datetime.now(), msg)

    @staticmethod
    def info(msg):
        print("{0}".format(Console.format_message(msg)))

    @staticmethod
    def success(msg):
        s = Console.format_message(msg)
        if Console.is_coloring_supported():
            print("\033[92m{0}\033[0m".format(s))
        else:
            print("{0}".format(s))

    @staticmethod
    def warning(msg):
        s = Console.format_message(msg)
        if Console.is_coloring_supported():
            print("\033[93m{0}\033[0m".format(s))
        else:
            print("{0}".format(s))

    @staticmethod
    def error(msg):
        s = Console.format_message(msg)
        if Console.is_coloring_supported():
            print("\033[91m{0}\033[0m".format(s))
        else:
            print("{0}".format(s))


#--------------------------------------------------------------------------------------------------
# Log class to write to log files.
#--------------------------------------------------------------------------------------------------

class Log:
    def __init__(self, path):
        self.path = path
        self.reset()
        self.emit("# Beginning logging at {0}.".format(datetime.datetime.now()))

    def reset(self):
        self.project_file = None
        self.start_time = None
        self.end_time = None

    def begin_rendering(self, project_file):
        self.project_file = project_file
        self.start_time = datetime.datetime.now()

    def end_rendering(self):
        self.end_time = datetime.datetime.now()
        self.message("Completed")
        self.reset()

    def message(self, msg):
        self.emit("{0} : {1} : {2} : {3}".format(xstr(self.project_file), xstr(self.start_time), xstr(self.end_time), xstr(msg)))

    def emit(self, msg):
        with open(self.path, "a") as file:
            file.write(msg + "\n")


#--------------------------------------------------------------------------------------------------
# Watching and rendering logic.
#--------------------------------------------------------------------------------------------------

def get_project_files(directory):
    project_files = []

    for entity in os.listdir(directory):
        file_path = os.path.join(directory, entity)
        if os.path.isfile(file_path):
            if os.path.splitext(file_path)[1] == '.appleseed':
                project_files.append(file_path)

    return project_files

def get_missing_project_dependencies(project_file):
    missing_deps = []

    directory = os.path.split(project_file)[0]

    with open(project_file, 'r') as file:
        data = file.read()

    for entity in xml.dom.minidom.parseString(data).getElementsByTagName('parameter'):
        if entity.getAttribute('name') == 'filename':
            filename = entity.getAttribute('value')

            if sys.platform == 'win32':
                filename = filename.replace('/', '\\')
            else:
                filename = filename.replace('\\', '/')

            filepath = os.path.join(directory, filename)

            if not os.path.exists(filepath):
                missing_deps.append(filepath)

    return missing_deps

def is_project_renderable(project_file):
    missing_deps = get_missing_project_dependencies(project_file)

    if len(missing_deps) == 0:
        return True

    Console.error('Missing dependencies for "{0}":'.format(os.path.split(project_file)[1]))

    for dep in missing_deps:
        Console.error("    {0}".format(dep))

    return False

def render_project(args, project_file):
    Console.success('Rendering "{0}"...'.format(project_file))

    # Rename the project file so others don't try to render it.
    suffix = "." + args.user_name
    os.rename(project_file, project_file + suffix)
    project_file += suffix

    # Create shell command.
    project_filename = os.path.split(project_file)[1]
    output_filename = os.path.splitext(project_filename)[0] + '.png'
    output_filepath = os.path.join(args.watch_dir, OUTPUT_DIR, output_filename)
    command = '"{0}" -o "{1}" "{2}"'.format(args.appleseed_bin_path, output_filepath, project_file)
    if args.args:
        command += ' {0}'.format(" ".join(args.args))

    # Make sure the output directory exists.
    safe_mkdir(os.path.join(args.watch_dir, OUTPUT_DIR))

    # Execute command.
    result = subprocess.call(command, shell=True)
    if result != 0:
        Console.warning('File may not have rendered correctly: "{0}".'.format(project_file))

    # Move the file into the completed directory.
    safe_mkdir(os.path.join(args.watch_dir, COMPLETED_DIR))
    move_dest = os.path.join(args.watch_dir, COMPLETED_DIR, os.path.split(project_file)[1])
    shutil.move(project_file, move_dest)

def watch(args, log):
    # Look for project files in the watch directory.
    project_files = get_project_files(args.watch_dir)

    # No project file found.
    if len(project_files) == 0:
        Console.info("Nothing to render.")
        return False

    # Define random start point for list.
    random_start_point = int(random.random() * (len(project_files) - 1))

    # Iterate over reordered list of project files.
    for project_file in project_files[random_start_point:] + project_files[:random_start_point]:
        if is_project_renderable(project_file):
            log.begin_rendering(project_file)
            render_project(args, project_file)
            log.end_rendering()
            return True

    # No renderable project file found.
    return False

def print_appleseed_version(args, log):
    try:
        p = subprocess.Popen([args.appleseed_bin_path, "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        version_string = p.communicate()[1].split(os.linesep, 1)[0]
        msg = "Running {0}.".format(version_string)
        print(msg)
        log.message(msg)
    except OSError:
        print("Failed to query {0} version.".format(APPLESEED_BIN))
        sys.exit(1)


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    # Parse the command line.
    parser = argparse.ArgumentParser(description="Watch a directory and render any project file that appears in it.")
    parser.add_argument("-a", dest="appleseed_dir", metavar="DIR", required=True, help="set appleseed binaries directory")
    parser.add_argument("-w", dest="watch_dir", metavar="DIR", help="set watch directory")
    parser.add_argument("-u", dest="user_name", metavar="NAME", help="set user name", default="anonymous")
    parser.add_argument("-p", dest="args", metavar="ARG", nargs="*", help="forward additional arguments to appleseed")
    args = parser.parse_args()

    # If no watch directory is provided, watch the current directory.
    if args.watch_dir is None:
        args.watch_dir = os.getcwd()

    # Compute the path to the command line appleseed renderer.
    args.appleseed_bin_path = os.path.join(args.appleseed_dir, APPLESEED_BIN)

    # Open the log file.
    log_dir = os.path.join(args.watch_dir, LOGS_DIR)
    log_filename = args.user_name + ".log"
    safe_mkdir(log_dir)
    log = Log(os.path.join(log_dir, log_filename))

    print_appleseed_version(args, log)
    print('Watching directory "{0}"'.format(args.watch_dir))

    # Main watch loop.
    while True:
        try:
            while watch(args, log): pass
            time.sleep(3)
        except KeyboardInterrupt, SystemExit:
            msg = "Exiting..."
            Console.info(msg)
            log.message(msg)
            break
        except:
            msg = "Unexpected error: {0}.".format(sys.exc_info()[0])
            Console.error(msg)
            log.message(msg)

if __name__ == '__main__':
    main()
