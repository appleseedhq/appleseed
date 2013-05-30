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
import traceback
import xml.dom.minidom as xml


#--------------------------------------------------------------------------------------------------
# Constants.
#--------------------------------------------------------------------------------------------------

VERSION = "1.8"
RENDERS_DIR = "_renders"
ARCHIVE_DIR = "_archives"
LOGS_DIR = "_logs"
PAUSE_BETWEEN_CHECKS = 10   # in seconds


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

def safe_mkdir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)

def format_message(severity, msg):
    now = datetime.datetime.now()
    padded_severity = severity.ljust(7)
    return "\n".join("{0} watch {1} | {2}".format(now, padded_severity, line) \
        for line in msg.splitlines())


#--------------------------------------------------------------------------------------------------
# Log backend to write to the console, using colors on systems that support them.
#--------------------------------------------------------------------------------------------------

class ConsoleBackend:
    @staticmethod
    def info(msg):
        print("{0}".format(msg))

    @staticmethod
    def warning(msg):
        if ConsoleBackend.is_coloring_supported():
            print("\033[93m{0}\033[0m".format(msg))
        else:
            print("{0}".format(msg))

    @staticmethod
    def error(msg):
        if ConsoleBackend.is_coloring_supported():
            print("\033[91m{0}\033[0m".format(msg))
        else:
            print("{0}".format(msg))

    @staticmethod
    def is_coloring_supported():
        return os.system == 'darwin'


#--------------------------------------------------------------------------------------------------
# Log backend to write to a log file.
#--------------------------------------------------------------------------------------------------

class LogFileBackend:
    def __init__(self, path):
        self.path = path

    def write(self, msg):
        safe_mkdir(os.path.dirname(self.path))

        with open(self.path, "a") as file:
            file.write(msg + "\n")


#--------------------------------------------------------------------------------------------------
# Log class to simultaneously write to a log file and to the console.
#--------------------------------------------------------------------------------------------------

class Log:
    def __init__(self, path):
        self.log_file = LogFileBackend(path)

    def info(self, msg):
        formatted_msg = format_message("info", msg)
        self.log_file.write(formatted_msg)
        ConsoleBackend.info(formatted_msg)

    def warning(self, msg):
        formatted_msg = format_message("warning", msg)
        self.log_file.write(formatted_msg)
        ConsoleBackend.warning(formatted_msg)

    def error(self, msg):
        formatted_msg = format_message("error", msg)
        self.log_file.write(formatted_msg)
        ConsoleBackend.error(formatted_msg)

    @staticmethod
    def info_no_log(msg):
        ConsoleBackend.info(format_message("info", msg))

    @staticmethod
    def warning_no_log(msg):
        ConsoleBackend.warning(format_message("warning", msg))

    @staticmethod
    def error_no_log(msg):
        ConsoleBackend.error(format_message("error", msg))


#--------------------------------------------------------------------------------------------------
# Code to temporarily disable Windows Error Reporting.
#--------------------------------------------------------------------------------------------------

if os.name == "nt":
    import _winreg

    WER_KEY_PATH = r"Software\Microsoft\Windows\Windows Error Reporting"

    def open_wer_key():
        try:
            return _winreg.OpenKey(_winreg.HKEY_CURRENT_USER, WER_KEY_PATH, 0,
                                   _winreg.KEY_ALL_ACCESS)
        except WindowsError:
            pass

        try:
            return _winreg.CreateKey(_winreg.HKEY_CURRENT_USER, WER_KEY_PATH)
        except WindowsError:
            pass

        return None

    def configure_wer(dont_show_ui, disabled):
        key = open_wer_key()

        if key is None:
            return None

        previous_dont_show_ui = _winreg.QueryValueEx(key, "DontShowUI")[0]
        previous_disabled = _winreg.QueryValueEx(key, "Disabled")[0]

        _winreg.SetValueEx(key, "DontShowUI", 0, _winreg.REG_DWORD, dont_show_ui)
        _winreg.SetValueEx(key, "Disabled", 0, _winreg.REG_DWORD, disabled)

        _winreg.CloseKey(key)

        return previous_dont_show_ui, previous_disabled

    def get_wer_status():
        key = open_wer_key()

        if key is None:
            return "(unavailable)"

        dont_show_ui = _winreg.QueryValueEx(key, "DontShowUI")[0]
        disabled = _winreg.QueryValueEx(key, "Disabled")[0]

        _winreg.CloseKey(key)

        return "DontShowUI={0} Disabled={1}".format(dont_show_ui, disabled)

    def enable_wer(log):
        log.info("enabling windows error reporting...")
        previous_values = configure_wer(0, 0)
        if previous_values is None:
            log.warning("could not enable windows error reporting.")
        return previous_values

    def disable_wer(log):
        log.info("disabling windows error reporting...")
        previous_values = configure_wer(1, 1)
        if previous_values is None:
            log.warning("could not disable windows error reporting.")
        return previous_values

    def restore_wer(previous_values, log):
        log.info("restoring initial windows error reporting parameters...")
        if configure_wer(previous_values[0], previous_values[1]) is None:
            log.warning("could not restore initial windows error reporting parameters.")


#--------------------------------------------------------------------------------------------------
# Launches appleseed.cli and print appleseed version information.
#--------------------------------------------------------------------------------------------------

def print_appleseed_version(args, log):
    try:
        p = subprocess.Popen([args.tool_path, "--version"], stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        version_string = p.communicate()[1].split(os.linesep, 1)[0]
        log.info("running {0}.".format(version_string))
    except OSError:
        log.error("failed to query {0} version.".format(args.tool_path))
        sys.exit(1)


#--------------------------------------------------------------------------------------------------
# Rendering logic.
#--------------------------------------------------------------------------------------------------

class ProcessFailedException(Exception):
    pass

def render_project(args, project_filepath, log):
    # Rename the project file so others don't try to render it.
    user_project_filepath = project_filepath + "." + args.user_name
    try:
        os.rename(project_filepath, user_project_filepath)
    except:
        # log.warning("failed to acquire {0}.".format(project_filepath))
        return False

    log.info("starting rendering {0}...".format(project_filepath))
    start_time = datetime.datetime.now()

    try:
        # Create shell command.
        project_filename = os.path.split(project_filepath)[1]
        output_filename = os.path.splitext(project_filename)[0] + '.' + args.output_format
        output_filepath = os.path.join(args.directory, RENDERS_DIR, output_filename)
        command = '"{0}" -o "{1}" "{2}"'.format(args.tool_path, output_filepath,
                                                user_project_filepath)
        if args.args:
            command += ' {0}'.format(" ".join(args.args))

        # Make sure the output directory exists.
        safe_mkdir(os.path.join(args.directory, RENDERS_DIR))

        # Execute command.
        result = subprocess.call(command, shell=True)
        if result != 0:
            raise ProcessFailedException()
    except:
        # Something failed, rename the project file back to its original name.
        log.error("failed to render {0}.".format(project_filepath))
        os.rename(user_project_filepath, project_filepath)
        raise

    # Create the archive directory if necessary.
    try:
        archive_dir = os.path.join(args.directory, ARCHIVE_DIR)
        safe_mkdir(archive_dir)
    except:
        pass

    try:
        # Rename the file and move it into the archive directory.
        shutil.move(user_project_filepath, os.path.join(archive_dir, project_filename))
    except:
        # For some reason we couldn't rename and move the file, try moving it without renaming it.
        user_project_filename = os.path.split(user_project_filepath)[1]
        shutil.move(user_project_filepath, os.path.join(archive_dir, user_project_filename))

    rendering_time = datetime.datetime.now() - start_time
    log.info("successfully rendered {0} in {1}.".format(project_filepath, rendering_time))

    return True


#--------------------------------------------------------------------------------------------------
# Watching logic.
#--------------------------------------------------------------------------------------------------

def get_project_files(directory):
    project_files = []

    for entry in os.listdir(directory):
        filepath = os.path.join(directory, entry)
        if os.path.isfile(filepath):
            if os.path.splitext(filepath)[1] == '.appleseed':
                project_files.append(filepath)

    return project_files

def convert_path_to_local(path):
    if os.name == "nt":
        return path.replace('/', '\\')
    else:
        return path.replace('\\', '/')

def extract_project_deps(project_filepath, log):
    try:
        with open(project_filepath, 'r') as file:
            contents = file.read()
    except:
        # log.warning("failed to acquire {0}.".format(project_filepath))
        return False, set()

    deps = set()
    directory = os.path.split(project_filepath)[0]

    for node in xml.parseString(contents).getElementsByTagName('parameter'):
        if node.getAttribute('name') == 'filename':
            filepath = node.getAttribute('value')
            filepath = convert_path_to_local(filepath)
            filepath = os.path.join(directory, filepath)
            deps.add(filepath)

    for node in xml.parseString(contents).getElementsByTagName('parameters'):
        if node.getAttribute('name') == 'filename':
            for child in node.childNodes:
                if child.nodeType == xml.Node.ELEMENT_NODE:
                    filepath = child.getAttribute('value')
                    filepath = convert_path_to_local(filepath)
                    filepath = os.path.join(directory, filepath)
                    deps.add(filepath)

    return True, deps

def count_missing_project_deps(deps):
    missing_deps = 0

    for filepath in deps:
        if not os.path.exists(filepath):
            missing_deps += 1

    return missing_deps

def watch(args, log):
    # Look for project files in the watch directory.
    project_files = get_project_files(args.directory)

    # No project file found.
    if len(project_files) == 0:
        Log.info_no_log("waiting for incoming data...")
        return False

    # Shuffle the array of project files.
    random.shuffle(project_files)

    # Render the first project file that has no missing dependencies.
    for project_filepath in project_files:
        deps_success, deps = extract_project_deps(project_filepath, log)
        if not deps_success:
            continue

        missing_deps = count_missing_project_deps(deps)
        if missing_deps > 0:
            Log.info_no_log("{0} missing dependencies for {1}.".format(missing_deps,
                                                                       project_filepath))
            continue

        return render_project(args, project_filepath, log)

    # None of the project file has all its dependencies ready.
    return False


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    # Parse the command line.
    parser = argparse.ArgumentParser(description="watch a directory and render any project file " \
                                     "that appears in it.")
    parser.add_argument("-t", "--tool-path", metavar="tool-path", required=True,
                        help="set the path to the appleseed.cli tool")
    parser.add_argument("-f", "--format", dest="output_format", metavar="FORMAT", default="exr",
                        help="set output format (e.g. png, exr)")
    parser.add_argument("-u", "--user", dest="user_name", metavar="NAME", default="anonymous",
                        help="set user name")
    parser.add_argument("-p", "--parameter", dest="args", metavar="ARG", nargs="*",
                        help="forward additional arguments to appleseed")
    parser.add_argument("directory", help="directory to watch")
    args = parser.parse_args()

    # If no watch directory is provided, watch the current directory.
    if args.directory is None:
        args.directory = os.getcwd()

    # Start the log.
    log = Log(os.path.join(args.directory, LOGS_DIR, args.user_name + ".log"))
    log.info("--- starting logging ---")

    # Print version information.
    log.info("running watchfolder.py version {0}.".format(VERSION))
    print_appleseed_version(args, log)

    # Disable Windows Error Reporting on Windows.
    if os.name == "nt":
        log.info("initial windows error reporting status: {0}".format(get_wer_status()))
        initial_wer_values = disable_wer(log)
        log.info("new windows error reporting status: {0}".format(get_wer_status()))

    log.info("watching directory {0}".format(os.path.abspath(args.directory)))
    random.seed()

    # Main watch/render loop.
    try:
        while True:
            try:
                while watch(args, log): pass
                time.sleep(PAUSE_BETWEEN_CHECKS)
            except KeyboardInterrupt, SystemExit:
                raise
            except ProcessFailedException:
                time.sleep(PAUSE_BETWEEN_CHECKS)
            except:
                exc_type, exc_value, exc_traceback = sys.exc_info()
                lines = traceback.format_exception(exc_type, exc_value, exc_traceback)
                log.error("".join(line for line in lines))
                time.sleep(PAUSE_BETWEEN_CHECKS)
    except KeyboardInterrupt, SystemExit:
        pass

    # Restore initial Windows Error Reporting parameters.
    if os.name == "nt":
        restore_wer(initial_wer_values, log)
        log.info("final windows error reporting status: {0}".format(get_wer_status()))

    log.info("exiting...")

if __name__ == '__main__':
    main()
