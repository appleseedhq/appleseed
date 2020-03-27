#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2012-2013 Jonathan Topf, Jupiter Jazz Limited
# Copyright (c) 2014-2018 Jonathan Topf, The appleseedhq Organization
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
import socket
import string
import subprocess
import sys
import time
import traceback
import xml.dom.minidom as xml

from utils import print_runtime_details


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "1.18"
RENDERS_DIR = "_renders"
ARCHIVE_DIR = "_archives"
LOGS_DIR = "_logs"
PAUSE_BETWEEN_CHECKS = 10   # in seconds
DEFAULT_TOOL_FILENAME = "appleseed.cli.exe" if os.name == "nt" else "appleseed.cli"


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

def safe_mkdir(dir):
    if not os.path.exists(dir):
        os.mkdir(dir)


def convert_path_to_local(path):
    if os.name == "nt":
        return path.replace('/', '\\')
    else:
        return path.replace('\\', '/')


def format_message(severity, msg):
    now = datetime.datetime.now()
    timestamp = now.strftime("%Y-%m-%d %H:%M:%S.%f")
    padded_severity = severity.ljust(7)
    return "\n".join("{0} node  {1} | {2}".format(timestamp, padded_severity, line)
                     for line in msg.splitlines())


VALID_USER_NAME_CHARS = frozenset("%s%s_-" % (string.ascii_letters, string.digits))


def cleanup_user_name(user_name):
    return "".join(c if c in VALID_USER_NAME_CHARS else '_' for c in user_name)


# -------------------------------------------------------------------------------------------------
# Log backend to write to the console, using colors on systems that support them.
# -------------------------------------------------------------------------------------------------

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


# -------------------------------------------------------------------------------------------------
# Log backend to write to a log file.
# -------------------------------------------------------------------------------------------------

class LogFileBackend:

    def __init__(self, path):
        self.path = path

    def write(self, msg):
        safe_mkdir(os.path.dirname(self.path))

        with open(self.path, "a") as file:
            file.write(msg + "\n")


# -------------------------------------------------------------------------------------------------
# Log class to simultaneously write to a log file and to the console.
# -------------------------------------------------------------------------------------------------

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


# -------------------------------------------------------------------------------------------------
# Code to temporarily disable Windows Error Reporting.
# -------------------------------------------------------------------------------------------------

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


# -------------------------------------------------------------------------------------------------
# Launches appleseed.cli and print appleseed version information.
# -------------------------------------------------------------------------------------------------

def print_appleseed_version(args, log):
    try:
        p = subprocess.Popen([args.tool_path, "--version", "--system"], stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        output = p.communicate()[1].split(os.linesep, 1)

        if p.returncode != 0:
            log.error("failed to query {0} version (return code: {1}).".format(args.tool_path, p.returncode))
            sys.exit(1)

        for line in output:
            log.info("{0}".format(line))

    except OSError:
        log.error("failed to query {0} version.".format(args.tool_path))
        sys.exit(1)


# -------------------------------------------------------------------------------------------------
# Rendering logic.
# -------------------------------------------------------------------------------------------------

class ProcessFailedException(Exception):
    pass


def render_project(args, project_filepath, log):
    # Assign the project file to ourselves.
    assigned_project_filepath = project_filepath + "." + args.user_name
    try:
        os.rename(project_filepath, assigned_project_filepath)
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
        command = '"{0}" -o "{1}" "{2}"'.format(args.tool_path, output_filepath, assigned_project_filepath)
        if args.args:
            command += ' {0}'.format(" ".join(args.args))

        # Make sure the output directory exists.
        safe_mkdir(os.path.join(args.directory, RENDERS_DIR))

        # Execute command.
        result = subprocess.call(command, shell=True)
        if result != 0:
            raise ProcessFailedException()
    except:
        # Rendering failed.
        log.error("failed to render {0}.".format(project_filepath))

        # Unassign the project file.
        try:
            os.rename(assigned_project_filepath, project_filepath)
        except:
            pass

        # Propagate the exception.
        raise

    # Rendering succeeded.
    rendering_time = datetime.datetime.now() - start_time
    log.info("successfully rendered {0} in {1}.".format(project_filepath, rendering_time))

    # Move the project file to the archive directory.
    archive_dir = os.path.join(args.directory, ARCHIVE_DIR)
    archived_project_filepath = os.path.join(archive_dir, project_filename)
    try:
        safe_mkdir(archive_dir)
        shutil.move(assigned_project_filepath, archived_project_filepath)
    except:
        log.error("failed to move {0} to {1}.".format(assigned_project_filepath, archived_project_filepath))

    return True


# -------------------------------------------------------------------------------------------------
# Watching logic.
# -------------------------------------------------------------------------------------------------

def get_project_files(directory):
    project_files = []

    for entry in os.listdir(directory):
        filepath = os.path.join(directory, entry)
        if os.path.isfile(filepath):
            if os.path.splitext(filepath)[1] == '.appleseed':
                project_files.append(filepath)

    return project_files


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


def gather_missing_project_deps(deps):
    missing_deps = []

    for filepath in deps:
        if not os.path.exists(filepath):
            missing_deps.append(filepath)

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

        missing_deps = gather_missing_project_deps(deps)

        if len(missing_deps) > 0:
            if args.print_missing_deps:
                Log.info_no_log("{0} missing dependencies for {1}:".format(len(missing_deps),
                                                                           project_filepath))
                for dep in missing_deps:
                    Log.info_no_log("  {0}".format(dep))
            else:
                Log.info_no_log("{0} missing dependencies for {1}.".format(len(missing_deps),
                                                                           project_filepath))
            continue

        return render_project(args, project_filepath, log)

    # None of the project file has all its dependencies ready.
    return False


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    # Parse the command line.
    parser = argparse.ArgumentParser(description="continuously watch a directory and render any "
                                     "appleseed project file that appears in it.")
    parser.add_argument("-t", "--tool-path", metavar="tool-path",
                        help="set the path to the appleseed.cli tool")
    parser.add_argument("-f", "--format", dest="output_format", metavar="FORMAT", default="exr",
                        help="set output format (e.g. png, exr)")
    parser.add_argument("-u", "--user", dest="user_name", metavar="NAME",
                        help="set user name (by default the host name is used)")
    parser.add_argument("-p", "--parameter", dest="args", metavar="ARG", nargs="*",
                        help="forward additional arguments to appleseed")
    parser.add_argument("--print-missing-deps", action='store_true',
                        help="print missing dependencies")
    parser.add_argument("directory", help="directory to watch")
    args = parser.parse_args()

    print_runtime_details("rendernode", VERSION, os.path.realpath(__file__))

    # If no tool path is provided, search for the tool in the same directory as this script.
    if args.tool_path is None:
        script_directory = os.path.dirname(os.path.realpath(__file__))
        args.tool_path = os.path.join(script_directory, DEFAULT_TOOL_FILENAME)
        print("setting tool path to {0}.".format(args.tool_path))

    # If no watch directory is provided, watch the current directory.
    if args.directory is None:
        args.directory = os.getcwd()

    # If no user name is provided, use the host name.
    if args.user_name is None:
        args.user_name = socket.gethostname()

    # Clean up the user name.
    args.user_name = cleanup_user_name(args.user_name)

    # Start the log.
    log = Log(os.path.join(args.directory, LOGS_DIR, args.user_name + ".log"))
    log.info("--- starting logging ---")

    # Print version information.
    log.info("running rendernode.py version {0}.".format(VERSION))
    print_appleseed_version(args, log)

    # Print the user name.
    log.info("user name is {0}.".format(args.user_name))

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
                while watch(args, log):
                    pass
            except KeyboardInterrupt, SystemExit:
                raise
            except ProcessFailedException:
                pass
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


if __name__ == "__main__":
    main()
