#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013 Francois Beaune, Jupiter Jazz Limited
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
import argparse
import datetime
import glob
import os
import shutil
import string
import sys
import time
import traceback
import xml.dom.minidom as xml


#--------------------------------------------------------------------------------------------------
# Constants.
#--------------------------------------------------------------------------------------------------

VERSION = "1.6"
RENDERS_DIR = "_renders"
ARCHIVE_DIR = "_archives"
LOGS_DIR = "_logs"
PAUSE_BETWEEN_CHECKS = 15   # in seconds


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

def get_directory_size(directory):
    size = 0
    for dirpath, dirnames, filenames in os.walk(directory):
        for filename in filenames:
            fp = os.path.join(dirpath, filename)
            try:
                size += os.path.getsize(fp)
            except:
                pass
    return size

def safe_mkdir(dir):
    if not os.path.exists(dir):
        os.makedirs(dir)

def convert_path_to_local(path):
    if os.name == "nt":
        return path.replace('/', '\\')
    else:
        return path.replace('\\', '/')

def format_message(severity, msg):
    now = datetime.datetime.now()
    timestamp = now.strftime("%Y-%m-%d %H:%M:%S.%f")
    padded_severity = severity.ljust(7)
    return "\n".join("{0} mgr   {1} | {2}".format(timestamp, padded_severity, line) \
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
# Dependencies handling.
#--------------------------------------------------------------------------------------------------

def get_files(directory, pattern):
    files = []
    for file in glob.glob(os.path.join(directory, pattern)):
        files.append(file)
    return files

def get_pending_and_rendering_project_files(directory):
    return get_files(directory, "*.appleseed*")

def extract_project_deps(directory, project_filepath):
    try:
        with open(os.path.join(directory, project_filepath), 'r') as file:
            contents = file.read()
    except:
        return False, set()

    deps = set()

    for node in xml.parseString(contents).getElementsByTagName('parameter'):
        if node.getAttribute('name') == 'filename':
            filepath = node.getAttribute('value')
            filepath = convert_path_to_local(filepath)
            deps.add(filepath)

    for node in xml.parseString(contents).getElementsByTagName('parameters'):
        if node.getAttribute('name') == 'filename':
            for child in node.childNodes:
                if child.nodeType == xml.Node.ELEMENT_NODE:
                    filepath = child.getAttribute('value')
                    filepath = convert_path_to_local(filepath)
                    deps.add(filepath)

    return True, deps

def gather_dependencies_refcounts(directory, log):
    refcounts = {}

    project_files = get_pending_and_rendering_project_files(directory)

    if len(project_files) > 0:
        log.info_no_log("computing dependencies of {0} project file(s)...".format(len(project_files)))

        for project_filepath in project_files:
            deps_success, deps = extract_project_deps(directory, project_filepath)
            if not deps_success:
                continue

            for dep in deps:
                if dep in refcounts:
                    refcounts[dep] += 1
                else:
                    refcounts[dep] = 1

        log.info_no_log("done.")

    return refcounts


#--------------------------------------------------------------------------------------------------
# Project files handling.
#--------------------------------------------------------------------------------------------------

def is_project_file_being_rendered(watched_directory, project_filepath):
    for file in glob.glob(os.path.join(watched_directory, "*.appleseed.*")):
        if os.path.splitext(file)[0] == os.path.join(watched_directory, project_filepath):
            return True, os.path.splitext(file)[1][1:]

    return False, ""

def print_rendering_status(shot_directory, watched_directory, archive_directory, log):
    rendering_count = 0
    pending_count = 0
    completed_count = 0
    usernames = {}

    for entry in os.listdir(shot_directory):
        # Only consider appleseed project files.
        src_filepath = os.path.join(shot_directory, entry)
        if not os.path.isfile(src_filepath):
            continue
        if os.path.splitext(src_filepath)[1] != '.appleseed':
            continue

        # Count pending project files.
        if os.path.isfile(os.path.join(watched_directory, entry)):
            pending_count += 1
            continue

        # Count project files being rendered.
        being_rendered, username = is_project_file_being_rendered(watched_directory, entry)
        if being_rendered:
            rendering_count += 1
            usernames[username] = entry
            continue

        # Count completed project files.
        if os.path.isfile(os.path.join(archive_directory, entry)):
            completed_count += 1
            continue

    total_count = completed_count + rendering_count + pending_count
    completed_percent = 0 if total_count == 0 else 100.0 * completed_count / total_count

    log.info("project files: {0}/{1} completed ({2} %), {3} rendering, {4} pending." \
        .format(completed_count, total_count, completed_percent, rendering_count, pending_count))

    if len(usernames) > 0:
        log.info_no_log("assignments:")
        for username in usernames.keys():
            filename = usernames[username]
            log.info_no_log("  {0}    {1}".format(filename, username))
    else:
        log.info_no_log("no project file assigned.")

def remove_orphan_project_file_deps(watched_directory, archive_directory, project_filepath, deps_refcounts, log):
    # Extract the dependencies from this project file.
    deps_success, deps = extract_project_deps(archive_directory, project_filepath)
    if not deps_success:
        return 0

    removed_deps = 0

    # Remove orphan dependencies.
    for dep in deps:
        if dep not in deps_refcounts or deps_refcounts[dep] == 0:
            dep_filepath = os.path.join(watched_directory, dep)
            if os.path.isfile(dep_filepath):
                os.remove(dep_filepath)
                removed_deps += 1

    return removed_deps

def remove_orphan_dependencies(watched_directory, archive_directory, deps_refcounts, log):
    # No dependencies to remove if the archive directory doesn't exist.
    if not os.path.isdir(archive_directory):
        return

    removed_deps = 0

    for project_file in get_files(archive_directory, "*.appleseed"):
        removed_deps += remove_orphan_project_file_deps(watched_directory, archive_directory, project_file, deps_refcounts, log)

    if removed_deps > 0:
        log.info("removed {0} orphan dependency(ies).".format(removed_deps))

def try_submitting_project_file(shot_directory, watched_directory, max_size, project_filepath, log):
    # Extract the dependencies from this project file.
    deps_success, deps = extract_project_deps(shot_directory, project_filepath)
    if not deps_success:
        return True     # continue to submit files

    # Compute the total size of the project with all its missing dependencies.
    project_size = os.path.getsize(os.path.join(shot_directory, project_filepath))
    for dep in deps:
        dep_filepath = os.path.join(shot_directory, dep)
        if not os.path.isfile(dep_filepath):
            project_size += os.path.getsize(dep_filepath)
    project_size_mb = project_size / (1024 * 1024)

    # Compute the current total size of the watched directory.
    watched_dir_size = get_directory_size(watched_directory)
    watched_dir_size_mb = watched_dir_size / (1024 * 1024)

    # Don't submit this project if it doesn't fit in the remaining available space.
    if max_size is not None and watched_dir_size_mb + project_size_mb >= max_size:
        log.info("watched directory is full at {0:.2f} MB ({1} bytes), " \
                 "max allowed size is {2:.2f} MB.".format(watched_dir_size_mb, watched_dir_size, max_size))
        return False    # don't try to submit new files in this round

    log.info("submitting project file {0}, size {1:.2f} MB ({2} bytes)...".format(project_filepath, project_size_mb, project_size))

    # Submit the project file.
    shutil.copyfile(os.path.join(shot_directory, project_filepath), os.path.join(watched_directory, project_filepath))

    # Copy the missing dependencies.
    copied_deps = 0
    for dep in deps:
        if not os.path.isfile(os.path.join(watched_directory, dep)):
            src_path = os.path.join(shot_directory, dep)
            dest_path = os.path.join(watched_directory, dep)
            safe_mkdir(os.path.dirname(dest_path))
            shutil.copyfile(src_path, dest_path)
            copied_deps += 1
    if copied_deps > 0:
        log.info("copied {0} dependency(ies).".format(copied_deps))

    return True         # continue to submit files

def submit_project_files(shot_directory, watched_directory, archive_directory, max_size, log):
    for entry in os.listdir(shot_directory):
        # Only consider appleseed project files.
        src_filepath = os.path.join(shot_directory, entry)
        if not os.path.isfile(src_filepath):
            continue
        if os.path.splitext(src_filepath)[1] != '.appleseed':
            continue

        # Don't submit the project file if it is pending.
        if os.path.isfile(os.path.join(watched_directory, entry)):
            continue

        # Don't submit the project file if it is currently being rendered.
        being_rendered, username = is_project_file_being_rendered(watched_directory, entry)
        if being_rendered:
            continue

        # Don't submit the project file if it was already rendered.
        if os.path.isfile(os.path.join(archive_directory, entry)):
            continue

        # Try submitting the project file; stop submitting files if that failed.
        if not try_submitting_project_file(shot_directory, watched_directory, max_size, entry, log):
            log.info("no longer submitting project files in this round.")
            break

def move_files(source_dir, dest_dir, log):
    # No files to move if the source directory doesn't exist.
    if not os.path.isdir(source_dir):
        return

    safe_mkdir(dest_dir)

    moved_files = 0

    for entry in os.listdir(source_dir):
        source_filepath = os.path.join(source_dir, entry)
        if os.path.isfile(source_filepath):
            dest_filepath = os.path.join(dest_dir, entry)
            if os.path.isfile(dest_filepath):
                os.remove(dest_filepath)
            shutil.move(source_filepath, dest_dir)
            moved_files += 1

    if moved_files > 0:
        log.info("moved {0} file(s) to {1}.".format(moved_files,
                                                    os.path.abspath(dest_dir)))


#--------------------------------------------------------------------------------------------------
# High level management logic.
#--------------------------------------------------------------------------------------------------

def manage(args, log):
    # Compute and print the size of the watched directory.
    watched_dir_size = get_directory_size(args.watched_directory)
    watched_dir_size_mb = watched_dir_size / (1024 * 1024)
    log.info("size of watched directory is {0:.2f} MB.".format(watched_dir_size_mb))

    print_rendering_status(args.shot_directory,
                           args.watched_directory,
                           os.path.join(args.watched_directory, ARCHIVE_DIR),
                           log)

    # Gather all the dependencies of pending and rendering project files.
    deps_refcounts = gather_dependencies_refcounts(args.watched_directory, log)

    # Remove orphan dependencies.
    remove_orphan_dependencies(args.watched_directory,
                               os.path.join(args.watched_directory, ARCHIVE_DIR),
                               deps_refcounts, log)

    # Submit new project files.
    submit_project_files(args.shot_directory,
                         args.watched_directory,
                         os.path.join(args.watched_directory, ARCHIVE_DIR),
                         args.max_size, log)

    # Move rendered frames to the shot directory.
    move_files(os.path.join(args.watched_directory, RENDERS_DIR),
               args.frames_directory, log)


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    # Parse the command line.
    parser = argparse.ArgumentParser(description="send a shot to a folder being watched by " \
                                     "appleseed render nodes.")
    parser.add_argument("-s", "--max-size", metavar="MB",
                        help="set the maximum allowed size in MB of the directory being watched " \
                        "(default is unlimited)")
    parser.add_argument("--shot", metavar="shot-directory", dest="shot_directory",
                        help="directory containing the original shot data")
    parser.add_argument("--watched", metavar="watched-directory", dest="watched_directory",
                        help="directory being watched by render nodes")
    parser.add_argument("--frames", metavar="frames-directory", dest="frames_directory",
                        help="directory where the rendered frames should be stored")
    args = parser.parse_args()

    if args.max_size is not None:
        args.max_size = float(args.max_size)

    # Start the log.
    log = Log(os.path.join(args.watched_directory, LOGS_DIR, "rendermanager.log"))
    log.info("--- starting logging ---")

    # Print version information.
    log.info("running rendermanager.py version {0}.".format(VERSION))

    # Main management loop.
    try:
        while True:
            try:
                manage(args, log)
            except KeyboardInterrupt, SystemExit:
                raise
            except:
                exc_type, exc_value, exc_traceback = sys.exc_info()
                lines = traceback.format_exception(exc_type, exc_value, exc_traceback)
                log.error("".join(line for line in lines))
            time.sleep(PAUSE_BETWEEN_CHECKS)
    except KeyboardInterrupt, SystemExit:
        pass

    log.info("exiting...")

if __name__ == '__main__':
    main()
