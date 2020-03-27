#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013 Francois Beaune, Jupiter Jazz Limited
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

from utils import print_runtime_details


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "2.9"
RENDERS_DIR = "_renders"
ARCHIVES_DIR = "_archives"
LOGS_DIR = "_logs"
PAUSE_BETWEEN_UPDATES = 60   # in seconds
MB = 1024 * 1024


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

def safe_get_file_size(filepath):
    try:
        return os.path.getsize(filepath)
    except:
        return 0


def get_directory_size(directory):
    size = 0
    for dirpath, dirnames, filenames in os.walk(directory):
        for filename in filenames:
            filepath = os.path.join(dirpath, filename)
            size += safe_get_file_size(filepath)
    return size


def get_files(directory, pattern="*"):
    files = []
    for file in glob.glob(os.path.join(directory, pattern)):
        files.append(file)
    return files


def safe_mkdir(dir):
    if not os.path.exists(dir):
        os.makedirs(dir)


def convert_path_to_local(path):
    if os.name == "nt":
        return path.replace('/', '\\')
    else:
        return path.replace('\\', '/')


def tail_file(f, window=20):
    """
    Returns the last `window` lines of file `f` as a list.
    Based on code from http://stackoverflow.com/a/7047765/393756.
    """

    BUFFER_SIZE = 1024

    f.seek(0, 2)
    bytes = f.tell()

    size = window + 1
    block = -1
    data = []

    while size > 0 and bytes > 0:
        if bytes > BUFFER_SIZE:
            # Seek back one whole block of size BUFFER_SIZE.
            f.seek(block * BUFFER_SIZE, 2)
            # Read one block.
            data.insert(0, f.read(BUFFER_SIZE))
        else:
            # File too small, start from begining.
            f.seek(0, 0)
            # Only read what was not read.
            data.insert(0, f.read(bytes))

        lines_found = data[0].count('\n')
        size -= lines_found
        bytes -= BUFFER_SIZE
        block -= 1

    return "".join(data).splitlines()[-window:]


def format_message(severity, msg):
    now = datetime.datetime.now()
    timestamp = now.strftime("%Y-%m-%d %H:%M:%S.%f")
    padded_severity = severity.ljust(7)
    return "\n".join("{0} mgr   {1} | {2}".format(timestamp, padded_severity, line)
                     for line in msg.splitlines())


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
# Dependency database.
# -------------------------------------------------------------------------------------------------

class DependencyDB:

    def __init__(self, source_directory, log):
        self.source_directory = source_directory
        self.log = log
        self.roots = {}

    def update(self, new_roots):
        for root in new_roots:
            if not root in self.roots:
                success, deps = self.__extract_dependencies(root)
                if success:
                    self.roots[root] = deps
                    self.log.info("  added {0}".format(root))

        updated_roots = {}

        for root in self.roots:
            if root in new_roots:
                updated_roots[root] = self.roots[root]
            else:
                self.log.info("  removed {0}".format(root))

        self.roots = updated_roots

    def get_all_dependencies(self):
        deps = set()
        for root in self.roots:
            deps = deps.union(self.roots[root])
        return deps

    def __extract_dependencies(self, filename):
        try:
            filepath = os.path.join(self.source_directory, filename)

            with open(filepath, 'r') as file:
                contents = file.read()

            xmldoc = xml.parseString(contents)
            deps = set()

            for node in xmldoc.getElementsByTagName('parameter'):
                if node.getAttribute('name') == 'filename':
                    deps.add(convert_path_to_local(node.getAttribute('value')))

            for node in xmldoc.getElementsByTagName('parameters'):
                if node.getAttribute('name') == 'filename':
                    for child in node.childNodes:
                        if child.nodeType == xml.Node.ELEMENT_NODE:
                            deps.add(convert_path_to_local(child.getAttribute('value')))

            return True, deps

        except KeyboardInterrupt, SystemExit:
            raise

        except:
            return False, set()


# -------------------------------------------------------------------------------------------------
# Management logic.
# -------------------------------------------------------------------------------------------------

class Manager:

    def __init__(self, args, log):
        self.args = args
        self.log = log
        self.frames_directory = os.path.join(self.args.target_directory, RENDERS_DIR)
        self.archives_directory = os.path.join(self.args.target_directory, ARCHIVES_DIR)
        self.all_uploaded_dependency_db = DependencyDB(self.args.target_directory, log)
        self.own_uploaded_dependency_db = DependencyDB(self.args.source_directory, log)
        self.completed_dependency_db = DependencyDB(self.args.source_directory, log)

    def manage(self):
        self.compute_target_directory_size()
        self.gather_files()
        self.print_status()
        if self.args.frames_directory is not None:
            self.move_frames()
        self.update_dependency_dbs()
        self.remove_orphan_dependencies()
        self.upload_project_files()
        self.upload_missing_dependencies()

    def compute_target_directory_size(self):
        self.target_directory_size = get_directory_size(self.args.target_directory)

    def gather_files(self):
        self.log.info("gathering files...")
        self.source_files = map(os.path.basename, get_files(self.args.source_directory, "*.appleseed"))
        self.uploaded_files = self.gather_uploaded_files()
        self.inprogress_files = self.gather_inprogress_files()
        self.completed_files = map(os.path.basename, get_files(self.archives_directory, "*.appleseed"))
        self.log.info("  found {0} source files (this shot) in {1}".format(len(self.source_files), self.args.source_directory))
        self.log.info("  found {0} uploaded files (all shots) in {1}".format(len(self.uploaded_files), self.args.target_directory))
        self.log.info("  found {0} in-progress files (all shots) in {1}".format(len(self.inprogress_files), self.args.target_directory))
        self.log.info("  found {0} completed files (all shots) in {1}".format(len(self.completed_files), self.archives_directory))

    def gather_uploaded_files(self):
        return map(os.path.basename, get_files(self.args.target_directory, "*.appleseed"))

    def gather_inprogress_files(self):
        inprogress = {}
        for filename in map(os.path.basename, get_files(self.args.target_directory, "*.appleseed.*")):
            parts = filename.split(".")
            assert len(parts) >= 3
            if parts[-2] == "appleseed":
                owner = parts[-1]
                stripped_filename = filename[:-(1 + len(owner))]
                inprogress.setdefault(stripped_filename, []).append(owner)
        return inprogress

    def print_status(self):
        self.log.info("-------------------------------------------------------------------")
        self.print_progress()
        self.print_assignments()
        self.print_pings()
        self.print_target_directory_size()
        self.log.info("-------------------------------------------------------------------")

    def print_progress(self):
        total = len(self.source_files)
        completed = self.count_completed_frames()
        rendering = self.count_inprogress_frames()
        pending = self.count_pending_frames()
        progress = 100.0 * completed / total if total > 0 else 0.0
        self.log.info("PROGRESS: {0}/{1} completed ({2:.2f} %), {3} rendering, {4} pending"
                      .format(completed, total, progress, rendering, pending))

    def print_assignments(self):
        assignments = {}
        for filename in self.source_files:
            if filename in self.inprogress_files.keys():
                assignments[filename] = ", ".join(self.inprogress_files[filename])
        if len(assignments) > 0:
            self.log.info("frame assignments:")
            for filename in assignments.keys():
                self.log.info("  {0}: {1}".format(filename, assignments[filename]))
        else:
            self.log.info("no frame assigned.")

    def print_pings(self):
        owners = set()
        for filename in self.source_files:
            if filename in self.inprogress_files.keys():
                for owner in self.inprogress_files[filename]:
                    owners.add(owner)
        unsorted_pings = [(owner, self.read_ping(owner)) for owner in owners]
        filtered_pings = [x for x in unsorted_pings if x[1] is not None]
        pings = sorted(filtered_pings, key=lambda x: x[1])
        if len(pings) > 0:
            max_owner_length = max([len(owner) for owner in owners])
            self.log.info("pings:")
            for (owner, ping) in pings:
                padding = " " * (max_owner_length + 1 - len(owner))
                self.log.info("  {0}:{1}{2}".format(owner, padding, self.format_ping(ping) if ping is not None else "n/a"))
        else:
            self.log.info("no pings.")

    def read_ping(self, owner):
        TIMESTAMP_LENGTH = 26
        try:
            with open(os.path.join(self.args.target_directory, LOGS_DIR, owner + ".log")) as file:
                last_line = tail_file(file, 1)[0]
                return datetime.datetime.strptime(last_line[:TIMESTAMP_LENGTH], "%Y-%m-%d %H:%M:%S.%f")
        except IOError as ex:
            return None

    def format_ping(self, ping):
        elapsed = datetime.datetime.now() - ping
        return "{0} ago (at {1})".format(elapsed, ping)

    def print_target_directory_size(self):
        size_mb = self.target_directory_size / MB
        max_size_mb = self.args.max_size / MB
        full = 100.0 * size_mb / max_size_mb if max_size_mb > 0 else 100.0
        self.log.info("size of target directory: {0:.2f}/{1} mb ({2:.2f} % full)"
                      .format(size_mb, max_size_mb, full))

    def count_completed_frames(self):
        return sum(1 for filename in self.source_files if filename in self.completed_files)

    def count_inprogress_frames(self):
        return sum(1 for filename in self.source_files if filename in self.inprogress_files)

    def count_pending_frames(self):
        return sum(1 for filename in self.source_files
                   if not filename in self.completed_files and not filename in self.inprogress_files)

    def move_frames(self):
        self.log.info("moving frames...")
        for filepath in get_files(self.frames_directory):
            self.move_frame(filepath)

    def move_frame(self, source_filepath):
        filename = os.path.basename(source_filepath)
        dest_filepath = os.path.join(self.args.frames_directory, filename)
        self.log.info("  moving {0}".format(filename))
        safe_mkdir(self.args.frames_directory)
        shutil.move(source_filepath, dest_filepath)

    def update_dependency_dbs(self):
        self.update_uploaded_dependency_db()
        self.update_completed_dependency_db()

    def update_uploaded_dependency_db(self):
        self.log.info("updating dependency database of uploaded and in-progress files (all shots)...")
        all_roots = map(os.path.basename, get_files(self.args.target_directory, "*.appleseed*"))
        self.all_uploaded_dependency_db.update(all_roots)

        self.log.info("updating dependency database of uploaded files (this shot)...")
        own_roots = [filename for filename in self.source_files
                     if filename in self.inprogress_files or filename in self.uploaded_files]
        self.own_uploaded_dependency_db.update(own_roots)

    def update_completed_dependency_db(self):
        self.log.info("updating dependency database of completed files (this shot)...")
        roots = [filename for filename in self.source_files if filename in self.completed_files]
        self.completed_dependency_db.update(roots)

    def remove_orphan_dependencies(self):
        self.log.info("removing orphan dependencies...")
        removed = 0
        all_uploaded_files_dependencies = self.all_uploaded_dependency_db.get_all_dependencies()
        for dep in self.completed_dependency_db.get_all_dependencies():
            if not dep in all_uploaded_files_dependencies:
                count = self.remove_file(dep)
                if count > 0:
                    self.log.info("  removed {0}".format(dep))
                removed += count
        if removed > 0:
            self.log.info("  removed {0} dependencies".format(removed))

    def upload_project_files(self):
        self.log.info("uploading project files...")
        for filename in self.source_files:
            if not filename in self.inprogress_files and not filename in self.completed_files:
                if self.upload_file(filename) > 0:
                    self.log.info("  uploaded {0}".format(filename))
                    self.uploaded_files = self.gather_uploaded_files()
                    self.update_uploaded_dependency_db()
                    self.upload_missing_dependencies()

    def upload_missing_dependencies(self):
        self.log.info("uploading missing dependencies...")
        uploaded = 0
        for dep in self.own_uploaded_dependency_db.get_all_dependencies():
            count = self.upload_file(dep)
            if count > 0:
                self.log.info("  uploaded {0}".format(dep))
            uploaded += count
        if uploaded > 0:
            self.log.info("  uploaded {0} dependencies".format(uploaded))

    def remove_file(self, filename):
        filepath = os.path.join(self.args.target_directory, filename)
        if not os.path.isfile(filepath):
            return 0

        try:
            filesize = safe_get_file_size(filepath)
            os.remove(filepath)
            self.target_directory_size = max(self.target_directory_size - filesize, 0)
            return 1
        except IOError as ex:
            self.log.error("  could not remove {0}: {1}".format(filepath, ex.strerror))
            return 0

    def upload_file(self, filename):
        dest_filepath = os.path.join(self.args.target_directory, filename)
        if os.path.isfile(dest_filepath):
            return 0

        source_filepath = os.path.join(self.args.source_directory, filename)
        filesize = safe_get_file_size(source_filepath)
        if self.target_directory_size + filesize > self.args.max_size:
            return 0

        try:
            safe_mkdir(os.path.dirname(dest_filepath))
            shutil.copyfile(source_filepath, dest_filepath)
            self.target_directory_size += filesize
            return 1
        except IOError as ex:
            self.log.error("  could not upload {0}: {1}".format(source_filepath, ex.strerror))
            return 0


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    # Parse the command line.
    parser = argparse.ArgumentParser(description="send a shot to a folder being watched by "
                                     "appleseed render nodes.")
    parser.add_argument("-s", "--max-size", metavar="MB",
                        help="set the maximum allowed size in mb of the target directory "
                        "(default is 1 terabyte)")
    parser.add_argument("--source", metavar="source-directory", dest="source_directory",
                        required=True, help="directory containing the source shot data")
    parser.add_argument("--target", metavar="target-directory", dest="target_directory",
                        required=True, help="directory being watched by render nodes")
    parser.add_argument("--frames", metavar="frames-directory", dest="frames_directory",
                        help="directory where the rendered frames should be stored")
    args = parser.parse_args()

    print_runtime_details("rendermanager", VERSION, os.path.realpath(__file__))

    if args.max_size is None:
        args.max_size = 2 ** 40                 # default to 1 terabyte
    else:
        args.max_size = long(args.max_size)
        args.max_size *= MB                     # convert to bytes

    # Start the log.
    log = Log(os.path.join(args.target_directory, LOGS_DIR, "rendermanager.log"))
    log.info("--- starting logging ---")
    log.info("running rendermanager.py version {0}.".format(VERSION))

    manager = Manager(args, log)

    # Main management loop.
    try:
        while True:
            try:
                manager.manage()
            except KeyboardInterrupt, SystemExit:
                raise
            except:
                exc_type, exc_value, exc_traceback = sys.exc_info()
                lines = traceback.format_exception(exc_type, exc_value, exc_traceback)
                log.error("".join(line for line in lines))

            log.info_no_log("waiting {0} seconds...".format(PAUSE_BETWEEN_UPDATES))
            time.sleep(PAUSE_BETWEEN_UPDATES)

    except KeyboardInterrupt, SystemExit:
        pass

    log.info("exiting...")


if __name__ == "__main__":
    main()
