#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

# Settings.
VersionString = "1.5"
Verbose = False
DumpStrippedInputFiles = False
StrippedInputFilesDirectory = "stripped/"

# Imports.
import glob
import os
import re
from subprocess import call
import sys
from xml.etree.ElementTree import ElementTree


#
# Utility functions.
#

NEWLINE_REGEX = r"[\r\n]+"

def info(message):
    print("INFO: {0}".format(message))

def warning(message):
    print("WARNING: {0}".format(message))

def fatal(message):
    print("\nFATAL: {0}, aborting.".format(message))
    sys.exit(1)

def safe_make_directory(path):
    if not os.path.isdir(path):
        os.makedirs(path)

def back_to_forward_slashes(path):
    return path.replace('\\', '/')

def make_relative_path(file_path, base):
    npath = os.path.normpath(os.path.normcase(file_path))
    nbase = os.path.normpath(os.path.normcase(base))
    if npath.startswith(nbase):
        result = npath[len(nbase):]
        if result.startswith("/") or result.startswith("\\"):
            result = result[1:]
        return result
    else:
        return file_path

def stable_unique(seq):
    seen = {}
    result = []
    for item in seq:
        if item in seen: continue
        seen[item] = 1
        result.append(item)
    return result

def load_file(file_path):
    try:
       file = open(file_path, "r")
       text = file.read()
       file.close()
       return text
    except IOError:
       fatal("failed to load file '" + file_path + "'")

def write_file(file_path, text):
    try:
       file = open(file_path, "w")
       file.write(text)
       file.close()
    except IOError:
       fatal("failed to write file '" + file_path + "'")


#
# Manifest class.
#

class Manifest:
    def __init__(self, manifest_path):
        print("Loading manifest file '" + manifest_path + "'...")

        # Parse the manifest file.
        try:
            tree = ElementTree()
            tree.parse(manifest_path)
        except IOError:
            fatal("failed to load manifest file '" + manifest_path + "'")

        # Extract the root path of the input files.
        self.input_root_path = tree.find("input").attrib["root"]

        # Collect the input files.
        self.input_file_paths = []
        for files_element in tree.findall("input/files"):
           file_mask = os.path.join(self.input_root_path, files_element.text)
           self.input_file_paths.extend(glob.glob(file_mask))
        self.input_file_paths = map(back_to_forward_slashes, self.input_file_paths)

        # Extract the output base path and base filename.
        self.output_base_path = tree.find("output/base_path").text
        self.output_base_filename = tree.find("output/base_filename").text

        # Extract the header text.
        self.header = tree.find("output/header").text

        # Extract the additional strings to strip.
        self.literals_to_strip = []
        self.regexes_to_strip = []
        for strip_element in tree.findall("output/strip"):
            if strip_element.attrib["type"] == "literal":
                self.literals_to_strip.append(strip_element.text)
            else:
                self.regexes_to_strip.append(strip_element.text)

        # Extract the command line to test-compile the output source file.
        self.test_command_lines = map(lambda e: e.text, tree.findall("test/commandline"))

    def get_output_header_file_path(self):
        return os.path.join(self.output_base_path, self.output_base_filename + ".h")

    def get_output_source_file_path(self):
        return os.path.join(self.output_base_path, self.output_base_filename + ".cpp")

    def strip(self, text):
        text = self.__strip_literals(text)
        text = self.__strip_regexes(text)
        return text

    def __strip_literals(self, text):
        for literal in self.literals_to_strip:
            text = text.replace(literal, "")
        return text

    def __strip_regexes(self, text):
        for regex in self.regexes_to_strip:
            text = re.sub(regex, "", text, 0, re.MULTILINE)
        return text


#
# DependencyFinder class.
#

class DependencyFinder:
    def __init__(self, manifest):
        self.manifest = manifest

        if DumpStrippedInputFiles:
            safe_make_directory(StrippedInputFilesDirectory)

    def gather_all_user_header_files(self):
        files = []

        for input_file_path in self.manifest.input_file_paths:
            if os.path.splitext(input_file_path)[1].lower() == ".h":
                files.extend(self.__gather_all_deps(input_file_path, r"\"", r"\"", 0))

        return stable_unique(files)

    def gather_all_user_source_files(self):
        files = []

        for file_path in self.manifest.input_file_paths:
            if os.path.splitext(file_path)[1].lower() == ".cpp":
                files.append(file_path)

        return files

    def gather_all_platform_header_files(self, header_files):
        files = []

        for file_path in header_files:
            files.extend(self.__gather_local_deps(file_path, r"<", r">"))

        files.sort()

        return stable_unique(files)

    def __gather_all_deps(self, file_path, opening_marker, closing_marker, level):
        if Verbose:
            print("{0}Including {1}".format(" " * level * 2, file_path))

        files = []

        # First gather all the dependencies of this file.
        for child_dep in self.__gather_local_deps(file_path, opening_marker, closing_marker):
            child_file_path = os.path.join(self.manifest.input_root_path, child_dep)
            files.extend(self.__gather_all_deps(child_file_path, opening_marker, closing_marker, level + 1))

        # Then append this file to the list.
        files.append(back_to_forward_slashes(file_path))

        return files

    def __gather_local_deps(self, file_path, opening_marker, closing_marker):
        text = load_file(file_path)
        text = self.manifest.strip(text)

        if DumpStrippedInputFiles:
            write_file(os.path.join(StrippedInputFilesDirectory, os.path.basename(file_path) + ".stripped"), text)

        deps = []
        pattern = re.compile(r"^#include " + opening_marker +
                             r"(?P<include_path>[^" + closing_marker + r"]*)" +
                             closing_marker + r"[^$]*$")

        for line in text.splitlines():
            match = pattern.search(line)
            if match:
                deps.append(match.group("include_path"))

        return deps


#
# FileGenerator class.
#

class FileGenerator:
    def __init__(self, manifest, depfinder):
        self.manifest = manifest
        self.depfinder = depfinder

    def generate_output_header_file(self):
        output_file_path = self.manifest.get_output_header_file_path()
        print("Generating '" + output_file_path + "'...")

        header_files = self.depfinder.gather_all_user_header_files()

        # Report unexpected dependencies.
        self.__report_unexpected_deps(header_files)

        # Open file for writing.
        safe_make_directory(self.manifest.output_base_path)
        output_file = open(output_file_path, "w")

        # Write the file header.
        output_file.write(self.manifest.header)

        # Generate and write the header guard.
        header_guard_token = os.path.basename(output_file_path).replace("/", "_").replace("\\", "_").replace(".", "_").upper()
        output_file.write("#ifndef " + header_guard_token + "\n")
        output_file.write("#define " + header_guard_token + "\n")

        # Include all required platform headers.
        # self.__write_platform_headers(output_file, self.depfinder.gather_all_platform_header_files(header_files))

        # Concatenate the content of all header files.
        for header_file_path in header_files:
            self.__write_file(output_file, header_file_path)

        # Close the header guard.
        output_file.write("\n#endif  // !" + header_guard_token + "\n")

        output_file.close()

    def generate_output_source_file(self):
        output_file_path = self.manifest.get_output_source_file_path()
        print("Generating '" + output_file_path + "'...")

        source_files = self.depfinder.gather_all_user_source_files()

        # Open file for writing.
        safe_make_directory(self.manifest.output_base_path)
        output_file = open(output_file_path, "w")

        # Write the file header.
        output_file.write(self.manifest.header)

        # Include the interface header file.
        output_file.write("// Interface header.\n")
        output_file.write("#include \"" + self.manifest.output_base_filename + ".h\"\n")

        # Include all required platform headers.
        # self.__write_platform_headers(output_file, self.depfinder.gather_all_platform_header_files(source_files))

        # Concatenate the content of all source files.
        for source_file_path in source_files:
            self.__write_file(output_file, source_file_path)

        output_file.close()

    def __report_unexpected_deps(self, files):
        for file in files:
            if file not in self.manifest.input_file_paths:
                warning("unexpected dependency: {0}".format(file))

    def __write_platform_headers(self, output_file, headers):
        if len(headers) > 0:
            output_file.write("\n// Standard and platform headers.\n")
            for header in headers:
                output_file.write("#include <" + header + ">\n")

    def __write_file(self, output_file, input_file_path):
        text = load_file(input_file_path)
        text = self.manifest.strip(text)

        # text = self.__strip_deps(text)
        text = self.__strip_user_deps(text)

        text = text.strip("\n")

        if len(text) == 0:
            info("file {0} is empty after stripping and will be omitted from the output.".format(input_file_path))
            return

        text = text + "\n"

        relative_path = make_relative_path(input_file_path, self.manifest.input_root_path)
        relative_path = back_to_forward_slashes(relative_path)

        # Emit a block comment containing the name of the input file.
        Width = 100
        Separator = "//{0}\n".format("-" * (Width - 2))
        output_file.write("\n\n")
        output_file.write(Separator)
        output_file.write("// {0}{1}\n".format(relative_path, " " * (Width - len(relative_path) - 3)))
        output_file.write(Separator)
        output_file.write("\n")

        output_file.write(text)

    def __strip_user_deps(self, text):
        return self.__strip_deps(text, r"\"", r"\"")

    def __strip_platform_deps(self, text):
        return self.__strip_deps(text, r"<", r">")

    def __strip_deps(self, text, opening_marker, closing_marker):
        return re.sub(r"^#include " + opening_marker +
                      r"[^" + closing_marker + r"]*" +
                      closing_marker + ".*" + NEWLINE_REGEX, "", text, 0, re.MULTILINE)


#
# Tester class.
#

class Tester:
    def __init__(self, manifest):
        self.manifest = manifest

    def compile(self):
        print("Test-compiling...")
        for command_line in self.manifest.test_command_lines:
            command_line = command_line.replace("$OutputBasePath", self.manifest.output_base_path)
            command_line = command_line.replace("$OutputHeaderFilePath", self.manifest.get_output_header_file_path())
            command_line = command_line.replace("$OutputSourceFilePath", self.manifest.get_output_source_file_path())
            call(command_line, shell=True)


#
# Entry point.
#

def main():
    print("appleseed.gather version " + VersionString)

    if len(sys.argv) < 2:
        print("Usage: " + sys.argv[0] + " manifest.xml")
        sys.exit(1)

    manifest = Manifest(sys.argv[1])
    depfinder = DependencyFinder(manifest)

    filegen = FileGenerator(manifest, depfinder)
    filegen.generate_output_header_file()
    filegen.generate_output_source_file()

    if len(manifest.test_command_lines) > 0:
        tester = Tester(manifest)
        tester.compile()

main()
