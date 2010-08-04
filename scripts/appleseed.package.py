#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010 Francois Beaune
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

# Package builder settings.
VersionString = "1.0"
SettingsFileName = "appleseed.package.configuration.xml"

# Imports.
from xml.etree.ElementTree import ElementTree
from distutils import archive_util, dir_util
import glob
import os
import platform
import shutil
import sys
import zipfile


#
# Utility functions.
#

def progress(message):
    print("  " + message + "...")

def fatal(message):
    print("\n  FATAL: " + message + ", aborting.")
    sys.exit(1)

def safe_delete_file(path):
    try:
        if os.path.exists(path):
            os.remove(path)
    except OSError:
        fatal("failed to delete file '" + path + "'")

def safe_delete_directory(path):
    try:
        if os.path.exists(path):
            shutil.rmtree(path)
    except OSError:
        fatal("failed to delete directory '" + path + "'")

def safe_make_directory(path):
    if not os.path.exists(path):
        os.mkdir(path)

def pushd(path):
    current_path = os.getcwd()
    os.chdir(path)
    return current_path

def extract_zip_file(zip_path, output_path):
    zf = zipfile.ZipFile(zip_path)
    zf.extractall(output_path)
    zf.close()

def copy_glob(input_pattern, output_path):
    for input_file in glob.glob(input_pattern):
        shutil.copy(input_file, output_path)


#
# Settings parser.
#

class Settings:
    def __init__(self):
        print "Loading settings from " + SettingsFileName + "..."
        self.load()
        self.complete()
        self.print_summary()

    def load(self):
        tree = ElementTree()
        try:
            tree.parse(SettingsFileName)
        except IOError:
            fatal("failed to load configuration file " + SettingsFileName)
        self.load_values(tree)

    def load_values(self, tree):
        self.configuration = tree.findtext("configuration")
        self.platform = tree.findtext("platform")
        self.appleseed_path = tree.findtext("appleseed_path")
        self.qt_path = tree.findtext("qt_path")
        self.platform_runtime_path = tree.findtext("platform_runtime_path")
        self.package_output_path = tree.findtext("package_output_path")

    def complete(self):
        self.package_filename = "appleseed-VERSION-MATURITY-" + self.platform + ".zip"
        self.package_filepath = os.path.join(self.package_output_path, self.package_filename)

    def print_summary(self):
        print "\nSettings:\n"
        print "  Configuration:             " + self.configuration
        print "  Platform:                  " + self.platform + " (Python says " + os.name + ")"
        print "  Path to appleseed:         " + self.appleseed_path
        print "  Path to Qt libraries:      " + self.qt_path
        print "  Path to platform runtime:  " + self.platform_runtime_path
        print "  Output directory:          " + self.package_output_path
        print "  Output file path:          " + self.package_filepath


#
# Base package builder.
#

class PackageBuilder:
    def __init__(self, settings):
        self.settings = settings

    def build_package(self):
        print "\nBuilding " + self.settings.package_filename + ":\n"
        self.remove_leftovers()
        self.retrieve_sandbox_from_git_repository()
        self.deploy_sandbox_to_stage()
        self.cleanup_stage()
        self.add_local_binaries_to_stage()
        self.add_local_schema_files_to_stage()
        self.add_text_files_to_stage()
        self.add_dummy_files_into_empty_directories()
        self.alterate_stage()
        self.build_final_zip_file()
        self.remove_stage()
        print "\nThe package was successfully created."

    def alterate_stage(self):
        return

    def remove_leftovers(self):
        progress("Removing leftovers from previous invocations")
        safe_delete_directory("appleseed")
        safe_delete_file("sandbox.zip")
        safe_delete_file(self.settings.package_filepath)

    def retrieve_sandbox_from_git_repository(self):
        progress("Retrieving sandbox from Git repository")
        current_path = pushd(os.path.join(self.settings.appleseed_path, "sandbox"))
        os.system("git archive --format=zip --output=" + os.path.join(current_path, "sandbox.zip") + " --worktree-attributes HEAD")
        os.chdir(current_path)

    def deploy_sandbox_to_stage(self):
        progress("Deploying sandbox to staging directory")
        extract_zip_file("sandbox.zip", "appleseed/")
        safe_delete_file("sandbox.zip")

    def cleanup_stage(self):
        progress("Cleaning up staging directory")
        safe_delete_directory("appleseed/scenes/cyberdemon")
        safe_delete_directory("appleseed/scenes/smoke")
        safe_delete_directory("appleseed/scenes/tests/invalid")
        safe_delete_directory("appleseed/scenes/tests/scope")
        safe_delete_directory("appleseed/scenes/tests/self intersections")
        safe_delete_file("appleseed/scenes/killeroo/killeroo ao.appleseed")
        safe_delete_file("appleseed/scenes/killeroo/killeroo ao close up.appleseed")
        safe_delete_directory("appleseed/scenes/winosi/renders")

    def add_local_binaries_to_stage(self):
        progress("Adding local binaries to staging directory")
        safe_make_directory("appleseed/bin")
        dir_util.copy_tree(os.path.join(self.settings.appleseed_path, "sandbox/bin", self.settings.configuration), "appleseed/bin/")

    def add_local_schema_files_to_stage(self):
        progress("Adding local schema files to staging directory")
        safe_make_directory("appleseed/schemas")
        copy_glob(os.path.join(self.settings.appleseed_path, "sandbox/schemas/*.xsd"), "appleseed/schemas/")

    def add_text_files_to_stage(self):
        progress("Adding LICENSE.txt and README.txt files")
        shutil.copy(os.path.join(self.settings.appleseed_path, "LICENSE.txt"), "appleseed/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "README.txt"), "appleseed/")

    def build_final_zip_file(self):
        progress("Building final zip file from staging directory")
        package_base_filepath = os.path.splitext(self.settings.package_filepath)[0]
        archive_util.make_zipfile(package_base_filepath, "appleseed")

    def create_preserve_file(self, path):
        f = open(os.path.join(path, "preserve.txt"), "w")
        f.write("This file allows to preserve this otherwise empty directory.\n")
        f.close()

    def add_dummy_files_into_empty_directories(self):
        progress("Adding dummy files to preserve empty directories")
        for dirpath, dirnames, filenames in os.walk("."):
            if len(dirnames) == 0 and len(filenames) == 0:
                self.create_preserve_file(dirpath)

    def remove_stage(self):
        safe_delete_directory("appleseed")


#
# Windows package builder.
#

class WindowsPackageBuilder(PackageBuilder):
    def alterate_stage(self):
        self.add_dependencies_to_stage()

    def add_dependencies_to_stage(self):
        progress("Windows-specific: adding dependencies to staging directory")
        shutil.copy(os.path.join(self.settings.qt_path, "lib/QtCore4.dll"), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.qt_path, "lib/QtGui4.dll"), "appleseed/bin/")
        dir_util.copy_tree(self.settings.platform_runtime_path, "appleseed/bin/Microsoft.VC90.CRT")


#
# Mac package builder.
#

class MacPackageBuilder(PackageBuilder):
    def alterate_stage(self):
        self.fixup_binaries()

    def fixup_binaries(self):
        progress("Mac-specific: fixing up binaries")
        self.fixup_libappleseed()
        self.fixup_libappleseed_shared()
        self.fixup_appleseed_cli()
        self.fixup_appleseed_studio()

    def fixup_libappleseed(self):
        self.fixup_id("libappleseed.dylib")

    def fixup_libappleseed_shared(self):
        self.fixup_id("libappleseed.shared.dylib")
        self.fixup_change("libappleseed.shared.dylib", os.path.join(self.settings.appleseed_path, "build/src/appleseed/libappleseed.dylib"), "libappleseed.dylib")

    def fixup_appleseed_cli(self):
        self.fixup_change("appleseed.cli", os.path.join(self.settings.appleseed_path, "build/src/appleseed/libappleseed.dylib"), "libappleseed.dylib")
        self.fixup_change("appleseed.cli", os.path.join(self.settings.appleseed_path, "build/src/appleseed.shared/libappleseed.shared.dylib"), "libappleseed.shared.dylib")

    def fixup_appleseed_studio(self):
        self.fixup_change("appleseed.studio", os.path.join(self.settings.appleseed_path, "build/src/appleseed/libappleseed.dylib"), "libappleseed.dylib")
        self.fixup_change("appleseed.studio", os.path.join(self.settings.appleseed_path, "build/src/appleseed.shared/libappleseed.shared.dylib"), "libappleseed.shared.dylib")

    def fixup_id(self, target):
        self.fixup(target, '-id @"' + target + '"')

    def fixup_change(self, target, old, new):
        self.fixup(target, '-change "' + old + '" "' + new + '"')

    def fixup(self, target, args):
        os.system("install_name_tool " + args + " " + os.path.join("appleseed/bin/", target))


#
# Entry point.
#

def main():
    print "appleseed.package version " + VersionString

    settings = Settings()

    if os.name == "nt":
        package_builder = WindowsPackageBuilder(settings)
    elif os.name == "posix" and platform.mac_ver()[0] != "":
        package_builder = MacPackageBuilder(settings)
    else:
        fatal("unsupported platform")

    package_builder.build_package()

main()
