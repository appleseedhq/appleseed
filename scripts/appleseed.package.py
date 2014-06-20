#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

# Package builder settings.
VersionString = "2.3.9"
SettingsFileName = "appleseed.package.configuration.xml"

# Imports.
from xml.etree.ElementTree import ElementTree
from distutils import archive_util, dir_util
from stat import *
from subprocess import *
import glob
import os
import platform
import shutil
import sys
import zipfile


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

def info(message):
    print("  " + message)

def progress(message):
    print("  " + message + "...")

def fatal(message):
    print
    print("FATAL: " + message + ", aborting.")
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
    if not os.path.isdir(path):
        os.makedirs(path)

def pushd(path):
    old_path = os.getcwd()
    os.chdir(path)
    return old_path

def extract_zip_file(zip_path, output_path):
    zf = zipfile.ZipFile(zip_path)
    zf.extractall(output_path)
    zf.close()

def copy_glob(input_pattern, output_path):
    for input_file in glob.glob(input_pattern):
        shutil.copy(input_file, output_path)


#--------------------------------------------------------------------------------------------------
# Settings.
#--------------------------------------------------------------------------------------------------

class Settings:
    def load(self):
        print "Loading settings from " + SettingsFileName + "..."
        tree = ElementTree()
        try:
            tree.parse(SettingsFileName)
        except IOError:
            fatal("failed to load configuration file '" + SettingsFileName + "'")
        self.load_values(tree)
        self.print_summary()

    def load_values(self, tree):
        self.configuration = tree.findtext("configuration")
        self.platform_id = tree.findtext("platform_id")
        self.platform_name = tree.findtext("platform_name")
        self.appleseed_path = tree.findtext("appleseed_path")
        self.qt_runtime_path = tree.findtext("qt_runtime_path")
        self.platform_runtime_path = tree.findtext("platform_runtime_path")
        self.package_output_path = tree.findtext("package_output_path")

    def print_summary(self):
        print
        print "  Configuration:             " + self.configuration
        print "  Platform ID:               " + self.platform_id + " (Python says " + os.name + ")"
        print "  Platform Name:             " + self.platform_name
        print "  Path to appleseed:         " + self.appleseed_path
        print "  Path to Qt runtime:        " + self.qt_runtime_path
        print "  Path to platform runtime:  " + self.platform_runtime_path
        print "  Output directory:          " + self.package_output_path
        print


#--------------------------------------------------------------------------------------------------
# Package information.
#--------------------------------------------------------------------------------------------------

class PackageInfo:
    def __init__(self, settings):
        self.settings = settings

    def load(self):
        print "Loading package information..."
        self.retrieve_git_tag()
        self.build_package_path()
        self.print_summary()

    def retrieve_git_tag(self):
        old_path = pushd(self.settings.appleseed_path)
        self.version = Popen("git describe --long", stdout=PIPE, shell=True).stdout.read().strip()
        os.chdir(old_path)

    def build_package_path(self):
        package_name = "appleseed-" + self.version + "-" + self.settings.platform_name + ".zip"
        self.package_path = os.path.join(self.settings.package_output_path, self.version, package_name)

    def print_summary(self):
        print
        print "  Version:                   " + self.version
        print "  Package path:              " + self.package_path
        print


#--------------------------------------------------------------------------------------------------
# Base package builder.
#--------------------------------------------------------------------------------------------------

class PackageBuilder:
    def __init__(self, settings, package_info):
        self.settings = settings
        self.package_info = package_info

    def build_package(self):
        print "Building package:"
        print
        self.orchestrate()
        print
        print "The package was successfully built."

    def orchestrate(self):
        self.remove_leftovers()
        self.retrieve_sandbox_from_git_repository()
        self.deploy_sandbox_to_stage()
        self.cleanup_stage()
        self.add_local_binaries_to_stage()
        self.add_scripts_to_stage()
        self.add_local_schema_files_to_stage()
        self.add_text_files_to_stage()
        self.add_dummy_files_into_empty_directories()
        self.alterate_stage()
        self.build_final_zip_file()
        self.remove_stage()

    def remove_leftovers(self):
        progress("Removing leftovers from previous invocations")
        safe_delete_directory("appleseed")
        safe_delete_file("sandbox.zip")
        safe_delete_file(self.package_info.package_path)

    def retrieve_sandbox_from_git_repository(self):
        progress("Retrieving sandbox from Git repository")
        old_path = pushd(os.path.join(self.settings.appleseed_path, "sandbox"))
        self.run("git archive --format=zip --output=" + os.path.join(old_path, "sandbox.zip") + " --worktree-attributes HEAD")
        os.chdir(old_path)

    def deploy_sandbox_to_stage(self):
        progress("Deploying sandbox to staging directory")
        extract_zip_file("sandbox.zip", "appleseed/")
        safe_delete_file("sandbox.zip")

    def cleanup_stage(self):
        progress("Cleaning up staging directory")

        # Remove API reference documentation.
        safe_delete_directory("appleseed/documentation/apireference")

        # Remove the test suite.
        safe_delete_directory("appleseed/tests/test scenes")

        # Remove voluminous unit tests/benchmarks data.
        safe_delete_file("appleseed/tests/unit benchmarks/inputs/test_knn_particles.bin")
        safe_delete_file("appleseed/tests/unit benchmarks/inputs/test_knn_photons.bin")

        # Remove the devkit which we ship separately.
        safe_delete_directory("appleseed/extras/devkit")

    def add_local_binaries_to_stage(self):
        progress("Adding local binaries to staging directory")
        safe_make_directory("appleseed/bin")
        dir_util.copy_tree(os.path.join(self.settings.appleseed_path, "sandbox/bin", self.settings.configuration), "appleseed/bin/")

    def add_scripts_to_stage(self):
        progress("Adding scripts to staging directory")
        shutil.copy("convertmany.py", "appleseed/bin/")
        shutil.copy("rendermany.py", "appleseed/bin/")
        shutil.copy("updatemany.py", "appleseed/bin/")
        shutil.copy("rendernode.py", "appleseed/bin/")
        shutil.copy("rendermanager.py", "appleseed/bin/")

    def add_local_schema_files_to_stage(self):
        progress("Adding local schema files to staging directory")
        safe_make_directory("appleseed/schemas")
        copy_glob(os.path.join(self.settings.appleseed_path, "sandbox/schemas/*.xsd"), "appleseed/schemas/")

    def add_text_files_to_stage(self):
        progress("Adding LICENSE.txt and README.md files")
        shutil.copy(os.path.join(self.settings.appleseed_path, "LICENSE.txt"), "appleseed/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "README.md"), "appleseed/")

    def add_dummy_files_into_empty_directories(self):
        progress("Adding dummy files to preserve empty directories")
        for dirpath, dirnames, filenames in os.walk("."):
            if len(dirnames) == 0 and len(filenames) == 0:
                self.create_preserve_file(dirpath)

    def create_preserve_file(self, path):
        with open(os.path.join(path, "preserve.txt"), "w") as f:
            f.write("This file allows to preserve this otherwise empty directory.\n")

    # This method is overridden in the platform-specific builders below.
    def alterate_stage(self):
        return

    def build_final_zip_file(self):
        progress("Building final zip file from staging directory")
        package_base_path = os.path.splitext(self.package_info.package_path)[0]
        archive_util.make_zipfile(package_base_path, "appleseed")

    def remove_stage(self):
        safe_delete_directory("appleseed")

    def run(self, cmdline):
        info("Running command line: {0}".format(cmdline))
        os.system(cmdline)


#--------------------------------------------------------------------------------------------------
# Windows package builder.
#--------------------------------------------------------------------------------------------------

class WindowsPackageBuilder(PackageBuilder):
    def alterate_stage(self):
        self.add_dependencies_to_stage()

    def add_dependencies_to_stage(self):
        progress("Windows-specific: adding dependencies to staging directory")
        self.copy_qt_framework("QtCore")
        self.copy_qt_framework("QtGui")
        copy_glob(os.path.join(self.settings.platform_runtime_path, "*"), "appleseed/bin/")

    def copy_qt_framework(self, framework_name):
        src_filepath = os.path.join(self.settings.qt_runtime_path, framework_name + "4" + ".dll")
        dst_path = os.path.join("appleseed", "bin")
        shutil.copy(src_filepath, dst_path)


#--------------------------------------------------------------------------------------------------
# Mac package builder.
#--------------------------------------------------------------------------------------------------

class MacPackageBuilder(PackageBuilder):
    def __init__(self, settings, package_info):
        PackageBuilder.__init__(self, settings, package_info)
        self.build_path = os.path.join(self.settings.appleseed_path, "build", self.settings.platform_id)

    def alterate_stage(self):
        self.add_dependencies_to_stage()
        self.fixup_binaries()
        self.create_qt_conf_file()
        self.copy_run_script()
        safe_delete_file("appleseed/bin/.DS_Store")

    def fixup_binaries(self):
        progress("Mac-specific: fixing up binaries")
        self.fixup_libappleseed()
        self.fixup_libappleseed_shared()
        self.fixup_appleseed_cli()
        self.fixup_appleseed_studio()
        self.fixup_qt_frameworks()

    def fixup_libappleseed(self):
        self.fixup_id("libappleseed.dylib", "libappleseed.dylib")

    def fixup_libappleseed_shared(self):
        self.fixup_id("libappleseed.shared.dylib", "libappleseed.shared.dylib")
        self.fixup_change("libappleseed.shared.dylib", os.path.join(self.build_path, "appleseed/libappleseed.dylib"), "libappleseed.dylib")

    def fixup_appleseed_cli(self):
        self.fixup_change("appleseed.cli", os.path.join(self.build_path, "appleseed/libappleseed.dylib"), "libappleseed.dylib")
        self.fixup_change("appleseed.cli", os.path.join(self.build_path, "appleseed.shared/libappleseed.shared.dylib"), "libappleseed.shared.dylib")

    def fixup_appleseed_studio(self):
        self.fixup_change("appleseed.studio", os.path.join(self.build_path, "appleseed/libappleseed.dylib"), "libappleseed.dylib")
        self.fixup_change("appleseed.studio", os.path.join(self.build_path, "appleseed.shared/libappleseed.shared.dylib"), "libappleseed.shared.dylib")
        self.fixup_change("appleseed.studio", self.get_qt_framework_path("QtCore"), "QtCore.framework/Versions/4/QtCore")
        self.fixup_change("appleseed.studio", self.get_qt_framework_path("QtGui"), "QtGui.framework/Versions/4/QtGui")
        self.fixup_change("appleseed.studio", self.get_qt_framework_path("QtOpenGL"), "QtOpenGL.framework/Versions/4/QtOpenGL")

    def fixup_qt_frameworks(self):
        self.fixup_id("QtCore.framework/Versions/4/QtCore", "QtCore.framework/Versions/4/QtCore")
        self.fixup_id("QtGui.framework/Versions/4/QtGui", "QtGui.framework/Versions/4/QtGui")
        self.fixup_id("QtOpenGL.framework/Versions/4/QtOpenGL", "QtOpenGL.framework/Versions/4/QtOpenGL")
        self.fixup_change("QtGui.framework/Versions/4/QtGui", self.get_qt_framework_path("QtCore"), "QtCore.framework/Versions/4/QtCore")
        self.fixup_change("QtOpenGL.framework/Versions/4/QtOpenGL", self.get_qt_framework_path("QtCore"), "QtCore.framework/Versions/4/QtCore")
        self.fixup_change("QtOpenGL.framework/Versions/4/QtOpenGL", self.get_qt_framework_path("QtGui"), "QtGui.framework/Versions/4/QtGui")

    def fixup_id(self, target, name):
        self.fixup(target, '-id @"' + name + '"')

    def fixup_change(self, target, old, new):
        self.fixup(target, '-change "' + old + '" "' + new + '"')

    def fixup(self, target, args):
        self.run("install_name_tool " + args + " " + os.path.join("appleseed/bin/", target))

    def add_dependencies_to_stage(self):
        progress("Mac-specific: adding dependencies to staging directory")
        self.copy_qt_framework("QtCore")
        self.copy_qt_framework("QtGui")
        self.copy_qt_resources("QtGui")
        self.copy_qt_framework("QtOpenGL")

    def copy_qt_framework(self, framework_name):
        src_filepath = self.get_qt_framework_path(framework_name)
        dest_path = os.path.join("appleseed", "bin", framework_name + ".framework", "Versions", "4")
        safe_make_directory(dest_path)
        shutil.copy(src_filepath, dest_path)
        os.chmod(os.path.join(dest_path, framework_name), S_IRUSR | S_IWUSR)

    def copy_qt_resources(self, framework_name):
        framework_dir = framework_name + ".framework"
        src_path = os.path.join(self.settings.qt_runtime_path, framework_dir, "Versions", "4", "Resources")
        dest_path = os.path.join("appleseed", "bin", framework_dir, "Resources")
        shutil.copytree(src_path, dest_path)

    def get_qt_framework_path(self, framework_name):
        return os.path.join(self.settings.qt_runtime_path, framework_name + ".framework", "Versions", "4", framework_name)

    def create_qt_conf_file(self):
        safe_make_directory("appleseed/bin/Contents/Resources")
        open("appleseed/bin/Contents/Resources/qt.conf", "w").close()

    def copy_run_script(self):
        script_filename = "run-appleseed.sh"
        dest_path = os.path.join("appleseed", "bin")
        shutil.copy(script_filename, dest_path)
        self.make_executable(os.path.join(dest_path, script_filename))

    def make_executable(self, filepath):
        mode = os.stat(filepath)[ST_MODE]
        mode |= S_IXUSR | S_IXGRP | S_IXOTH
        os.chmod(filepath, mode)


#--------------------------------------------------------------------------------------------------
# Linux package builder.
#--------------------------------------------------------------------------------------------------

class LinuxPackageBuilder(PackageBuilder):
    def alterate_stage(self):
        self.add_dependencies_to_stage()
        self.copy_run_script()

    def add_dependencies_to_stage(self):
        progress("Linux-specific: adding dependencies to staging directory")
        self.copy_qt_library("QtCore")
        self.copy_qt_library("QtGui")
        self.copy_qt_library("QtOpenGL")
        self.copy_png_library()

    def copy_qt_library(self, library_name):
        library_filename = "lib" + library_name + ".so.4"
        src_filepath = os.path.join(self.settings.qt_runtime_path, library_filename)
        dest_path = os.path.join("appleseed", "bin")
        shutil.copy(src_filepath, dest_path)

    def copy_png_library(self):
        library_filename = "libpng15.so.15"
        src_filepath = os.path.join("/usr/local/lib", library_filename)
        dest_path = os.path.join("appleseed", "bin")
        shutil.copy(src_filepath, dest_path)

    def copy_run_script(self):
        script_filename = "run-appleseed.sh"
        dest_path = os.path.join("appleseed", "bin")
        shutil.copy(script_filename, dest_path)
        self.make_executable(os.path.join(dest_path, script_filename))

    def make_executable(self, filepath):
        mode = os.stat(filepath)[ST_MODE]
        mode |= S_IXUSR | S_IXGRP | S_IXOTH
        os.chmod(filepath, mode)


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    print "appleseed.package version " + VersionString
    print

    settings = Settings()
    package_info = PackageInfo(settings)

    settings.load()
    package_info.load()

    if os.name == "nt":
        package_builder = WindowsPackageBuilder(settings, package_info)
    elif os.name == "posix" and platform.mac_ver()[0] != "":
        package_builder = MacPackageBuilder(settings, package_info)
    elif os.name == "posix" and platform.mac_ver()[0] == "":
        package_builder = LinuxPackageBuilder(settings, package_info)
    else:
        fatal("unsupported platform: " + os.name)

    package_builder.build_package()

if __name__ == '__main__':
    main()
