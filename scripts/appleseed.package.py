#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

from __future__ import print_function
from distutils import archive_util, dir_util
from xml.etree.ElementTree import ElementTree
import argparse
import fnmatch
import glob
import os
import platform
import re
import shutil
import stat
import subprocess
import sys
import time
import traceback
import zipfile


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "2.5.4"
SETTINGS_FILENAME = "appleseed.package.configuration.xml"


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

def info(message):
    print("  " + message)


def progress(message):
    print("  " + message + "...")


def fatal(message):
    print("Fatal: " + message + ". Aborting.")
    if sys.exc_info()[0]:
        print(traceback.format_exc())
    sys.exit(1)


def exe(filepath):
    return filepath + ".exe" if os.name == "nt" else filepath


def safe_delete_file(path):
    try:
        if os.path.exists(path):
            os.remove(path)
    except OSError:
        fatal("Failed to delete file '" + path + "'")


def on_rmtree_error(func, path, exc_info):
    # path contains the path of the file that couldn't be removed.
    # Let's just assume that it's read-only and unlink it.
    os.chmod(path, stat.S_IWRITE)
    os.unlink(path)


def safe_delete_directory(path):
    Attempts = 10
    for attempt in range(Attempts):
        try:
            if os.path.exists(path):
                shutil.rmtree(path, onerror=on_rmtree_error)
            return
        except OSError:
            if attempt < Attempts - 1:
                time.sleep(0.5)
            else:
                fatal("Failed to delete directory '" + path + "'")


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


def make_writable(filepath):
    os.chmod(filepath, stat.S_IRUSR | stat.S_IWUSR)


def merge_tree(src, dst, symlinks=False, ignore=None):
    names = os.listdir(src)
    if ignore is not None:
        ignored_names = ignore(src, names)
    else:
        ignored_names = set()

    if not os.path.exists(dst):
        os.makedirs(dst)

    errors = []
    for name in names:
        if name in ignored_names:
            continue
        srcname = os.path.join(src, name)
        dstname = os.path.join(dst, name)
        try:
            if symlinks and os.path.islink(srcname):
                linkto = os.readlink(srcname)
                os.symlink(linkto, dstname)
            elif os.path.isdir(srcname):
                merge_tree(srcname, dstname, symlinks, ignore)
            else:
                # Will raise a SpecialFileError for unsupported file types.
                shutil.copy2(srcname, dstname)
        # Catch the Error from the recursive copytree so that we can
        # continue with other files.
        except Error, err:
            errors.extend(err.args[0])
        except EnvironmentError, why:
            errors.append((srcname, dstname, str(why)))
    try:
        shutil.copystat(src, dst)
    except OSError, why:
        if WindowsError is not None and isinstance(why, WindowsError):
            # Copying file access times may fail on Windows.
            pass
        else:
            errors.append((src, dst, str(why)))
    if errors:
        raise Error, errors


# -------------------------------------------------------------------------------------------------
# Settings.
# -------------------------------------------------------------------------------------------------

class Settings:

    def load(self):
        print("Loading settings from " + SETTINGS_FILENAME + "...")
        tree = ElementTree()
        try:
            tree.parse(SETTINGS_FILENAME)
        except IOError:
            fatal("Failed to load configuration file '" + SETTINGS_FILENAME + "'")
        self.__load_values(tree)
        self.__print_summary()

    def __load_values(self, tree):
        self.platform = self.__get_required(tree, "platform")
        self.configuration = self.__get_required(tree, "configuration")
        self.appleseed_path = self.__get_required(tree, "appleseed_path")
        self.appleseed_headers_path = self.__get_required(tree, "appleseed_headers_path")
        self.qt_runtime_path = self.__get_required(tree, "qt_runtime_path")
        self.platform_runtime_path = self.__get_required(tree, "platform_runtime_path")
        self.python_path = self.__get_required(tree, "python_path")
        self.package_output_path = self.__get_required(tree, "package_output_path")

    def __get_required(self, tree, key):
        value = tree.findtext(key)
        if value is None:
            fatal("Missing value \"{0}\" in configuration file".format(key))
        return value

    def __print_summary(self):
        print("")
        print("  Platform:                  " + self.platform)
        print("  Configuration:             " + self.configuration)
        print("  Path to appleseed:         " + self.appleseed_path)
        print("  Path to appleseed headers: " + self.appleseed_headers_path)
        print("  Path to Qt runtime:        " + self.qt_runtime_path)
        if os.name == "nt":
            print("  Path to platform runtime:  " + self.platform_runtime_path)
        print("  Path to Python 2.7:        " + self.python_path)
        print("  Output directory:          " + self.package_output_path)
        print("")


# -------------------------------------------------------------------------------------------------
# Package information.
# -------------------------------------------------------------------------------------------------

class PackageInfo:

    def __init__(self, settings, no_zip):
        self.no_zip = no_zip
        self.settings = settings

    def load(self):
        print("Loading package information...")
        self.retrieve_git_tag()
        self.build_package_path()
        self.print_summary()

    def retrieve_git_tag(self):
        old_path = pushd(self.settings.appleseed_path)
        self.version = subprocess.Popen("git describe --long", stdout=subprocess.PIPE, shell=True).stdout.read().strip()
        os.chdir(old_path)

    def build_package_path(self):
        package_name = "appleseed-" + self.version + "-" + self.settings.platform + ".zip"
        self.package_path = os.path.join(self.settings.package_output_path, self.version, package_name)

    def print_summary(self):
        print("")
        print("  Version:                   " + self.version)
        if not self.no_zip:
            print("  Package path:              " + self.package_path)
        else:
            print("  Package directory:         " + self.settings.package_output_path)
        print("")


# -------------------------------------------------------------------------------------------------
# Base package builder.
# -------------------------------------------------------------------------------------------------

class PackageBuilder:

    def __init__(self, settings, package_info):
        self.settings = settings
        self.package_info = package_info

    def build_package(self):
        print("Building package:")
        print("")
        self.orchestrate()
        print("")
        print("The package was successfully built.")

    def orchestrate(self):
        self.remove_leftovers()
        self.retrieve_sandbox_from_git_repository()
        self.deploy_sandbox_to_stage()
        self.cleanup_stage()
        self.add_local_binaries_to_stage()
        self.add_local_libraries_to_stage()
        self.add_headers_to_stage()
        self.add_shaders_to_stage()
        self.add_scripts_to_stage()
        self.add_local_schema_files_to_stage()
        self.add_text_files_to_stage()
        self.add_dummy_files_into_empty_directories()
        self.disable_system_qt_plugins()
        self.alter_stage()
        if self.package_info.no_zip:
            self.deploy_stage_to_package_directory()
        else:
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

        # Temporarily remove Alembic assembly C++ plugin.
        safe_delete_directory("appleseed/samples/cpp/alembicassembly")

    def add_local_binaries_to_stage(self):
        progress("Adding local binaries to staging directory")
        safe_make_directory("appleseed/bin")
        dir_util.copy_tree(os.path.join(self.settings.appleseed_path, "sandbox/bin", self.settings.configuration), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "sandbox/bin", exe("maketx")), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "sandbox/bin", exe("oiiotool")), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "sandbox/bin", exe("idiff")), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "sandbox/bin", exe("oslc")), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "sandbox/bin", exe("oslinfo")), "appleseed/bin/")

    def add_local_libraries_to_stage(self):
        progress("Adding local libraries to staging directory")
        safe_make_directory("appleseed/lib")
        dir_util.copy_tree(os.path.join(self.settings.appleseed_path, "sandbox/lib", self.settings.configuration), "appleseed/lib/")

    #
    # This method is used by the Mac and Linux package builders.
    # It requires the following members to be defined:
    #
    #   self.shared_lib_ext
    #   self.get_dependencies_for_file()
    #

    def add_unix_dependencies_to_stage(self):
        # Get shared libs needed by binaries.
        bin_libs = set()
        for dirpath, dirnames, filenames in os.walk("appleseed/bin"):
            for filename in filenames:
                ext = os.path.splitext(filename)[1]
                if ext != ".py" and ext != ".conf":
                    libs = self.get_dependencies_for_file(os.path.join("appleseed/bin", filename))
                    bin_libs = bin_libs.union(libs)

        # Get shared libs needed by appleseed.python.
        for dirpath, dirnames, filenames in os.walk("appleseed/lib"):
            appleseedpython_shared_lib = "_appleseedpython" + self.shared_lib_ext
            if appleseedpython_shared_lib in filenames:
                libs = self.get_dependencies_for_file(os.path.join(dirpath, appleseedpython_shared_lib))
                bin_libs = bin_libs.union(libs)

        # Get shared libs needed by libraries.
        lib_libs = set()
        for lib in bin_libs:
            libs = self.get_dependencies_for_file(lib)
            lib_libs = lib_libs.union(libs)

        all_libs = bin_libs.union(lib_libs)

        if False:
            # Print dependencies.
            info("    Dependencies:")
            for lib in all_libs:
                info("      " + lib)

        # Copy needed libs to lib directory.
        dest_dir = os.path.join("appleseed", "lib/")
        for lib in all_libs:
            # The library might already exist, but without writing rights.
            lib_name = os.path.basename(lib)
            dest_path = os.path.join(dest_dir, lib_name)
            if not os.path.exists(dest_path):
                progress("  Copying {0} to {1}".format(lib, dest_dir))
                try:
                    shutil.copy(lib, dest_dir)
                    make_writable(dest_path)
                except IOError:
                    info("WARNING: could not copy {0} to {1}".format(lib, dest_dir))

    def add_headers_to_stage(self):
        progress("Adding headers to staging directory")
        safe_make_directory("appleseed/include")
        ignore_files = shutil.ignore_patterns("*.cpp", "*.c", "*.xsd", "snprintf", "version.h.in")
        shutil.copytree(os.path.join(self.settings.appleseed_headers_path, "foundation"), "appleseed/include/foundation", ignore=ignore_files)
        shutil.copytree(os.path.join(self.settings.appleseed_headers_path, "main"), "appleseed/include/main", ignore=ignore_files)
        shutil.copytree(os.path.join(self.settings.appleseed_headers_path, "renderer"), "appleseed/include/renderer", ignore=ignore_files)

    def add_shaders_to_stage(self):
        progress("Adding shaders to staging directory")
        safe_delete_directory("appleseed/shaders")
        shutil.copytree(os.path.join(self.settings.appleseed_path, "sandbox/shaders"), "appleseed/shaders")
        shutil.copytree(os.path.join(self.settings.appleseed_path, "src/appleseed.shaders/src"), "appleseed/shaders/src")

    def add_scripts_to_stage(self):
        progress("Adding scripts to staging directory")
        shutil.copy("cleanmany.py", "appleseed/bin/")
        shutil.copy("convertmany.py", "appleseed/bin/")
        shutil.copy("rendermany.py", "appleseed/bin/")
        shutil.copy("rendernode.py", "appleseed/bin/")
        shutil.copy("rendermanager.py", "appleseed/bin/")

    def add_local_schema_files_to_stage(self):
        progress("Adding local schema files to staging directory")
        safe_make_directory("appleseed/schemas")
        copy_glob(os.path.join(self.settings.appleseed_path, "sandbox/schemas/*.xsd"), "appleseed/schemas/")

    def add_text_files_to_stage(self):
        progress("Adding text files")
        shutil.copy(os.path.join(self.settings.appleseed_path, "LICENSE.txt"), "appleseed/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "README.md"), "appleseed/")
        shutil.copy(os.path.join(self.settings.appleseed_path, "THIRDPARTIES.txt"), "appleseed/")

    def add_dummy_files_into_empty_directories(self):
        progress("Adding dummy files to preserve empty directories")
        for dirpath, dirnames, filenames in os.walk("."):
            if len(dirnames) == 0 and len(filenames) == 0:
                self.create_preserve_file(dirpath)

    def disable_system_qt_plugins(self):
        progress("Disabling system's Qt plugins")
        with open("appleseed/bin/qt.conf", "w") as f:
            pass

    def create_preserve_file(self, path):
        with open(os.path.join(path, "preserve.txt"), "w") as f:
            f.write("This file allows to preserve this otherwise empty directory.\n")

    # This method is overridden in the platform-specific builders below.
    def alter_stage(self):
        return

    def deploy_stage_to_package_directory(self):
        package_directory = os.path.join(self.settings.package_output_path, "appleseed")
        progress("Removing existing package directory")
        safe_delete_directory(package_directory)
        progress("Deploying staging directory to package directory")
        shutil.copytree("appleseed", package_directory)

    def build_final_zip_file(self):
        progress("Building final zip file from staging directory")
        package_base_path = os.path.splitext(self.package_info.package_path)[0]
        archive_util.make_zipfile(package_base_path, "appleseed")

    def remove_stage(self):
        progress("Deleting staging directory")
        safe_delete_directory("appleseed")

    def run(self, cmdline):
        info("Running command line: {0}".format(cmdline))
        os.system(cmdline)

    def run_subprocess(self, cmdline):
        p = subprocess.Popen(cmdline, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return p.returncode, out, err


# -------------------------------------------------------------------------------------------------
# Windows package builder.
# -------------------------------------------------------------------------------------------------

class WindowsPackageBuilder(PackageBuilder):

    def alter_stage(self):
        self.add_dependencies_to_stage()
        self.add_python_to_stage()

    def add_dependencies_to_stage(self):
        progress("Windows-specific: Adding dependencies to staging directory")
        self.copy_qt_framework("QtCore")
        self.copy_qt_framework("QtGui")
        self.copy_qt_framework("QtOpenGL")
        copy_glob(os.path.join(self.settings.platform_runtime_path, "*"), "appleseed/bin/")

    def add_python_to_stage(self):
        progress("Windows-specific: Adding Python 2.7 to staging directory")

        shutil.copy(os.path.join(self.settings.python_path, "Microsoft.VC90.CRT.manifest"), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.python_path, "msvcr90.dll"), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.python_path, "python.exe"), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.python_path, "python27.dll"), "appleseed/bin/")

        safe_make_directory("appleseed/python27")
        shutil.copytree(os.path.join(self.settings.python_path, "DLLs"), "appleseed/python27/DLLs")
        shutil.copytree(os.path.join(self.settings.python_path, "include"), "appleseed/python27/include")
        shutil.copytree(os.path.join(self.settings.python_path, "Lib"), "appleseed/python27/Lib")
        shutil.copytree(os.path.join(self.settings.python_path, "libs"), "appleseed/python27/libs")
        shutil.copy(os.path.join(self.settings.python_path, "LICENSE.txt"), "appleseed/python27")
        shutil.copy(os.path.join(self.settings.python_path, "README.txt"), "appleseed/python27")

    def copy_qt_framework(self, framework_name):
        src_filepath = os.path.join(self.settings.qt_runtime_path, framework_name + "4" + ".dll")
        dst_path = os.path.join("appleseed", "bin")
        shutil.copy(src_filepath, dst_path)


# -------------------------------------------------------------------------------------------------
# Mac package builder.
# -------------------------------------------------------------------------------------------------

class MacPackageBuilder(PackageBuilder):

    def __init__(self, settings, package_info):
        PackageBuilder.__init__(self, settings, package_info)
        self.shared_lib_ext = ".dylib"
        self.system_libs_prefixes = ["/System/Library/", "/usr/lib/libcurl", "/usr/lib/libc++",
                                     "/usr/lib/libbz2", "/usr/lib/libSystem", "usr/lib/libz",
                                     "/usr/lib/libncurses", "/usr/lib/libobjc.A.dylib"]

    def alter_stage(self):
        safe_delete_file("appleseed/bin/.DS_Store")
        self.add_dependencies_to_stage()
        self.add_python_to_stage()
        self.fixup_binaries()
        self.create_qt_conf_file()
        os.rename("appleseed/bin/appleseed.studio", "appleseed/bin/appleseed-studio")

    def add_dependencies_to_stage(self):
        progress("Mac-specific: Adding dependencies to staging directory")
        self.add_unix_dependencies_to_stage()
        self.copy_qt_framework("QtCore")
        self.copy_qt_framework("QtGui")
        self.copy_qt_resources("QtGui")
        self.copy_qt_framework("QtOpenGL")

    def add_python_to_stage(self):
        progress("Mac-specific: Adding Python 2.7 to staging directory")
        safe_make_directory("appleseed/python27")
        shutil.copytree(os.path.join(self.settings.python_path, "bin"), "appleseed/python27/bin")
        shutil.copytree(os.path.join(self.settings.python_path, "include"), "appleseed/python27/include")
        shutil.copytree(os.path.join(self.settings.python_path, "lib"), "appleseed/python27/lib")
        shutil.copytree(os.path.join(self.settings.python_path, "share"), "appleseed/python27/share")

    def copy_qt_framework(self, framework_name):
        framework_dir = framework_name + ".framework"
        src_filepath = os.path.join(self.settings.qt_runtime_path, framework_dir, "Versions", "4", framework_name)
        dest_path = os.path.join("appleseed", "lib", framework_dir, "Versions", "4")
        safe_make_directory(dest_path)
        shutil.copy(src_filepath, dest_path)
        make_writable(os.path.join(dest_path, framework_name))

    def copy_qt_resources(self, framework_name):
        framework_dir = framework_name + ".framework"
        src_path = os.path.join(self.settings.qt_runtime_path, framework_dir, "Versions", "4", "Resources")
        dest_path = os.path.join("appleseed", "lib", framework_dir, "Resources")
        shutil.copytree(src_path, dest_path)

    def fixup_binaries(self):
        progress("Mac-specific: Fixing up binaries")
        self.set_libraries_ids()
        self.set_qt_framework_ids()
        self.change_library_paths_in_libraries()
        self.change_library_paths_in_executables()
        self.change_qt_framework_paths_in_qt_frameworks()

    def set_libraries_ids(self):
        for dirpath, dirnames, filenames in os.walk("appleseed/lib"):
            for filename in filenames:
                if os.path.splitext(filename)[1] == ".dylib":
                    lib_path = os.path.join(dirpath, filename)
                    self.set_library_id(lib_path, filename)

    def set_qt_framework_ids(self):
        self.set_library_id("appleseed/lib/QtCore.framework/Versions/4/QtCore", "QtCore.framework/Versions/4/QtCore")
        self.set_library_id("appleseed/lib/QtGui.framework/Versions/4/QtGui", "QtGui.framework/Versions/4/QtGui")
        self.set_library_id("appleseed/lib/QtOpenGL.framework/Versions/4/QtOpenGL", "QtOpenGL.framework/Versions/4/QtOpenGL")

    def change_library_paths_in_libraries(self):
        for dirpath, dirnames, filenames in os.walk("appleseed/lib"):
            for filename in filenames:
                ext = os.path.splitext(filename)[1]
                if ext == ".dylib" or ext == ".so":
                    lib_path = os.path.join(dirpath, filename)
                    self.change_library_paths_in_binary(lib_path)
                    self.change_qt_framework_paths_in_binary(lib_path)

    def change_library_paths_in_executables(self):
        for dirpath, dirnames, filenames in os.walk("appleseed/bin"):
            for filename in filenames:
                ext = os.path.splitext(filename)[1]
                if ext != ".py" and ext != ".conf":
                    exe_path = os.path.join(dirpath, filename)
                    self.change_library_paths_in_binary(exe_path)
                    self.change_qt_framework_paths_in_binary(exe_path)

    # Can be used on executables and dynamic libraries.
    def change_library_paths_in_binary(self, bin_path):
        progress("Patching {0}".format(bin_path))
        bin_dir = os.path.dirname(bin_path)
        path_to_appleseed_lib = os.path.relpath("appleseed/lib/", bin_dir)
        for lib_path in self.get_dependencies_for_file(bin_path, fix_paths=False):
            lib_name = os.path.basename(lib_path)
            self.change_library_path(bin_path, lib_path, "@loader_path/{0}/{1}".format(path_to_appleseed_lib, lib_name))

    # Can be used on executables and dynamic libraries.
    def change_qt_framework_paths_in_binary(self, bin_path):
        for fwk_path in self.get_qt_frameworks_for_file(bin_path):
            fwk_name = re.search(r"(Qt.*)\.framework", fwk_path).group(1)
            self.change_library_path(bin_path, fwk_path, "@executable_path/../lib/{0}.framework/Versions/4/{0}".format(fwk_name))

    def change_qt_framework_paths_in_qt_frameworks(self):
        self.change_qt_framework_paths_in_binary("appleseed/lib/QtCore.framework/Versions/4/QtCore")
        self.change_qt_framework_paths_in_binary("appleseed/lib/QtGui.framework/Versions/4/QtGui")
        self.change_qt_framework_paths_in_binary("appleseed/lib/QtOpenGL.framework/Versions/4/QtOpenGL")

    def set_library_id(self, target, name):
        self.run('install_name_tool -id "{0}" {1}'.format(name, target))

    def change_library_path(self, target, old, new):
        self.run('install_name_tool -change "{0}" "{1}" {2}'.format(old, new, target))

    def get_dependencies_for_file(self, filename, fix_paths=True):
        returncode, out, err = self.run_subprocess(["otool", "-L", filename])
        if returncode != 0:
            fatal("Failed to invoke otool(1) to get dependencies for {0}: {1}".format(filename, err))

        libs = set()

        for line in out.split("\n")[1:]:    # skip the first line
            line = line.strip()

            # Ignore empty lines.
            if len(line) == 0:
                continue

            # Parse the line.
            m = re.match(r"(.*) \(compatibility version .*, current version .*\)", line)
            if not m:
                fatal("Failed to parse line from otool(1) output: " + line)
            lib = m.group(1)

            # Ignore libs relative to @rpath.
            if "@rpath" in lib:
                continue

            # Ignore libs relative to @loader_path.
            if "@loader_path" in lib:
                continue

            # Ignore system libs.
            if self.is_system_lib(lib):
                continue

            # Ignore Qt frameworks.
            if re.search(r"Qt.*\.framework", lib):
                continue

            if fix_paths:
                # Optionally search for libraries in other places.
                if not os.path.exists(lib):
                    candidate = os.path.join("/usr/local/lib/", lib)
                    if os.path.exists(candidate):
                        lib = candidate

            libs.add(lib)

        if False:
            info("Dependencies for file {0}:".format(filename))
            for lib in libs:
                info("    {0}".format(lib))

        return libs

    def get_qt_frameworks_for_file(self, filename, fix_paths=True):
        returncode, out, err = self.run_subprocess(["otool", "-L", filename])
        if returncode != 0:
            fatal("Failed to invoke otool(1) to get dependencies for {0}: {1}".format(filename, err))

        libs = set()

        for line in out.split("\n")[1:]:    # skip the first line
            line = line.strip()

            # Ignore empty lines.
            if len(line) == 0:
                continue

            # Parse the line.
            m = re.match(r"(.*) \(compatibility version .*, current version .*\)", line)
            if not m:
                fatal("Failed to parse line from otool(1) output: " + line)
            lib = m.group(1)

            if re.search(r"Qt.*\.framework", lib):
                libs.add(lib)

        return libs

    def is_system_lib(self, lib):
        for prefix in self.system_libs_prefixes:
            if lib.startswith(prefix):
                return True
        return False

    def create_qt_conf_file(self):
        safe_make_directory("appleseed/bin/Contents/Resources")
        open("appleseed/bin/Contents/Resources/qt.conf", "w").close()


# -------------------------------------------------------------------------------------------------
# Linux package builder.
# -------------------------------------------------------------------------------------------------

class LinuxPackageBuilder(PackageBuilder):

    def __init__(self, settings, package_info):
        PackageBuilder.__init__(self, settings, package_info)
        self.shared_lib_ext = ".so"
        self.system_libs_prefixes = ["linux", "librt", "libpthread", "libGL", "libX", "libselinux",
                                     "libICE", "libSM", "libdl", "libm.so", "libgcc", "libc.so",
                                     "/lib64/ld-linux-", "libstdc++", "libxcb", "libdrm", "libnsl",
                                     "libuuid", "libgthread", "libglib", "libgobject", "libglapi",
                                     "libffi", "libfontconfig", "libutil", "libpython",
                                     "libxshmfence.so"]

    def alter_stage(self):
        self.make_executable(os.path.join("appleseed/bin", "maketx"))
        self.make_executable(os.path.join("appleseed/bin", "oiiotool"))
        self.make_executable(os.path.join("appleseed/bin", "idiff"))
        self.make_executable(os.path.join("appleseed/bin", "oslc"))
        self.make_executable(os.path.join("appleseed/bin", "oslinfo"))
        self.add_dependencies_to_stage()
        self.set_runtime_paths_on_binaries()
        self.clear_runtime_paths_on_libraries()
        self.add_python_to_stage()  # Must be last.

    def make_executable(self, filepath):
        mode = os.stat(filepath)[stat.ST_MODE]
        mode |= stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
        os.chmod(filepath, mode)

    def add_dependencies_to_stage(self):
        progress("Linux-specific: Adding dependencies to staging directory")
        self.add_unix_dependencies_to_stage()

    def set_runtime_paths_on_binaries(self):
        progress("Linux-specific: Setting runtime paths on binaries")
        for dirpath, dirnames, filenames in os.walk("appleseed/bin"):
            for filename in filenames:
                ext = os.path.splitext(filename)[1]
                if ext != ".py" and ext != ".conf":
                    self.run("chrpath -r \$ORIGIN/../lib " + os.path.join("appleseed/bin", filename))

    def clear_runtime_paths_on_libraries(self):
        progress("Linux-specific: Clearing runtime paths on libraries")
        for dirpath, dirnames, filenames in os.walk("appleseed/lib"):
            for filename in filenames:
                if os.path.splitext(filename)[1] == ".so":
                    self.run("chrpath -d " + os.path.join(dirpath, filename))

    def get_dependencies_for_file(self, filename):
        returncode, out, err = self.run_subprocess(["ldd", filename])
        if returncode != 0:
            fatal("Failed to invoke ldd(1) to get dependencies for {0}: {1}".format(filename, err))

        libs = set()

        for line in out.split("\n"):
            line = line.strip()

            # Ignore empty lines.
            if len(line) == 0:
                continue

            # Ignore system libs.
            if self.is_system_lib(line):
                continue

            libs.add(line.split()[2])

        return libs

    def is_system_lib(self, lib):
        for prefix in self.system_libs_prefixes:
            if lib.startswith(prefix):
                return True
        return False

    def add_python_to_stage(self):
        progress("Linux-specific: Adding Python 2.7 to staging directory")

        merge_tree(os.path.join(self.settings.python_path, "bin"), "appleseed/bin", symlinks=True)
        merge_tree(os.path.join(self.settings.python_path, "lib"), "appleseed/lib", symlinks=True)
        merge_tree(os.path.join(self.settings.python_path, "include"), "appleseed/include", symlinks=True)


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="build an appleseed package from sources")

    parser.add_argument("--nozip", help="do not build a final .zip.  Files will be copied to staging directory only", action="store_true")

    args = parser.parse_args()

    no_zip = args.nozip

    print("appleseed.package version " + VERSION)
    print("")

    print("IMPORTANT:")
    print("")
    print("  - You may need to run this tool with sudo on Linux and macOS")
    print("  - Make sure there are no obsolete binaries in sandbox/bin")
    print("")

    settings = Settings()
    package_info = PackageInfo(settings, no_zip)

    settings.load()
    package_info.load()

    if os.name == "nt":
        package_builder = WindowsPackageBuilder(settings, package_info)
    elif os.name == "posix" and platform.mac_ver()[0] != "":
        package_builder = MacPackageBuilder(settings, package_info)
    elif os.name == "posix" and platform.mac_ver()[0] == "":
        package_builder = LinuxPackageBuilder(settings, package_info)
    else:
        fatal("Unsupported platform: " + os.name)

    package_builder.build_package()

if __name__ == "__main__":
    main()
