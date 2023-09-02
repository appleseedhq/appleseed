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
import colorama
import fnmatch
import glob
import json
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

from utils import print_runtime_details  # local module


# -------------------------------------------------------------------------------------------------
# Constants.
# -------------------------------------------------------------------------------------------------

VERSION = "2.7.3"
SETTINGS_FILENAME = "appleseed.package.configuration.xml"
VERBOSE = False


# -------------------------------------------------------------------------------------------------
# Utility functions.
# -------------------------------------------------------------------------------------------------

GREEN_CHECKMARK = u"{0}\u2713{1}".format(colorama.Style.BRIGHT + colorama.Fore.GREEN, colorama.Style.RESET_ALL)
RED_CROSSMARK = u"{0}\u2717{1}".format(colorama.Style.BRIGHT + colorama.Fore.RED, colorama.Style.RESET_ALL)


def trace(message):
    if VERBOSE:
        print(u"  {0}{1}{2}".format(colorama.Style.DIM + colorama.Fore.WHITE, message, colorama.Style.RESET_ALL))


def info(message):
    print(u"  {0}".format(message))


def progress(message):
    print(u"  {0}...".format(message))


def warning(message):
    print(u"  {0}Warning: {1}.{2}".format(colorama.Style.BRIGHT + colorama.Fore.MAGENTA, message, colorama.Style.RESET_ALL))


def fatal(message):
    print(u"{0}Fatal: {1}. Aborting.{2}".format(colorama.Style.BRIGHT + colorama.Fore.RED, message, colorama.Style.RESET_ALL))
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
        fatal("Failed to delete file {0}".format(path))


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
                fatal("Failed to delete directory {0}".format(path))


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
    st = os.stat(filepath)
    os.chmod(filepath, st.st_mode | stat.S_IRUSR | stat.S_IWUSR)


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
        except Error as ex:
            errors.extend(ex.args[0])
        except EnvironmentError as ex:
            errors.append((srcname, dstname, str(ex)))
    try:
        shutil.copystat(src, dst)
    except OSError as ex:
        if WindowsError is not None and isinstance(ex, WindowsError):
            # Copying file access times may fail on Windows.
            pass
        else:
            errors.append((src, dst, str(ex)))
    if errors:
        raise Error(errors)


# -------------------------------------------------------------------------------------------------
# Settings.
# -------------------------------------------------------------------------------------------------


class Settings:

    def load(self):
        print("Loading settings from {0}...".format(SETTINGS_FILENAME))
        tree = ElementTree()
        try:
            tree.parse(SETTINGS_FILENAME)
        except IOError:
            fatal("Failed to load configuration file {0}".format(SETTINGS_FILENAME))
        self.__load_values(tree)

    def print_summary(self):
        print("")
        print("  Platform:                  {0}".format(self.platform))
        print("  Configuration:             {0}".format(self.configuration))
        print("  Path to appleseed:         {0}".format(self.appleseed_path))
        print("  Path to appleseed headers: {0}".format(self.appleseed_headers_path))
        print("  Path to Python 2.7:        {0}".format(self.python_path))
        print("  Path to Qt runtime:        {0}".format(self.qt_runtime_path))
        if os.name == "nt":
            print("  Path to platform runtime:  {0}".format(self.platform_runtime_path))
            print("  Path to OpenSSL DLLs:      {0}".format(self.openssl_path))
        print("  Output directory:          {0}".format(self.package_output_path))
        print("")

    def __load_values(self, tree):
        self.platform = self.__get_required(tree, "platform")
        self.configuration = self.__get_required(tree, "configuration")
        self.appleseed_path = self.__get_required(tree, "appleseed_path")
        self.appleseed_headers_path = self.__get_required(tree, "appleseed_headers_path")
        self.qt_runtime_path = self.__get_required(tree, "qt_runtime_path")
        self.platform_runtime_path = self.__get_required(tree, "platform_runtime_path")
        self.python_path = self.__get_required(tree, "python_path")
        self.openssl_path = self.__get_required(tree, "openssl_path")
        self.package_output_path = self.__get_required(tree, "package_output_path")

    def __get_required(self, tree, key):
        value = tree.findtext(key)
        if value is None:
            fatal("Missing value \"{0}\" in configuration file".format(key))
        return value


# -------------------------------------------------------------------------------------------------
# Package information.
# -------------------------------------------------------------------------------------------------

class PackageInfo:

    def __init__(self, settings, no_zip, no_app=True):
        self.no_zip = no_zip
        self.no_app = no_app
        self.settings = settings

    def load(self):
        print("Loading package information...")
        self.retrieve_git_tag()
        self.build_package_path()

    def retrieve_git_tag(self):
        old_path = pushd(self.settings.appleseed_path)
        self.version = subprocess.check_output(["git", "describe", "--long"]).decode("utf-8").strip()
        os.chdir(old_path)

    def build_package_path(self):
        if os.name == 'nt':
            ext = "zip"
        else:
            if not self.no_app:
                ext = 'dmg'
            else:
                ext = "tar.gz"

        package_dir = "appleseed-{0}".format(self.version)
        package_name = "appleseed-{0}-{1}.{2}".format(self.version, self.settings.platform, ext)
        self.package_path = os.path.join(self.settings.package_output_path, package_dir, package_name)

    def print_summary(self):
        print("")
        print("  Version:                   {0}".format(self.version))
        if not self.no_zip:
            print("  Package path:              {0}".format(self.package_path))
        else:
            print("  Package directory:         {0}".format(self.settings.package_output_path))
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
        # self.disable_system_qt_plugins()
        self.postprocess_stage()
        if self.package_info.no_zip:
            self.deploy_stage_to_package_directory()
        else:
            self.build_final_archive_file()
        self.remove_stage()

    def remove_leftovers(self):
        progress("Removing leftovers from previous invocations")
        safe_delete_directory("appleseed")
        safe_delete_directory("appleseed.app")
        safe_delete_file("sandbox.zip")
        safe_delete_file(self.package_info.package_path)

    def retrieve_sandbox_from_git_repository(self):
        progress("Retrieving sandbox from Git repository")
        old_path = pushd(os.path.join(self.settings.appleseed_path, "sandbox"))
        self.run("git archive --format=zip --output={0} --worktree-attributes HEAD".format(os.path.join(old_path, "sandbox.zip")))
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
        safe_delete_directory("appleseed/examples/cpp/alembicassembly")

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

    def get_paths_to_binaries(self):
        paths = []

        # Main binaries.
        for dirpath, dirnames, filenames in os.walk("appleseed/bin"):
            for filename in filenames:
                ext = os.path.splitext(filename)[1]
                if ext != ".py" and ext != ".conf":
                    paths.append(os.path.join(dirpath, filename))

        # appleseed.python.
        # Loadable modules use the .so extension on macOS, just like on Linux.
        # Visit https://stackoverflow.com/a/2339910/393756 for details.
        # TODO: commented out _appleseedpython3.so: on macOS it depends on the Python framework,
        # and unfortunately the Python 2 and Python 3 frameworks have the same name, hence they
        # collide when copied to lib/, causing appleseed.studio to fail at startup.
        # appleseedpython_shared_libs = [ "_appleseedpython.so", "_appleseedpython3.so" ]
        appleseedpython_shared_libs = ["_appleseedpython.so"]
        for dirpath, dirnames, filenames in os.walk("appleseed/lib"):
            for appleseedpython_shared_lib in appleseedpython_shared_libs:
                if appleseedpython_shared_lib in filenames:
                    paths.append(os.path.join(dirpath, appleseedpython_shared_lib))

        return paths

    #
    # This method is used by the macOS and Linux package builders.
    # It requires the following members to be defined:
    #
    #   self.SHARED_LIB_EXT
    #   self.get_dependencies_for_file()
    #

    def add_unix_dependencies_to_stage(self, roots):
        # Get shared libs needed by root files.
        root_libs = set()
        for path in roots:
            libs = self.get_dependencies_for_file(path)
            root_libs.update(libs)

        # Gather indirect dependencies.
        all_libs = root_libs.copy()
        visited_libs = set()
        self.get_recursive_dependencies_for_files(all_libs, visited_libs, root_libs)

        # Print final list of dependencies.
        trace("  === Final list of all dependencies: ===")
        for lib in all_libs:
            trace("      {0}".format(lib))

        # Copy needed libs to lib directory.
        dest_dir = os.path.join("appleseed", "lib/")
        for lib in all_libs:
            # The library might already exist, but without writing rights.
            lib_name = os.path.basename(lib)
            dest_path = os.path.join(dest_dir, lib_name)
            if not os.path.exists(dest_path):
                trace("  Copying {0} to {1}...".format(lib, dest_dir))
                try:
                    shutil.copy(lib, dest_dir)
                    make_writable(dest_path)
                except IOError:
                    warning("Could not copy {0} to {1}".format(lib, dest_dir))

    def get_recursive_dependencies_for_files(self, output_libs, visited_libs, input_libs):
        for lib in input_libs:
            if lib not in visited_libs:
                visited_libs.add(lib)
                sub_libs = self.get_dependencies_for_file(lib)
                output_libs.update(sub_libs)
                self.get_recursive_dependencies_for_files(output_libs, visited_libs, sub_libs)

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
    def postprocess_stage(self):
        return

    def deploy_stage_to_package_directory(self):
        package_directory = os.path.join(self.settings.package_output_path, "appleseed")
        progress("Removing existing package directory")
        safe_delete_directory(package_directory)
        progress("Deploying staging directory to package directory")
        if self.package_info.no_app:
            shutil.copytree("appleseed", package_directory, symlinks=True)
        else:
            shutil.copytree("appleseed.app", package_directory + ".app", symlinks=True)

    def build_final_archive_file(self):
        progress("Building final archive file from staging directory")
        package_base_path = os.path.splitext(self.package_info.package_path)[0]
        if os.name == 'nt':
            archive_util.make_zipfile(package_base_path, "appleseed")
        elif not self.package_info.no_app:
            background_icon_path = os.path.join(self.settings.appleseed_path, "resources/logo/appleseed-drive-background.png")
            drive_icon_path = os.path.join(self.settings.appleseed_path, "resources/logo/appleseed-drive.png")
            self._create_dmg('appleseed.app', self.package_info.package_path, background_icon_path, drive_icon_path)
        else:
            archive_util.make_tarball(package_base_path, "appleseed")

    def remove_stage(self):
        progress("Deleting staging directory")
        safe_delete_directory("appleseed")
        safe_delete_directory("appleseed.app")

    def run(self, cmdline):
        trace("  Running command line: {0}".format(cmdline))
        os.system(cmdline)

    def run_subprocess(self, cmdline):
        if VERBOSE:
            trace("Invoking: {0}".format(cmdline))
        p = subprocess.Popen(cmdline, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        return p.returncode, out, err


# -------------------------------------------------------------------------------------------------
# Windows package builder.
# -------------------------------------------------------------------------------------------------

class WindowsPackageBuilder(PackageBuilder):

    def postprocess_stage(self):
        self.__add_dependencies_to_stage()
        self.__add_python_to_stage()
        self.__add_openssl_to_stage()

    def __add_dependencies_to_stage(self):
        progress("Windows-specific: Adding dependencies to staging directory")

        # Qt frameworks.
        self.__copy_qt_framework("Qt5Core")
        self.__copy_qt_framework("Qt5Gui")
        self.__copy_qt_framework("Qt5Network")
        self.__copy_qt_framework("Qt5OpenGL")
        self.__copy_qt_framework("Qt5Widgets")

        # Qt platform plugins.
        safe_make_directory("appleseed/bin/platforms")
        qt_platform_plugins_path = os.path.join(self.settings.qt_runtime_path, "plugins", "platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "qdirect2d.dll"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "qminimal.dll"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "qoffscreen.dll"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "qwindows.dll"), "appleseed/bin/platforms")

        # Platform runtime files.
        copy_glob(os.path.join(self.settings.platform_runtime_path, "*"), "appleseed/bin/")

    def __add_python_to_stage(self):
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

    def __copy_qt_framework(self, framework_name):
        src_filepath = os.path.join(self.settings.qt_runtime_path, "bin", framework_name + ".dll")
        dst_path = os.path.join("appleseed", "bin")
        shutil.copy(src_filepath, dst_path)

    def __add_openssl_to_stage(self):
        progress("Windows-specific: Adding OpenSSL DLLs to staging directory")

        shutil.copy(os.path.join(self.settings.openssl_path, "libeay32.dll"), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.openssl_path, "ssleay32.dll"), "appleseed/bin/")
        shutil.copy(os.path.join(self.settings.openssl_path, "OpenSSL License.txt"), "appleseed/bin/")


# -------------------------------------------------------------------------------------------------
# Mac package builder.
# -------------------------------------------------------------------------------------------------

class MacPackageBuilder(PackageBuilder):

    SHARED_LIB_EXT = ".dylib"

    SYSTEM_LIBS_PREFIXES = [
        "/System/Library/",
        "/usr/lib"
    ]

    QT_FRAMEWORKS = [
        "QtConcurrent",
        "QtCore",
        "QtDBus",
        "QtGui",
        "QtNetwork",
        "QtOpenGL",
        "QtPrintSupport",
        "QtWidgets"
    ]

    def postprocess_stage(self):
        safe_delete_file("appleseed/bin/.DS_Store")
        self.__add_dependencies_to_stage()
        self.__add_python_to_stage()
        self.__fixup_binaries()
        os.rename("appleseed/bin/appleseed.studio", "appleseed/bin/appleseed-studio")
        if not self.package_info.no_app:
            icon_path = os.path.join(self.settings.appleseed_path, "resources/logo/appleseed-seeds-2048.png")
            self.__create_app('appleseed', "appleseed.app", icon_path)

    def __add_dependencies_to_stage(self):
        progress("Mac-specific: Adding dependencies to staging directory")

        # Qt frameworks.
        for framework in self.QT_FRAMEWORKS:
            self.__copy_qt_framework(framework)
            self.__copy_qt_resources(framework)

        # Qt platform plugins.
        safe_make_directory("appleseed/bin/platforms")
        qt_platform_plugins_path = os.path.join(self.settings.qt_runtime_path, "plugins", "platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "libqcocoa.dylib"), "appleseed/bin/platforms")

        # Add other platform plugins only if they exists
        for platform_plugin in ["libqminimal.dylib", "libqoffscreen.dylib", "libqwebgl.dylib"]:
            platform_plugin_path = os.path.join(qt_platform_plugins_path, platform_plugin)
            if os.path.exists(platform_plugin_path):
                shutil.copy(platform_plugin_path, "appleseed/bin/platforms")

        self.add_unix_dependencies_to_stage(self.get_paths_to_binaries())

    def __add_python_to_stage(self):
        progress("Mac-specific: Adding Python 2.7 to staging directory")

        # Python framework (TODO: currently hardcoded to Python 2.7).
        # This assumes a homebrew install of python
        python_dir = "appleseed/python27"
        safe_make_directory(python_dir)

        # just copy everything
        merge_tree(self.settings.python_path, python_dir, symlinks=True)

        # Homebrew will create a symlink for site-packages.
        # Replace it with a empty directory if found
        python_home = os.path.join(python_dir, "Frameworks/Python.framework/Versions/2.7")
        site_packages_path = os.path.join(python_home, "lib/python2.7/site-packages")
        if os.path.islink(site_packages_path):
            os.remove(site_packages_path)
            safe_make_directory(site_packages_path)

    def __copy_qt_framework(self, framework_name):
        framework_dir = framework_name + ".framework"
        src_filepath = os.path.join(self.settings.qt_runtime_path, "lib", framework_dir, "Versions", "5", framework_name)
        dest_path = os.path.join("appleseed", "lib", framework_dir, "Versions", "5")
        safe_make_directory(dest_path)
        shutil.copy(src_filepath, dest_path)
        make_writable(os.path.join(dest_path, framework_name))

    def __copy_qt_resources(self, framework_name):
        framework_dir = framework_name + ".framework"
        src_path = os.path.join(self.settings.qt_runtime_path, "lib", framework_dir, "Versions", "5", "Resources")
        dest_path = os.path.join("appleseed", "lib", framework_dir, "Resources")
        shutil.copytree(src_path, dest_path)

    def __fixup_binaries(self):
        progress("Mac-specific: Fixing up binaries")
        self.set_libraries_ids()
        self.set_qt_framework_ids()
        self.__change_library_paths_in_libraries()
        self.__change_library_paths_in_executables()
        self.__change_qt_framework_paths_in_qt_frameworks()

    def set_libraries_ids(self):
        for lib_dir in ("appleseed/lib", 'appleseed/python27', 'appleseed/bin/platforms'):
            for dirpath, dirnames, filenames in os.walk(lib_dir):
                for filename in filenames:
                    ext = os.path.splitext(filename)[1]
                    if ext == ".dylib" or ext == ".so":
                        lib_path = os.path.join(dirpath, filename)
                        self.__set_library_id(lib_path, filename)

    def set_qt_framework_ids(self):
        for framework in self.QT_FRAMEWORKS:
            self.__set_library_id("appleseed/lib/{0}.framework/Versions/5/{0}".format(framework), "{0}.framework/Versions/5/{0}".format(framework))
        self.__set_library_id("appleseed/python27/Frameworks/Python.framework/Versions/2.7/Python", "Python.framework/Versions/2.7/Python")

    def __change_library_paths_in_libraries(self):
        for lib_dir in ("appleseed/lib", 'appleseed/python27'):
            for dirpath, dirnames, filenames in os.walk(lib_dir):
                for filename in filenames:
                    ext = os.path.splitext(filename)[1]
                    if ext == ".dylib" or ext == ".so":
                        lib_path = os.path.join(dirpath, filename)
                        self.__change_library_paths_in_binary(lib_path)
                        self.__change_qt_framework_paths_in_binary(lib_path)

    def __change_library_paths_in_executables(self):
        for bin_dir in ("appleseed/bin", "appleseed/python27/bin", "appleseed/python27/Frameworks/Python.framework/Versions/2.7/bin"):
            for dirpath, dirnames, filenames in os.walk(bin_dir):
                for filename in filenames:
                    ext = os.path.splitext(filename)[1]
                    if ext != ".py" and ext != ".conf":
                        exe_path = os.path.join(dirpath, filename)
                        self.__change_library_paths_in_binary(exe_path)
                        self.__change_qt_framework_paths_in_binary(exe_path)

    def __change_python_library_path(self, bin_path):
        if os.path.islink(bin_path):
            return

        python_lib = "appleseed/python27/Frameworks/Python.framework/Versions/2.7/Python"

        bin_dir = os.path.dirname(bin_path)
        path_to_python_lib = os.path.relpath(python_lib, bin_dir)

        # print(bin_dir, path_to_appleseed_lib)

        for lib_path in self.__iter_all_dependencies(bin_path):
            lib_name = os.path.basename(lib_path)
            if lib_name == 'Python':
                self.__change_library_path(bin_path, lib_path,  "@loader_path/{0}".format(path_to_python_lib))

    # Can be used on executables and dynamic libraries.
    def __change_library_paths_in_binary(self, bin_path):
        if os.path.islink(bin_path):
            return

        progress("  Patching {0}".format(os.path.abspath(bin_path)))
        make_writable(bin_path)

        bin_dir = os.path.dirname(bin_path)
        path_to_appleseed_lib = os.path.relpath("appleseed/lib/", bin_dir)

        # fix_paths set to False because we must retrieve the unmodified dependency in order to replace it by the correct one.
        for lib_path in self.get_dependencies_for_file(bin_path, fix_paths=False, verbose=False):
            lib_name = os.path.basename(lib_path)
            if path_to_appleseed_lib == ".":
                self.__change_library_path(bin_path, lib_path, "@loader_path/{0}".format(lib_name))
            else:
                self.__change_library_path(bin_path, lib_path, "@loader_path/{0}/{1}".format(path_to_appleseed_lib, lib_name))

        self.__change_python_library_path(bin_path)
        self.__delete_rpaths(bin_path)

    # Can be used on executables and dynamic libraries.
    def __change_qt_framework_paths_in_binary(self, bin_path):
        for fwk_path in self.__get_qt_frameworks_for_file(bin_path):
            fwk_name = re.search(r"(Qt.*)\.framework", fwk_path).group(1)
            self.__change_library_path(bin_path, fwk_path, "@executable_path/../lib/{0}.framework/Versions/5/{0}".format(fwk_name))

        self.__change_python_library_path(bin_path)
        self.__delete_rpaths(bin_path)

    def __change_qt_framework_paths_in_qt_frameworks(self):
        for framework in self.QT_FRAMEWORKS:
            self.__change_qt_framework_paths_in_binary("appleseed/lib/{0}.framework/Versions/5/{0}".format(framework))
        self.__change_qt_framework_paths_in_binary("appleseed/python27/Frameworks/Python.framework/Versions/2.7/Python")

    def __set_library_id(self, target, name):
        if os.path.islink(target):
            return
        make_writable(target)
        self.run('install_name_tool -id "{0}" {1}'.format(name, target))

    def __delete_rpaths(self, target):
        for path in self.__get_lib_search_paths(target, use_env=False):
            self.run('install_name_tool -delete_rpath "{0}" "{1}"'.format(path, target))

    def __change_library_path(self, target, old, new):
        self.run('install_name_tool -change "{0}" "{1}" {2}'.format(old, new, target))

    def __get_lib_search_paths(self, filepath, use_env=True):
        returncode, out, err = self.run_subprocess(["otool", "-l", filepath])
        if returncode != 0:
            fatal("Failed to invoke otool(1) to get rpath for {0}: {1}".format(filepath, err))

        lc_path_found = False

        rpaths = []
        # parse otool output for rpaths, there can be multiple
        for line in out.split("\n"):
            line = line.strip()

            if lc_path_found and line.startswith("path"):
                path_split = line.split(' ')
                if len(path_split) < 2:
                    fatal("Failed to parse line from otool(1) output: " + line)
                rpaths.append(path_split[1])
                lc_path_found = False

            if line == "cmd LC_RPATH":
                lc_path_found = True

        search_paths = []
        if use_env:
            DYLD_LIBRARY_PATH = os.environ.get("DYLD_LIBRARY_PATH", "").split(":")
            DYLD_FALLBACK_LIBRARY_PATH = os.environ.get("DYLD_FALLBACK_LIBRARY_PATH", "").split(":")

            # DYLD_LIBRARY_PATH overides rpaths
            for path in DYLD_LIBRARY_PATH:
                if os.path.exists(path):
                    search_paths.append(path)

        for path in rpaths:
            if os.path.exists(path):
                search_paths.append(path)

        if use_env:
            for path in DYLD_FALLBACK_LIBRARY_PATH:
                if os.path.exists(path):
                    search_paths.append(path)

        return search_paths

    def __iter_all_dependencies(self, filepath):
        returncode, out, err = self.run_subprocess(["otool", "-L", filepath])
        if returncode != 0:
            fatal("Failed to invoke otool(1) to get dependencies for {0}: {1}".format(filepath, err))

        for line in out.split("\n")[1:]:    # skip the first line
            line = line.strip()

            # Ignore empty lines.
            if len(line) == 0:
                continue

            # Parse the line.
            m = re.match(r"(.*) \(compatibility version .*, current version .*\)", line)
            if not m:
                fatal("Failed to parse line from otool(1) output: {0}".format(line))
            lib = m.group(1)

            # Ignore self-references (why do these happen?).
            if lib == filepath:
                continue

            yield lib

    def get_dependencies_for_file(self, filepath, fix_paths=True, verbose=True):
        filename = os.path.basename(filepath)

        loader_path = os.path.dirname(filepath)
        search_paths = self.__get_lib_search_paths(filepath)

        if verbose:
            trace("  Gathering dependencies for file")
            trace("      {0}".format(filepath))
            trace("  with @loader_path set to")
            trace("      {0}".format(loader_path))
            trace("  and rpath search path to:")
            for path in search_paths:
                trace("    {0}".format(path))

        returncode, out, err = self.run_subprocess(["otool", "-L", filepath])
        if returncode != 0:
            fatal("Failed to invoke otool(1) to get dependencies for {0}: {1}".format(filepath, err))

        libs = set()

        for lib in self.__iter_all_dependencies(filepath):
            # Ignore system libs.
            if self.__is_system_lib(lib):
                continue

            # Ignore Qt frameworks.
            # TODO: maybe we can simply ignore all frameworks.
            if re.search(r"Qt.*\.framework", lib):
                continue

            # Ignore Python framework.
            if re.search(r"Python\.framework", lib):
                continue

            # Ignore self-references (why do these happen?).
            if os.path.basename(lib) == os.path.basename(filepath):
                continue

            if fix_paths:
                # handle no search paths case
                if not search_paths:
                    fixed_lib = lib.replace("@loader_path", loader_path)
                    if os.path.exists(fixed_lib):
                        lib = fixed_lib
                    else:
                        # Try to handle other relative libs.
                        candidate = os.path.join(loader_path, fixed_lib)
                        if not os.path.exists(candidate):
                            fatal("Unable to resolve lib {}".format(lib))
                        lib = candidate

                for path in search_paths:
                    # Handle libs relative to @loader_path.
                    fixed_lib = lib.replace("@loader_path", loader_path)

                    # Handle libs relative to @rpath.
                    fixed_lib = fixed_lib.replace("@rpath", path)

                    if os.path.exists(fixed_lib):
                        lib = fixed_lib
                        break

                    # Try to handle other relative libs.
                    elif not os.path.isabs(fixed_lib):
                        candidate = os.path.join(loader_path, fixed_lib)
                        if not os.path.exists(candidate):
                            candidate = os.path.join(path, fixed_lib)
                        if os.path.exists(candidate):
                            info("Resolved relative dependency {0} as {1}".format(fixed_lib, candidate))
                            lib = candidate
                            break

            libs.add(lib)

        if verbose:
            trace("  Dependencies for file {0}:".format(filepath))
            for lib in libs:
                if os.path.isfile(lib):
                    trace(u"      {0} {1}".format(GREEN_CHECKMARK, lib))
                else:
                    trace(u"      {0} {1}".format(RED_CROSSMARK, lib))

        # Don't warn about missing dependencies if we didn't attempt to fix them.
        if fix_paths:
            for lib in libs:
                if not os.path.isfile(lib):
                    warning("  Dependency {0} could not be found on disk".format(lib))
                    warning("   searchpaths: {0}".format(search_paths))

        return libs

    def __get_qt_frameworks_for_file(self, filepath):
        libs = set()
        for lib in self.__iter_all_dependencies(filepath):
            if re.search(r"Qt.*\.framework", lib):
                libs.add(lib)

        return libs

    def __is_system_lib(self, lib):
        for prefix in self.SYSTEM_LIBS_PREFIXES:
            if lib.startswith(prefix):
                return True
        return False

    def __create_icon(self, image, icon_path):
        icon_set = "appleseed.iconset"
        safe_make_directory(icon_set)

        for size in (16, 32, 64, 128, 256, 1024, 2048):
            if size < 1024:
                name = "icon_{0}x{0}.png".format(size)
                cmd = ['sips', '-z', str(size), str(size), image, '--out', os.path.join(icon_set, name)]
                # pipe stderr to stdout to silence verbosity
                subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            if size > 16:
                name = "icon_{0}x{0}@2x.png".format(size/2)
                cmd = ['sips', '-z', str(size), str(size), image, '--out', os.path.join(icon_set, name)]
                # pipe stderr to stdout to silence verbosity
                subprocess.check_output(cmd, stderr=subprocess.STDOUT)

        subprocess.check_call(['iconutil', '--convert', 'icns', icon_set, '-o', icon_path])
        safe_delete_directory(icon_set)

    def __create_app(self, staging_dir, app_path, icon_path):
        # cleanup old app
        safe_delete_directory(app_path)

        contents = os.path.join(app_path, "Contents")

        shutil.move(staging_dir, contents)

        resources = os.path.join(contents, "Resources")
        safe_make_directory(resources)

        macos = os.path.join(contents, "MacOS")
        safe_make_directory(macos)

        # relative symlink for executable
        os.symlink('../bin/appleseed-studio', os.path.join(macos, 'appleseed'))

        icon = os.path.join(resources, "appleseed.icns")
        self.__create_icon(icon_path, icon)

        with open(os.path.join(contents, "Info.plist"), 'wb') as f:
            f.write(b'<?xml version="1.0" encoding="UTF-8"?>\n')
            f.write(b'<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">\n')
            f.write(b'<plist version="1.0">\n')
            f.write(b'<dict>\n')
            f.write(b'  <key>NSPrincipalClass</key>\n')
            f.write(b'  <string>NSApplication</string>\n')
            f.write(b'  <key>CFBundlePackageType</key>\n')
            f.write(b'  <string>APPL</string>\n')
            f.write(b'  <key>CFBundleExecutable</key>\n')
            f.write(b'  <string>appleseed</string>\n')
            f.write(b'  <key>CFBundleIconFile</key>\n')
            f.write(b'  <string>appleseed.icns</string>\n')
            f.write(b'  <key>CFBundleIdentifier</key>\n')
            f.write(b'  <string>net.appleseedhq.appleseed</string>\n')
            f.write(b'  <key>CFBundleShortVersionString</key>\n')
            f.write(b'  <string>2.1.0</string>\n')
            f.write(b'</dict>\n')
            f.write(b'</plist>\n')

    def _create_dmg(self, app_path, dmg_path, background_path, drive_icon_path):
        drive_icon = 'appleseed-drive.icns'
        self.__create_icon(drive_icon_path, drive_icon)

        settings = {
            "title": "appleseed",
            "background":  background_path,
            "icon": drive_icon,
            "format": "UDZO",
            "compression-level": 9,
            "window": {"position": {"x": 100, "y": 100}, "size": {"width": 640, "height": 280}},
            "contents": [
                {"x": 140, "y": 120, "type": "file", "path": app_path},
                {"x": 500, "y": 120, "type": "link", "path": "/Applications"},
            ]
        }

        settings_file = "settings.json"
        with open(settings_file, 'wb') as f:
            json.dump(settings, f)

        output_dir = os.path.dirname(dmg_path)
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        # use dmgbuild to create disk image
        # https://github.com/al45tair/dmgbuild
        cmd = ['dmgbuild', '-s', settings_file, 'appleseed', dmg_path]
        subprocess.check_call(cmd)

        # cleanup
        safe_delete_file(drive_icon)
        safe_delete_file(settings_file)


# -------------------------------------------------------------------------------------------------
# Linux package builder.
# -------------------------------------------------------------------------------------------------

class LinuxPackageBuilder(PackageBuilder):

    SHARED_LIB_EXT = ".so"

    SYSTEM_LIBS_PREFIXES = [
        "/lib64/ld-linux-",
        "libc.so",
        "libdl",
        "libdrm",
        "libffi",
        "libfontconfig",
        "libfreetype",
        "libgcc",
        "libGL",
        "libglapi",
        "libglib",
        "libgobject",
        "libgthread",
        "libICE",
        "libm.so",
        "libnsl",
        "libpthread",
        "libpython",
        "librt",
        "libselinux",
        "libSM",
        "libstdc++",
        "libutil",
        "libuuid",
        "libX",
        "libxcb",
        "libxshmfence.so",
        "linux"
    ]

    def postprocess_stage(self):
        self.__make_executable(os.path.join("appleseed/bin", "maketx"))
        self.__make_executable(os.path.join("appleseed/bin", "oiiotool"))
        self.__make_executable(os.path.join("appleseed/bin", "idiff"))
        self.__make_executable(os.path.join("appleseed/bin", "oslc"))
        self.__make_executable(os.path.join("appleseed/bin", "oslinfo"))
        self.__add_dependencies_to_stage()
        self.__set_runtime_paths_on_binaries("appleseed/bin/", "$ORIGIN/../lib")
        self.__set_runtime_paths_on_libraries("appleseed/bin/", "$ORIGIN/../../lib")  # Qt plugins
        self.__set_runtime_paths_on_libraries("appleseed/lib/", "$ORIGIN/../lib")
        self.__add_python_to_stage()  # must be last

    def __make_executable(self, filepath):
        mode = os.stat(filepath)[stat.ST_MODE]
        mode |= stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
        os.chmod(filepath, mode)

    def __add_dependencies_to_stage(self):
        progress("Linux-specific: Adding dependencies to staging directory")

        # Qt platform plugins.
        safe_make_directory("appleseed/bin/platforms")
        qt_platform_plugins_path = os.path.join(self.settings.qt_runtime_path, "plugins", "platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "libqeglfs.so"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "libqminimalegl.so"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "libqminimal.so"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "libqoffscreen.so"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "libqvnc.so"), "appleseed/bin/platforms")
        shutil.copy(os.path.join(qt_platform_plugins_path, "libqxcb.so"), "appleseed/bin/platforms")

        self.add_unix_dependencies_to_stage(self.get_paths_to_binaries())

    def __set_runtime_paths_on_binaries(self, root_path, rpath):
        progress("Linux-specific: Setting runtime paths on binaries in {0}".format(root_path))
        for dirpath, dirnames, filenames in os.walk(root_path):
            for filename in filenames:
                ext = os.path.splitext(filename)[1]
                # need to skip shared libs because we don't want to patch Qt plugins
                if ext != ".py" and ext != ".conf" and not self.__is_shared_lib(filename):
                    self.run("patchelf --set-rpath '{0}' {1}".format(rpath, os.path.join(dirpath, filename)))
                else:
                    trace("  Skipping {0}".format(filename))

    def __set_runtime_paths_on_libraries(self, root_path, rpath):
        progress("Linux-specific: Setting runtime paths on libraries in {0}".format(root_path))
        for dirpath, dirnames, filenames in os.walk(root_path):
            for filename in filenames:
                if self.__is_shared_lib(filename):
                    self.run("patchelf --set-rpath '{0}' {1}".format(rpath, os.path.join(dirpath, filename)))
                else:
                    trace("  Skipping {0}".format(filename))

    def get_dependencies_for_file(self, filepath):
        returncode, out, err = self.run_subprocess(["ldd", filepath])
        if returncode != 0:
            fatal("Failed to invoke ldd(1) to get dependencies for {0}: {1}".format(filepath, err))

        libs = set()

        for line in out.split("\n"):
            line = line.strip()

            # Ignore empty lines.
            if len(line) == 0:
                continue

            # Ignore libs without any dyanmic dependencies.
            if "statically linked" in line:
                continue

            # Ignore system libs.
            if self.__is_system_lib(line):
                continue

            libs.add(line.split()[2])

        if libs:
            trace("  Dependencies for {0}:".format(filepath))
            for lib in libs:
                trace("      {0}".format(lib))
        else:
            trace("  Dependencies for {0}: None".format(filepath))

        return libs

    def __is_shared_lib(self, filename):
        return filename.endswith(".so") or ".so." in filename

    def __is_system_lib(self, lib):
        for prefix in self.SYSTEM_LIBS_PREFIXES:
            if lib.startswith(prefix):
                return True
        return False

    def __add_python_to_stage(self):
        progress("Linux-specific: Adding Python 2.7 to staging directory")
        merge_tree(os.path.join(self.settings.python_path, "lib"), "appleseed/lib", symlinks=True)
        merge_tree(os.path.join(self.settings.python_path, "include"), "appleseed/include", symlinks=True)


# -------------------------------------------------------------------------------------------------
# Entry point.
# -------------------------------------------------------------------------------------------------

def main():
    colorama.init()

    parser = argparse.ArgumentParser(description="build an appleseed package from sources")

    parser.add_argument("--nozip", help="do not build a final zip file. Files will be copied to staging directory only", action="store_true")
    parser.add_argument("--noapp", help="do not build a macOS app. Files will be stored in directory format only", action="store_true")

    args = parser.parse_args()

    no_zip = args.nozip
    no_app = True
    if sys.platform == 'darwin':
        no_app = args.noapp

    print_runtime_details("appleseed.package", VERSION, os.path.realpath(__file__))

    print("VERY IMPORTANT:")
    print("")
    print("  - Make sure there are no obsolete binaries in sandbox/bin/ and sandbox/lib/")
    print("  - You may need to run this tool with sudo on Linux and macOS")
    print("  - On Linux, you may need to set $LD_LIBRARY_PATH to allow ldd(1) to find third party shared libraries")
    print("  - On macOS, you may need to set $DYLD_LIBRARY_PATH to allow script to find third party shared libraries")
    print("  - You need to have dmgbuild installed and in $PATH on macOS to build .dmg file: pip install dmgbuild")
    print("")

    settings = Settings()
    settings.load()
    settings.print_summary()

    package_info = PackageInfo(settings, no_zip,  no_app)
    package_info.load()
    package_info.print_summary()

    if os.name == "nt":
        package_builder = WindowsPackageBuilder(settings, package_info)
    elif os.name == "posix" and platform.mac_ver()[0] != "":
        package_builder = MacPackageBuilder(settings, package_info)
    elif os.name == "posix" and platform.mac_ver()[0] == "":
        package_builder = LinuxPackageBuilder(settings, package_info)
    else:
        fatal("Unsupported platform: {0}".format(os.name))

    package_builder.build_package()

    package_info.print_summary()


if __name__ == "__main__":
    main()
