# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/rishav/Desktop/appleseed

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/rishav/Desktop/appleseed/code

# Utility rule file for appleseed.python.copy_py_files.

# Include the progress variables for this target.
include src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/progress.make

appleseed.python.copy_py_files: src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/build.make
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp /home/rishav/Desktop/appleseed/src/appleseed.python/__init__.py /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp /home/rishav/Desktop/appleseed/src/appleseed.python/logtarget.py /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp /home/rishav/Desktop/appleseed/src/appleseed.python/textureconverter.py /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp /home/rishav/Desktop/appleseed/src/appleseed.python/studio/__init__.py /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp /home/rishav/Desktop/appleseed/src/appleseed.python/studio/Qt.py /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp /home/rishav/Desktop/appleseed/src/appleseed.python/studio/ui.py /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp /home/rishav/Desktop/appleseed/src/appleseed.python/studio/plugins.py /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed/studio
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && mkdir -p /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && cp -r /home/rishav/Desktop/appleseed/src/appleseed.python/test /home/rishav/Desktop/appleseed/sandbox/lib/Release/python2.7/appleseed
.PHONY : appleseed.python.copy_py_files

# Rule to build all files generated by this target.
src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/build: appleseed.python.copy_py_files

.PHONY : src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/build

src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/clean:
	cd /home/rishav/Desktop/appleseed/code/src/appleseed.python && $(CMAKE_COMMAND) -P CMakeFiles/appleseed.python.copy_py_files.dir/cmake_clean.cmake
.PHONY : src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/clean

src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/depend:
	cd /home/rishav/Desktop/appleseed/code && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/rishav/Desktop/appleseed /home/rishav/Desktop/appleseed/src/appleseed.python /home/rishav/Desktop/appleseed/code /home/rishav/Desktop/appleseed/code/src/appleseed.python /home/rishav/Desktop/appleseed/code/src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/appleseed.python/CMakeFiles/appleseed.python.copy_py_files.dir/depend

