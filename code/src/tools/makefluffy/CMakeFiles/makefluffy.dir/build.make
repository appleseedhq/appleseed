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

# Include any dependencies generated for this target.
include src/tools/makefluffy/CMakeFiles/makefluffy.dir/depend.make

# Include the progress variables for this target.
include src/tools/makefluffy/CMakeFiles/makefluffy.dir/progress.make

# Include the compile flags for this target's objects.
include src/tools/makefluffy/CMakeFiles/makefluffy.dir/flags.make

src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o: src/tools/makefluffy/CMakeFiles/makefluffy.dir/flags.make
src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o: ../src/tools/makefluffy/commandlinehandler.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/rishav/Desktop/appleseed/code/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o"
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && /usr/bin/c++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o -c /home/rishav/Desktop/appleseed/src/tools/makefluffy/commandlinehandler.cpp

src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/makefluffy.dir/commandlinehandler.cpp.i"
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/rishav/Desktop/appleseed/src/tools/makefluffy/commandlinehandler.cpp > CMakeFiles/makefluffy.dir/commandlinehandler.cpp.i

src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/makefluffy.dir/commandlinehandler.cpp.s"
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/rishav/Desktop/appleseed/src/tools/makefluffy/commandlinehandler.cpp -o CMakeFiles/makefluffy.dir/commandlinehandler.cpp.s

src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.requires:

.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.requires

src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.provides: src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.requires
	$(MAKE) -f src/tools/makefluffy/CMakeFiles/makefluffy.dir/build.make src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.provides.build
.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.provides

src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.provides.build: src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o


src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o: src/tools/makefluffy/CMakeFiles/makefluffy.dir/flags.make
src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o: ../src/tools/makefluffy/main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/rishav/Desktop/appleseed/code/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o"
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && /usr/bin/c++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/makefluffy.dir/main.cpp.o -c /home/rishav/Desktop/appleseed/src/tools/makefluffy/main.cpp

src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/makefluffy.dir/main.cpp.i"
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/rishav/Desktop/appleseed/src/tools/makefluffy/main.cpp > CMakeFiles/makefluffy.dir/main.cpp.i

src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/makefluffy.dir/main.cpp.s"
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/rishav/Desktop/appleseed/src/tools/makefluffy/main.cpp -o CMakeFiles/makefluffy.dir/main.cpp.s

src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.requires:

.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.requires

src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.provides: src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.requires
	$(MAKE) -f src/tools/makefluffy/CMakeFiles/makefluffy.dir/build.make src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.provides.build
.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.provides

src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.provides.build: src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o


# Object files for target makefluffy
makefluffy_OBJECTS = \
"CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o" \
"CMakeFiles/makefluffy.dir/main.cpp.o"

# External object files for target makefluffy
makefluffy_EXTERNAL_OBJECTS =

src/tools/makefluffy/makefluffy: src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o
src/tools/makefluffy/makefluffy: src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o
src/tools/makefluffy/makefluffy: src/tools/makefluffy/CMakeFiles/makefluffy.dir/build.make
src/tools/makefluffy/makefluffy: src/appleseed.shared/libappleseed.shared.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_atomic-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_chrono-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_date_time-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_filesystem-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_regex-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_system-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_thread-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_wave-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /usr/lib/x86_64-linux-gnu/libpthread.so
src/tools/makefluffy/makefluffy: src/appleseed/libappleseed.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libpng.so
src/tools/makefluffy/makefluffy: /home/rishav/anaconda3/lib/libz.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libxerces-c.so
src/tools/makefluffy/makefluffy: /home/rishav/anaconda3/lib/libz.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libxerces-c.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libOpenImageIO.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/liboslexec.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/liboslcomp.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/liboslquery.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libSeExpr.so
src/tools/makefluffy/makefluffy: src/thirdparty/bcd/libbcd.a
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libHalf.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libIex.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libImath.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libIlmImf.so
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libIlmThread.so
src/tools/makefluffy/makefluffy: src/thirdparty/lz4/liblz4.a
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_atomic-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_chrono-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_date_time-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_filesystem-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_regex-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_system-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_thread-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /home/rishav/Desktop/prebuilt-linux-deps/lib/libboost_wave-gcc48-mt-1_61.so.1.61.0
src/tools/makefluffy/makefluffy: /usr/lib/x86_64-linux-gnu/libpthread.so
src/tools/makefluffy/makefluffy: src/tools/makefluffy/CMakeFiles/makefluffy.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/rishav/Desktop/appleseed/code/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Linking CXX executable makefluffy"
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/makefluffy.dir/link.txt --verbose=$(VERBOSE)
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && mkdir -p /home/rishav/Desktop/appleseed/sandbox/bin/Release
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && cp /home/rishav/Desktop/appleseed/code/src/tools/makefluffy/makefluffy /home/rishav/Desktop/appleseed/sandbox/bin/Release

# Rule to build all files generated by this target.
src/tools/makefluffy/CMakeFiles/makefluffy.dir/build: src/tools/makefluffy/makefluffy

.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/build

src/tools/makefluffy/CMakeFiles/makefluffy.dir/requires: src/tools/makefluffy/CMakeFiles/makefluffy.dir/commandlinehandler.cpp.o.requires
src/tools/makefluffy/CMakeFiles/makefluffy.dir/requires: src/tools/makefluffy/CMakeFiles/makefluffy.dir/main.cpp.o.requires

.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/requires

src/tools/makefluffy/CMakeFiles/makefluffy.dir/clean:
	cd /home/rishav/Desktop/appleseed/code/src/tools/makefluffy && $(CMAKE_COMMAND) -P CMakeFiles/makefluffy.dir/cmake_clean.cmake
.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/clean

src/tools/makefluffy/CMakeFiles/makefluffy.dir/depend:
	cd /home/rishav/Desktop/appleseed/code && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/rishav/Desktop/appleseed /home/rishav/Desktop/appleseed/src/tools/makefluffy /home/rishav/Desktop/appleseed/code /home/rishav/Desktop/appleseed/code/src/tools/makefluffy /home/rishav/Desktop/appleseed/code/src/tools/makefluffy/CMakeFiles/makefluffy.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/tools/makefluffy/CMakeFiles/makefluffy.dir/depend

