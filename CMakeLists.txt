
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

cmake_minimum_required(VERSION 2.6)

# CMP0008: Libraries linked by full-path must have a valid library file name.
cmake_policy(SET CMP0008 NEW)

# Replace slashes by backslashes in a string.
macro(slashes_to_backslashes output input)
    string(REGEX REPLACE "/" "\\\\" ${output} ${input})
endmacro()

# Filter a list of items using a regular expression.
macro(filter_list output_list input_list regex)
    foreach(item ${input_list})
        if(${item} MATCHES ${regex})
            list(APPEND ${output_list} ${item})
        endif()
    endforeach()
endmacro()

# Copy a target binary to the sandbox.
macro(add_copy_target_to_sandbox_command target)
    if(WIN32 AND MSVC90)
        slashes_to_backslashes(project_source_dir_win ${PROJECT_SOURCE_DIR})
        add_custom_command(TARGET ${target} POST_BUILD
            COMMAND mkdir ${project_source_dir_win}\\sandbox\\bin\\win32.vs90\\$(ConfigurationName) 2> nul
            COMMAND copy $(TargetPath) ${project_source_dir_win}\\sandbox\\bin\\win32.vs90\\$(ConfigurationName)
        )
    endif()
endmacro()

# Create the configurations. Must come before project().
set(CMAKE_CONFIGURATION_TYPES "Debug;Release;Profile;Ship" CACHE STRING "" FORCE)

project(appleseed)

add_subdirectory(src)
