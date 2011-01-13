
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2011 Francois Beaune
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

cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

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

# Convert a semicolon-separated list to a whitespace-separated string.
macro(convert_list_to_string output_string input_list)
    foreach(item ${input_list})
        if(DEFINED ${output_string})
            set(${output_string} "${${output_string}} ${item}")
        else()
            set(${output_string} "${item}")
        endif()
    endforeach()
endmacro()

# Assign a whitespace-separated string to a variable, given a list.
macro(set_to_string output_variable first_arg)
    set(arg_list ${first_arg} ${ARGN})
    convert_list_to_string(${output_variable} "${arg_list}")
endmacro()

#
# A variant of the QT4_WRAP_CPP macro designed to moc .cpp files.
#
# It differs from the original macro in two ways:
#
#   1.  It generates a file of the form moc_cpp_XXX.cxx for a given input file XXX.cpp,
#       whereas the original macro generates a file of the form moc_XXX.cxx for a given
#       input file XXX.h.
#
#   2.  It adds a dependency between the input file XXX.cpp and the generated file so
#       that the generated file gets rebuild whenever the input file is modified. This
#       is necessary because the generated file is to be included in the input file.
#
# The original QT4_WRAP_CPP macro can be found in share/cmake-2.8/Modules/Qt4Macros.cmake.
#

MACRO(QT4_WRAP_CPP_CPLUSPLUS_FILES outfiles)
    QT4_GET_MOC_FLAGS(moc_flags)
    QT4_EXTRACT_OPTIONS(moc_files moc_options ${ARGN})
    FOREACH(it ${moc_files})
        GET_FILENAME_COMPONENT(it ${it} ABSOLUTE)
        QT4_MAKE_OUTPUT_FILE(${it} moc_cpp_ cxx outfile)
        QT4_CREATE_MOC_COMMAND(${it} ${outfile} "${moc_flags}" "${moc_options}")
        SET(${outfiles} ${${outfiles}} ${outfile})
        SET_SOURCE_FILES_PROPERTIES(${it} PROPERTIES OBJECT_DEPENDS ${outfile})
    ENDFOREACH(it)
ENDMACRO()

# Create the configurations. Must come before project().
set(CMAKE_CONFIGURATION_TYPES "Debug;Release;Profile;Ship" CACHE STRING "" FORCE)

# Select a configuration, if none is selected yet.
if(NOT CMAKE_BUILD_TYPE)
    convert_list_to_string(config_types_string ${CMAKE_CONFIGURATION_TYPES})
    set(CMAKE_BUILD_TYPE Release
        CACHE STRING "Choose the type of build, options are: ${config_types_string}" FORCE)
endif()

project(appleseed)

# Build the products from source.
add_subdirectory(src)
