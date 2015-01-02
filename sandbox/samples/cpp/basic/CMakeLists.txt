
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013 Nicholas Yue, Jupiter Jazz Limited
# Copyright (c) 2014-2015 Nicholas Yue, The appleseedhq Organization
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


cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

project (basic)

set (CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/../../../share/cmake/Modules;${PROJECT_SOURCE_DIR}/../../../../src/cmake/Modules)

find_package (Appleseed)
find_package (Boost)
find_package (Imath)
find_package (OpenEXR)

include_directories (${APPLESEED_INCLUDE_DIRS})
include_directories (${Boost_INCLUDE_DIR})
include_directories (${IMATH_INCLUDE_DIRS})
include_directories (${OPENEXR_INCLUDE_DIRS})

link_directories (${Boost_LIBRARY_DIRS})

if (MSVC)
    add_definitions (/D "_CRT_SECURE_NO_WARNINGS")
endif ()

add_executable (basic basic.cpp)

target_link_libraries (basic
    ${APPLESEED_LIBRARIES}
    ${IMATH_LIBRARIES}
    ${OPENEXR_LIBRARIES}
)
