
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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


#--------------------------------------------------------------------------------------------------
# CMake configuration.
#--------------------------------------------------------------------------------------------------

cmake_minimum_required (VERSION 3.9 FATAL_ERROR)

project (proceduraltexture)

set (CMAKE_MODULE_PATH
    ${PROJECT_SOURCE_DIR}/../../../share/cmake/Modules
    ${PROJECT_SOURCE_DIR}/../../../../cmake/modules
)


#--------------------------------------------------------------------------------------------------
# Build options.
#--------------------------------------------------------------------------------------------------

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    option (USE_FIND_PACKAGE_FOR_OIIO "Use find_package() for OpenImageIO library"         OFF)
else ()
    set (USE_FIND_PACKAGE_FOR_OIIO ON)
endif ()

option (USE_STATIC_OIIO     "Use static OpenImageIO libraries"          ON)


#--------------------------------------------------------------------------------------------------
# Compile flags.
#--------------------------------------------------------------------------------------------------

if (UNIX)
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

    # Remove when C++11 support improves.
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-declarations")
endif ()


#--------------------------------------------------------------------------------------------------
# Find packages.
#--------------------------------------------------------------------------------------------------

find_package (Imath)

if (USE_STATIC_OIIO)
    add_definitions (-DOIIO_STATIC_BUILD)
endif ()
if (USE_FIND_PACKAGE_FOR_OIIO)
    find_package (OpenImageIO REQUIRED)
endif ()


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

include_directories (${IMATH_INCLUDE_DIRS})

if (USE_FIND_PACKAGE_FOR_OIIO)
    include_directories (${OPENIMAGEIO_INCLUDE_DIRS})
else ()
    include_directories (${APPLESEED_DEPS_STAGE_DIR}/oiio-debug/include)
endif ()


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

if (MSVC)
    add_definitions (/D "_CRT_SECURE_NO_WARNINGS")
    add_definitions (/D "_SCL_SECURE_NO_WARNINGS")
endif ()


#--------------------------------------------------------------------------------------------------
# Product.
#--------------------------------------------------------------------------------------------------

add_library (proceduraltexture.imageio SHARED proceduraltexture.cpp)
set_target_properties (proceduraltexture.imageio PROPERTIES PREFIX "")

target_link_libraries (proceduraltexture.imageio
    ${IMATH_LIBRARIES}
    ${OPENIMAGEIO_LIBRARIES}
)
