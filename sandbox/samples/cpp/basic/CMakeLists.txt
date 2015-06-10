
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


#--------------------------------------------------------------------------------------------------
# CMake configuration.
#--------------------------------------------------------------------------------------------------

cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

project (basic)

set (CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/../../../share/cmake/Modules;${PROJECT_SOURCE_DIR}/../../../../src/cmake/Modules)


#--------------------------------------------------------------------------------------------------
# Build options.
#--------------------------------------------------------------------------------------------------

option (USE_EXTERNAL_OIIO       "Use external OpenImageIO library"                      OFF)
option (USE_EXTERNAL_OSL        "Use external OpenShadingLanguage library"              OFF)

option (WITH_OSL                "Build OSL support"                                     OFF)

option (USE_STATIC_BOOST        "Use static Boost libraries"                            ON)
option (USE_STATIC_OIIO         "Use static OpenImageIO libraries"                      ON)
option (USE_STATIC_OSL          "Use static OpenShadingLanguage libraries"              ON)


#--------------------------------------------------------------------------------------------------
# Boost libraries.
#--------------------------------------------------------------------------------------------------

set (Boost_MULTITHREADED TRUE)

if (USE_STATIC_BOOST)
    set (Boost_USE_STATIC_LIBS TRUE)
endif ()

find_package (Boost 1.47 REQUIRED ${BOOST_NEEDED_LIBS})

add_definitions (-DBOOST_FILESYSTEM_VERSION=3 -DBOOST_FILESYSTEM_NO_DEPRECATED)

include_directories (${Boost_INCLUDE_DIRS})
link_directories (${Boost_LIBRARY_DIRS})


#--------------------------------------------------------------------------------------------------
# Find external packages.
#--------------------------------------------------------------------------------------------------

find_package (Appleseed)
find_package (Imath)
find_package (OpenEXR)

if (WITH_OSL)
    set (WITH_OIIO TRUE)
endif ()

if (WITH_OIIO)
    add_definitions (-DAPPLESEED_WITH_OIIO)
    if (USE_STATIC_OIIO)
        add_definitions (-DOIIO_STATIC_BUILD)
    endif ()
    if (USE_EXTERNAL_OIIO)
        find_package (OpenImageIO REQUIRED)
    endif ()
endif ()

if (WITH_OSL)
    add_definitions (-DAPPLESEED_WITH_OSL)
    if (USE_STATIC_OSL)
        add_definitions (-DOSL_STATIC_LIBRARY)
    endif ()
    if (USE_EXTERNAL_OSL)
        find_package (OSL REQUIRED)
    endif ()
endif ()


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

include_directories (${APPLESEED_INCLUDE_DIRS})
include_directories (${IMATH_INCLUDE_DIRS})
include_directories (${OPENEXR_INCLUDE_DIRS})

if (WITH_OIIO)
    if (USE_EXTERNAL_OIIO)
        include_directories (${OPENIMAGEIO_INCLUDE_DIRS})
    else ()
        include_directories (${APPLESEED_DEPS_STAGE_DIR}/oiio-debug/include)
    endif ()
endif ()

if (WITH_OSL)
    if (USE_EXTERNAL_OSL)
        include_directories (${OSL_INCLUDE_DIRS})
    else ()
        include_directories (${APPLESEED_DEPS_STAGE_DIR}/osl-debug/include)
    endif ()
endif ()


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

if (MSVC)
    add_definitions (/D "_CRT_SECURE_NO_WARNINGS")
endif ()


#--------------------------------------------------------------------------------------------------
# Product.
#--------------------------------------------------------------------------------------------------

add_executable (basic basic.cpp)

target_link_libraries (basic
    ${APPLESEED_LIBRARIES}
    ${IMATH_LIBRARIES}
    ${OPENEXR_LIBRARIES}
)
