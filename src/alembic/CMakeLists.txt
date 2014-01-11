
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


#--------------------------------------------------------------------------------------------------
# CMake configuration.
#--------------------------------------------------------------------------------------------------

cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

cmake_policy (SET CMP0008 NEW)  # libraries linked by full-path must have a valid library file name

project (alembic)


#--------------------------------------------------------------------------------------------------
# Platform-specific settings.
#--------------------------------------------------------------------------------------------------

# This must come after project () otherwise CMAKE_SYSTEM_NAME is undefined.
if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set (WINDOWS TRUE)
    include (../cmake/config/win-vs110.txt)
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set (DARWIN TRUE)
    include (../cmake/config/mac-gcc4.txt)
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set (LINUX TRUE)
    include (../cmake/config/linux-gcc4.txt)
else ()
    message (FATAL_ERROR "Unsupported platform or compiler (CMAKE_SYSTEM_NAME is \"${CMAKE_SYSTEM_NAME}\"). "
                         "Edit ${CMAKE_CURRENT_LIST_FILE} to add support for it.")
endif ()


#--------------------------------------------------------------------------------------------------
# Output directory.
#--------------------------------------------------------------------------------------------------

set (LIBRARY_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/../../build/${platform}/alembic)


#--------------------------------------------------------------------------------------------------
# Version information.
#--------------------------------------------------------------------------------------------------

set (PROJECT_VERSION_MAJOR 1)
set (PROJECT_VERSION_MINOR 0)
set (PROJECT_VERSION_PATCH 1)
set (PROJECT_VERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH})
set (VERSION ${PROJECT_VERSION})


#--------------------------------------------------------------------------------------------------
# Boost libraries.
#--------------------------------------------------------------------------------------------------

set (Boost_MULTITHREADED TRUE)
set (Boost_USE_STATIC_LIBS TRUE)

find_package (Boost 1.44 REQUIRED thread)

include_directories (${Boost_INCLUDE_DIRS})


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

include_directories (
    include
    lib
    ../openexr/include/OpenEXR
    ../hdf5/hl/src
    ../hdf5/build       # for H5pubconf.h (a generated file)
    ../hdf5/src
)


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    add_definitions(
        -DPLATFORM_WINDOWS -DPLATFORM=WINDOWS
    )
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    add_definitions(
        #-Wno-long-double     # for osx versions before snow leopard
        -DPLATFORM_DARWIN -DPLATFORM=DARWIN
    )
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    add_definitions(
        -DPLATFORM_LINUX -DPLATFORM=LINUX
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

macro (ak_common_append_files ret ext)
  foreach (_dir ${ARGN})
    file (GLOB _files "${_dir}/${ext}")
    list (APPEND ${ret} ${_files})
  endforeach ()
endmacro ()

ak_common_append_files (alembic_sources "*.cpp" lib/Alembic/Abc)
ak_common_append_files (alembic_sources "*.cpp" lib/Alembic/AbcCoreAbstract)
ak_common_append_files (alembic_sources "*.cpp" lib/Alembic/AbcCoreHDF5)
ak_common_append_files (alembic_sources "*.cpp" lib/Alembic/AbcGeom)
ak_common_append_files (alembic_sources "*.cpp" lib/Alembic/MD5Hash)
ak_common_append_files (alembic_sources "*.cpp" lib/Alembic/Util)


#--------------------------------------------------------------------------------------------------
# Target.
#--------------------------------------------------------------------------------------------------

add_library (alembic STATIC
    ${alembic_sources}
)
