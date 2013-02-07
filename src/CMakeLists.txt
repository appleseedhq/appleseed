
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

include (cmake/utilities.txt)


#--------------------------------------------------------------------------------------------------
# Build configurations.
#--------------------------------------------------------------------------------------------------

# Create the build configurations. Must come before project ().
set (CMAKE_CONFIGURATION_TYPES "Debug;Release;Profile;Ship" CACHE STRING "" FORCE)

# Select a build configuration if none is selected yet.
if (NOT CMAKE_BUILD_TYPE)
    convert_list_to_string (config_types_string ${CMAKE_CONFIGURATION_TYPES})
    set (CMAKE_BUILD_TYPE Ship
         CACHE STRING "Choose the type of build, options are: ${config_types_string}" FORCE)
endif ()

project (appleseed)

# Setup the path for our FindPackage modules.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake/Modules)


#--------------------------------------------------------------------------------------------------
# Build options.
#--------------------------------------------------------------------------------------------------

option (USE_STATIC_BOOST        "Use static Boost libs"         ON)
option (USE_EXTERNAL_PNG        "Use external PNG lib"          OFF)
option (USE_EXTERNAL_ALEMBIC    "Use external Alembic lib"      OFF)
option (USE_EXTERNAL_XERCES     "Use external Xerces lib"       OFF)
option (WITH_CLI                "Build appleseed.cli"           ON)
option (WITH_STUDIO             "Build appleseed.studio"        ON)
option (WITH_PYTHON             "Build Python module"           ON)
option (WITH_TOOLS              "Build appleseed tools"         ON)


#--------------------------------------------------------------------------------------------------
# Platform-specific settings.
#--------------------------------------------------------------------------------------------------

# This must come after project () otherwise CMAKE_SYSTEM_NAME is undefined.
if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    include (cmake/config/win-vs110.txt)
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    include (cmake/config/mac-gcc4.txt)
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    include (cmake/config/linux-gcc4.txt)
else ()
    message (FATAL_ERROR "Unsupported platform or compiler (CMAKE_SYSTEM_NAME is \"${CMAKE_SYSTEM_NAME}\"). "
                         "Edit ${CMAKE_CURRENT_LIST_FILE} to add support for it.")
endif ()


#--------------------------------------------------------------------------------------------------
# Version information.
#--------------------------------------------------------------------------------------------------

# Extract git tag.
exec_program (
    ${git_command}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ARGS "describe --long"
    OUTPUT_VARIABLE git_tag
)


#--------------------------------------------------------------------------------------------------
# Boost libraries.
#--------------------------------------------------------------------------------------------------

set (Boost_MULTITHREADED TRUE)

if (USE_STATIC_BOOST)
    set (Boost_USE_STATIC_LIBS TRUE)
endif ()

set (BOOST_NEEDED_LIBS chrono date_time filesystem regex system thread)
find_package (Boost 1.47 REQUIRED ${BOOST_NEEDED_LIBS})

add_definitions (-DBOOST_FILESYSTEM_VERSION=3 -DBOOST_FILESYSTEM_NO_DEPRECATED)

include_directories (${Boost_INCLUDE_DIRS})
link_directories (${Boost_LIBRARY_DIRS})


#--------------------------------------------------------------------------------------------------
# Find external packages.
#--------------------------------------------------------------------------------------------------

if (USE_EXTERNAL_PNG)
    find_package (PNG REQUIRED)
    add_definitions (-DWITH_EXTERNAL_PNG)
endif ()

if (USE_EXTERNAL_ALEMBIC)
    find_package (ZLIB REQUIRED)
    find_package (Imath REQUIRED)
    find_package (OpenEXR REQUIRED)
    find_package (HDF5 REQUIRED COMPONENTS C HL)
    find_package (Alembic REQUIRED)
    add_definitions (-DWITH_EXTERNAL_ALEMBIC)
endif ()

if (USE_EXTERNAL_XERCES)
    find_package (Xerces REQUIRED)
    add_definitions (-DWITH_EXTERNAL_XERCES)
endif ()


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

# Common include paths.
include_directories (
    appleseed
    STANN/include
    ${PYTHON_INCLUDE_DIRS}
)

if (USE_EXTERNAL_PNG)
    include_directories (${PNG_INCLUDE_DIR})
else ()
    include_directories (libpng)
endif ()

if (USE_EXTERNAL_ALEMBIC)
    include_directories (
        ${ZLIB_INCLUDE_DIRS}
        ${OPENEXR_INCLUDE_DIR}
        ${OPENEXR_INCLUDE_DIR}/OpenEXR/
        ${ILMBASE_INCLUDE_DIR}
        ${ILMBASE_INCLUDE_DIR}/OpenEXR/
        ${HDF5_INCLUDE_DIRS}
        ${ALEMBIC_INCLUDE_DIRS}
    )
else ()
    include_directories (
        alembic/lib
        openexr/include
        openexr/include/OpenEXR
        zlib
    )
endif ()

if (USE_EXTERNAL_XERCES)
    include_directories (${XERCES_INCLUDE_DIRS})
else ()
    include_directories (xerces-c/src)
endif ()


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

# All configurations.
set (preprocessor_definitions_common
    ${preprocessor_definitions_common}
    APPLESEED_VERSION_STRING="${git_tag}"
)
if (CMAKE_SIZEOF_VOID_P MATCHES 4)
    set (preprocessor_definitions_common
        ${preprocessor_definitions_common}
        ARCH32
)
elseif (CMAKE_SIZEOF_VOID_P MATCHES 8)
    set (preprocessor_definitions_common
        ${preprocessor_definitions_common}
        ARCH64
)
else ()
    message (FATAL_ERROR "Cannot determine pointer size")
endif ()

# Debug configuration.
set (preprocessor_definitions_debug
    ${preprocessor_definitions_debug}
    ${preprocessor_definitions_common}
    DEBUG
    _DEBUG
)

# Release configuration.
set (preprocessor_definitions_release
    ${preprocessor_definitions_release}
    ${preprocessor_definitions_common}
    NDEBUG
)

# Ship configuration.
set (preprocessor_definitions_ship
    ${preprocessor_definitions_ship}
    ${preprocessor_definitions_release}     # the Ship configuration inherits from the Release configuration
)

# Profile configuration.
set (preprocessor_definitions_profile
    ${preprocessor_definitions_profile}
    ${preprocessor_definitions_ship}        # the Profile configuration inherits from the Ship configuration
)

# Add the SHIP preprocessor definition only after the Profile configuration is set.
set (preprocessor_definitions_ship
    ${preprocessor_definitions_ship}
    SHIP
)

# A convenience macro to apply the preprocessor definitions to all configurations of a given target.
macro (apply_preprocessor_definitions target)
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_DEBUG ${preprocessor_definitions_debug}
    )
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_RELEASE ${preprocessor_definitions_release}
    )
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_SHIP ${preprocessor_definitions_ship}
    )
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_PROFILE ${preprocessor_definitions_profile}
    )
endmacro ()

# A convenience macro to append custom preprocessor definitions to all configurations of a given target.
macro (append_custom_preprocessor_definitions target first_definition)
    set (definitions ${first_definition} ${ARGN})
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_DEBUG ${definitions}
    )
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_RELEASE ${definitions}
    )
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_SHIP ${definitions}
    )
    set_property (TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_PROFILE ${definitions}
    )
endmacro ()


#--------------------------------------------------------------------------------------------------
# Compilation/linking flags.
#--------------------------------------------------------------------------------------------------

# All configurations.
set (compiler_flags_common
    ${compiler_flags_common}
)
set (exe_linker_flags_common
    ${exe_linker_flags_common}
)
set (shared_lib_linker_flags_common
    ${shared_lib_linker_flags_common}
)

# Debug configuration.
set (compiler_flags_debug
    ${compiler_flags_debug}
    ${compiler_flags_common}
)
set (exe_linker_flags_debug
    ${exe_linker_flags_debug}
    ${exe_linker_flags_common}
)
set (shared_lib_linker_flags_debug
    ${shared_lib_linker_flags_debug}
    ${shared_lib_linker_flags_common}
)

# Release configuration.
set (compiler_flags_release
    ${compiler_flags_release}
    ${compiler_flags_common}
)
set (exe_linker_flags_release
    ${exe_linker_flags_release}
    ${exe_linker_flags_common}
)
set (shared_lib_linker_flags_release
    ${shared_lib_linker_flags_release}
    ${shared_lib_linker_flags_common}
)

# Ship configuration.
set (compiler_flags_ship
    ${compiler_flags_ship}
    ${compiler_flags_release}               # the Ship configuration inherits from the Release configuration
)
set (exe_linker_flags_ship
    ${exe_linker_flags_ship}
    ${exe_linker_flags_release}             # the Ship configuration inherits from the Release configuration
)
set (shared_lib_linker_flags_ship
    ${shared_lib_linker_flags_ship}
    ${shared_lib_linker_flags_release}      # the Ship configuration inherits from the Release configuration
)

# Profile configuration.
set (compiler_flags_profile
    ${compiler_flags_profile}
    ${compiler_flags_ship}                  # the Profile configuration inherits from the Ship configuration
)
set (exe_linker_flags_profile
    ${exe_linker_flags_profile}
    ${exe_linker_flags_ship}                # the Profile configuration inherits from the Ship configuration
)
set (shared_lib_linker_flags_profile
    ${shared_lib_linker_flags_profile}
    ${shared_lib_linker_flags_ship}         # the Profile configuration inherits from the Ship configuration
)

# Apply Debug configuration compilation/linking flags.
convert_list_to_string (CMAKE_CXX_FLAGS_DEBUG "${compiler_flags_debug}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_DEBUG "${exe_linker_flags_debug}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_DEBUG "${shared_lib_linker_flags_debug}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_DEBUG "${shared_lib_linker_flags_debug}")

# Apply Release configuration compilation/linking flags.
convert_list_to_string (CMAKE_CXX_FLAGS_RELEASE "${compiler_flags_release}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_RELEASE "${exe_linker_flags_release}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_RELEASE "${shared_lib_linker_flags_release}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_RELEASE "${shared_lib_linker_flags_release}")

# Apply Ship configuration compilation/linking flags.
convert_list_to_string (CMAKE_CXX_FLAGS_SHIP "${compiler_flags_ship}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_SHIP "${exe_linker_flags_ship}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_SHIP "${shared_lib_linker_flags_ship}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_SHIP "${shared_lib_linker_flags_ship}")

# Apply Profile configuration compilation/linking flags.
convert_list_to_string (CMAKE_CXX_FLAGS_PROFILE "${compiler_flags_profile}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_PROFILE "${exe_linker_flags_profile}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_PROFILE "${shared_lib_linker_flags_profile}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_PROFILE "${shared_lib_linker_flags_profile}")


#--------------------------------------------------------------------------------------------------
# Products.
#--------------------------------------------------------------------------------------------------

add_subdirectory (appleseed)

if (WITH_CLI OR WITH_STUDIO)
    add_subdirectory (appleseed.shared)
endif ()

if (WITH_CLI)
    add_subdirectory (appleseed.cli)
endif ()

if (WITH_STUDIO)
    add_subdirectory (appleseed.studio)
endif ()

if (WITH_TOOLS)
    add_subdirectory (tools/animatecamera)
    add_subdirectory (tools/maketiledexr)
    add_subdirectory (tools/normalizeprojectfile)
endif()

if (WITH_PYTHON)
    add_subdirectory (appleseed.python)
endif ()


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

install (PROGRAMS ../scripts/run-appleseed.sh
    DESTINATION bin
)

install (FILES ../scripts/watch_folder.py
    DESTINATION bin
)

install (
    FILES
        ../sandbox/schemas/settings.xsd
        ../sandbox/schemas/project.xsd
    DESTINATION schemas
)

install (
    DIRECTORY
        ../sandbox/animations
        ../sandbox/extras
        ../sandbox/images
        ../sandbox/scenes
        ../sandbox/tests
    DESTINATION .
)
