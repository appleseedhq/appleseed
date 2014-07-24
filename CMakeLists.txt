
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

include (src/cmake/utilities.txt)


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
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/src/cmake/Modules)


#--------------------------------------------------------------------------------------------------
# Build options.
#--------------------------------------------------------------------------------------------------

option (USE_STATIC_BOOST        "Use static Boost libs"                                 ON)
option (USE_EXTERNAL_ZLIB       "Use external zlib lib"                                 OFF)
option (USE_EXTERNAL_PNG        "Use external png lib"                                  OFF)
option (USE_EXTERNAL_EXR        "Use external OpenEXR lib"                              OFF)
option (USE_EXTERNAL_ALEMBIC    "Use external Alembic lib"                              OFF)
option (USE_EXTERNAL_XERCES     "Use external Xerces lib"                               OFF)

option (WITH_CLI                "Build appleseed.cli"                                   ON)
option (WITH_STUDIO             "Build appleseed.studio"                                ON)
option (WITH_TOOLS              "Build appleseed tools"                                 ON)
option (WITH_PYTHON             "Build Python bindings"                                 ON)
option (WITH_ALEMBIC            "Build Alembic support"                                 ON)
option (WITH_OSL                "Build OSL support"                                     OFF)
option (WITH_DISNEY_MATERIAL    "Build Disney Material"                                 OFF)

option (USE_SSE                 "Use SSE and SSE 2 instruction sets"                    ON)
option (USE_QMC_SAMPLER         "Use QMC sampler (possible software patent issues)"     OFF)

option (HIDE_SYMBOLS            "When using gcc, hide symbols not on the public API"    ON)


#--------------------------------------------------------------------------------------------------
# Enable testing with CTest.
#--------------------------------------------------------------------------------------------------

enable_testing ()


#--------------------------------------------------------------------------------------------------
# Platform-specific settings.
#--------------------------------------------------------------------------------------------------

# This must come after project () otherwise CMAKE_SYSTEM_NAME is undefined.
if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    include (src/cmake/config/win-vs110.txt)
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    include (src/cmake/config/mac-gcc4.txt)
elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
    include (src/cmake/config/linux-gcc4.txt)
else ()
    message (FATAL_ERROR "Unsupported platform or compiler (CMAKE_SYSTEM_NAME is \"${CMAKE_SYSTEM_NAME}\"). "
                         "Edit ${CMAKE_CURRENT_LIST_FILE} to add support for it.")
endif ()


#--------------------------------------------------------------------------------------------------
# Version information.
#--------------------------------------------------------------------------------------------------

if (git_tag)
    # The presence of this variable is currently used as an indication that this is not a git
    # repository and hence the git command will fail. This variable should be set (via CMake
    # command line option) to the following versioning pattern: MAJOR.MINOR.PATCH to satisfy
    # the parsing later on in the file.
    #
    # This is just an interim solution and should be replaced by a proper version header file
    # approach as outlined by Esteban.
else ()
    # Extract git tag.
    exec_program (
        ${git_command}
        ${CMAKE_CURRENT_SOURCE_DIR}
        ARGS "describe --long"
        OUTPUT_VARIABLE git_tag
    )
endif ()


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

if (USE_EXTERNAL_ZLIB)
    find_package (ZLIB REQUIRED)
endif ()

if (USE_EXTERNAL_PNG)
    find_package (PNG REQUIRED)
endif ()

if (USE_EXTERNAL_EXR)
    find_package (Imath REQUIRED)
    find_package (OpenEXR REQUIRED)
endif ()

if (WITH_ALEMBIC)
    add_definitions (-DWITH_ALEMBIC)

    if (USE_EXTERNAL_ALEMBIC)
        find_package (HDF5 REQUIRED COMPONENTS C HL)
        find_package (Alembic REQUIRED)
    endif ()
endif ()

if (USE_EXTERNAL_XERCES)
    add_definitions (-DWITH_EXTERNAL_XERCES)

    find_package (Xerces REQUIRED)
endif ()

if (WITH_OSL)
    add_definitions (-DWITH_OSL)

    find_package (OpenImageIO REQUIRED)
    find_package (OSL REQUIRED)
endif ()

if (WITH_DISNEY_MATERIAL)
    add_definitions (-DWITH_DISNEY_MATERIAL)

    find_package(SeExpr REQUIRED)

    if (WITH_STUDIO)
        find_package(SeExprEditor REQUIRED)
    endif ()
endif ()

#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

# Common include paths.
include_directories (
    src/appleseed
    src/lz4
    src/minilzo
    src/STANN/include
    ${PYTHON_INCLUDE_DIRS}
)

if (USE_EXTERNAL_ZLIB)
    include_directories (${ZLIB_INCLUDE_DIRS})
else ()
    include_directories (3rdparty/zlib)
endif ()

if (USE_EXTERNAL_PNG)
    include_directories (${PNG_INCLUDE_DIR})
else ()
    include_directories (3rdparty/libpng)
endif ()

if (USE_EXTERNAL_EXR)
    include_directories (
        ${OPENEXR_INCLUDE_DIRS}
        ${OPENEXR_INCLUDE_DIRS}/OpenEXR/
        ${IMATH_INCLUDE_DIRS}
        ${IMATH_INCLUDE_DIRS}/OpenEXR/
    )
else ()
    include_directories (
        3rdparty/openexr/include
        3rdparty/openexr/include/OpenEXR
    )
endif ()

if (WITH_ALEMBIC)
    if (USE_EXTERNAL_ALEMBIC)
        include_directories (
            ${HDF5_INCLUDE_DIRS}
            ${ALEMBIC_INCLUDE_DIRS}
        )
    else ()
        include_directories (3rdparty/alembic/lib)
    endif ()
endif ()

if (USE_EXTERNAL_XERCES)
    include_directories (${XERCES_INCLUDE_DIRS})
else ()
    include_directories (3rdparty/xerces-c/src)
endif ()

if (WITH_OSL)
    include_directories (
        ${OPENIMAGEIO_INCLUDE_DIRS}
        ${OSL_INCLUDE_DIRS}
    )
endif ()

if (WITH_DISNEY_MATERIAL)
    include_directories (${SEEXPR_INCLUDE_DIRS})

    if (WITH_STUDIO)
        include_directories (${SEEXPREDITOR_INCLUDE_DIRS})
    endif ()
endif ()

#--------------------------------------------------------------------------------------------------
# Extract version from git.
#--------------------------------------------------------------------------------------------------

string (REGEX MATCHALL "[0-9]+" APPLESEED_VERSION_COMPONENTS ${git_tag})
list (GET APPLESEED_VERSION_COMPONENTS 0 APPLESEED_VERSION_MAJOR)
list (GET APPLESEED_VERSION_COMPONENTS 1 APPLESEED_VERSION_MINOR)
list (GET APPLESEED_VERSION_COMPONENTS 2 APPLESEED_VERSION_PATCH)


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

# Definitions common to all configurations.
set (preprocessor_definitions_common
    ${preprocessor_definitions_common}
    APPLESEED_VERSION_STRING="${git_tag}"
)
if (CMAKE_SIZEOF_VOID_P MATCHES 4)
    set (preprocessor_definitions_common
        ${preprocessor_definitions_common}
        APPLESEED_ARCH32
)
elseif (CMAKE_SIZEOF_VOID_P MATCHES 8)
    set (preprocessor_definitions_common
        ${preprocessor_definitions_common}
        APPLESEED_ARCH64
)
else ()
    message (FATAL_ERROR "Cannot determine pointer size.")
endif ()
if (USE_QMC_SAMPLER)
    set (preprocessor_definitions_common
        ${preprocessor_definitions_common}
        USE_QMC_SAMPLER
    )
endif ()
if (USE_SSE)
    if (CMAKE_SYSTEM_NAME STREQUAL "Linux" AND CMAKE_SIZEOF_VOID_P MATCHES 4)
        message (WARNING "Building appleseed with SSE/SSE2 instruction sets on 32-bit Linux is not supported; continuing without SSE/SSE2.")
    else ()
        set (preprocessor_definitions_common
            ${preprocessor_definitions_common}
            APPLESEED_USE_SSE
        )
    endif ()
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

# Flags common to all configurations.
set (c_compiler_flags_common ${c_compiler_flags_common})
set (cxx_compiler_flags_common ${cxx_compiler_flags_common})
set (exe_linker_flags_common ${exe_linker_flags_common})
set (shared_lib_linker_flags_common ${shared_lib_linker_flags_common})
set (static_lib_linker_flags_common ${static_lib_linker_flags_common})

# Debug configuration.
set (c_compiler_flags_debug ${c_compiler_flags_debug}                       ${c_compiler_flags_common})
set (cxx_compiler_flags_debug ${cxx_compiler_flags_debug}                   ${cxx_compiler_flags_common})
set (exe_linker_flags_debug ${exe_linker_flags_debug}                       ${exe_linker_flags_common})
set (shared_lib_linker_flags_debug ${shared_lib_linker_flags_debug}         ${shared_lib_linker_flags_common})
set (static_lib_linker_flags_debug ${static_lib_linker_flags_debug}         ${static_lib_linker_flags_common})

# Release configuration.
set (c_compiler_flags_release ${c_compiler_flags_release}                   ${c_compiler_flags_common})
set (cxx_compiler_flags_release ${cxx_compiler_flags_release}               ${cxx_compiler_flags_common})
set (exe_linker_flags_release ${exe_linker_flags_release}                   ${exe_linker_flags_common})
set (shared_lib_linker_flags_release ${shared_lib_linker_flags_release}     ${shared_lib_linker_flags_common})
set (static_lib_linker_flags_release ${static_lib_linker_flags_release}     ${static_lib_linker_flags_common})

# Ship configuration (inherits from the Release configuration).
set (c_compiler_flags_ship ${c_compiler_flags_ship}                         ${c_compiler_flags_release})
set (cxx_compiler_flags_ship ${cxx_compiler_flags_ship}                     ${cxx_compiler_flags_release})
set (exe_linker_flags_ship ${exe_linker_flags_ship}                         ${exe_linker_flags_release})
set (shared_lib_linker_flags_ship ${shared_lib_linker_flags_ship}           ${shared_lib_linker_flags_release})
set (static_lib_linker_flags_ship ${static_lib_linker_flags_ship}           ${static_lib_linker_flags_release})

# Profile configuration (inherits from the Ship configuration).
set (c_compiler_flags_profile ${c_compiler_flags_profile}                   ${c_compiler_flags_ship})
set (cxx_compiler_flags_profile ${cxx_compiler_flags_profile}               ${cxx_compiler_flags_ship})
set (exe_linker_flags_profile ${exe_linker_flags_profile}                   ${exe_linker_flags_ship})
set (shared_lib_linker_flags_profile ${shared_lib_linker_flags_profile}     ${shared_lib_linker_flags_ship})
set (static_lib_linker_flags_profile ${static_lib_linker_flags_profile}     ${static_lib_linker_flags_ship})

# Apply Debug configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_DEBUG "${c_compiler_flags_debug}")
convert_list_to_string (CMAKE_CXX_FLAGS_DEBUG "${c_compiler_flags_debug} ${cxx_compiler_flags_debug}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_DEBUG "${exe_linker_flags_debug}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_DEBUG "${shared_lib_linker_flags_debug}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_DEBUG "${shared_lib_linker_flags_debug}")

# Apply Release configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_RELEASE "${c_compiler_flags_release}")
convert_list_to_string (CMAKE_CXX_FLAGS_RELEASE "${c_compiler_flags_release} ${cxx_compiler_flags_release}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_RELEASE "${exe_linker_flags_release}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_RELEASE "${shared_lib_linker_flags_release}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_RELEASE "${shared_lib_linker_flags_release}")

# Apply Ship configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_SHIP "${c_compiler_flags_ship}")
convert_list_to_string (CMAKE_CXX_FLAGS_SHIP "${c_compiler_flags_ship} ${cxx_compiler_flags_ship}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_SHIP "${exe_linker_flags_ship}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_SHIP "${shared_lib_linker_flags_ship}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_SHIP "${shared_lib_linker_flags_ship}")

# Apply Profile configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_PROFILE "${c_compiler_flags_profile}")
convert_list_to_string (CMAKE_CXX_FLAGS_PROFILE "${c_compiler_flags_profile} ${cxx_compiler_flags_profile}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_PROFILE "${exe_linker_flags_profile}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_PROFILE "${shared_lib_linker_flags_profile}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_PROFILE "${shared_lib_linker_flags_profile}")

# A convenience macro to apply the static library linker flags to all configurations of a given target.
macro (apply_static_lib_linker_flags target)
    set_property (TARGET ${target} PROPERTY
        STATIC_LIBRARY_FLAGS_DEBUG ${static_lib_linker_flags_debug}
    )
    set_property (TARGET ${target} PROPERTY
        STATIC_LIBRARY_FLAGS_RELEASE ${static_lib_linker_flags_release}
    )
    set_property (TARGET ${target} PROPERTY
        STATIC_LIBRARY_FLAGS_SHIP ${static_lib_linker_flags_ship}
    )
    set_property (TARGET ${target} PROPERTY
        STATIC_LIBRARY_FLAGS_PROFILE ${static_lib_linker_flags_profile}
    )
endmacro ()


#--------------------------------------------------------------------------------------------------
# Products.
#--------------------------------------------------------------------------------------------------

add_subdirectory (src/appleseed)
add_subdirectory (src/lz4)
add_subdirectory (src/minilzo)

if (WITH_CLI OR WITH_STUDIO OR WITH_TOOLS)
    add_subdirectory (src/appleseed.shared)
endif ()

if (WITH_CLI)
    add_subdirectory (src/appleseed.cli)
endif ()

if (WITH_STUDIO)
    add_subdirectory (src/appleseed.studio)
endif ()

if (WITH_PYTHON)
    add_subdirectory (src/appleseed.python)
endif ()

if (WITH_TOOLS)
    add_subdirectory (src/tools/animatecamera)
    add_subdirectory (src/tools/convertmeshfile)
    add_subdirectory (src/tools/maketiledexr)
    add_subdirectory (src/tools/updateprojectfile)
endif()


#--------------------------------------------------------------------------------------------------
# Documentation.
#--------------------------------------------------------------------------------------------------

find_package (Doxygen)

if (DOXYGEN_FOUND)
    configure_file (${PROJECT_SOURCE_DIR}/scripts/appleseed.doxyfile.in ${PROJECT_BINARY_DIR}/appleseed.doxyfile @ONLY)
    add_custom_target (
        doc ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/appleseed.doxyfile
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        COMMENT "Generating documentation with Doxygen" VERBATIM
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Devkit preparation (for installation).
#--------------------------------------------------------------------------------------------------

file (GLOB_RECURSE DEVKIT_APPLESEED_INCLUDE RELATIVE ${PROJECT_SOURCE_DIR} "appleseed/*.h")
file (GLOB_RECURSE DEVKIT_APPLESEED_SHARED_INCLUDE RELATIVE ${PROJECT_SOURCE_DIR} "appleseed.shared/*.h")

# Archives creation.
execute_process (
    COMMAND ${CMAKE_COMMAND} -E tar cf ${CMAKE_BINARY_DIR}/appleseed_include.tar ${DEVKIT_APPLESEED_INCLUDE}
    COMMAND ${CMAKE_COMMAND} -E tar cf ${CMAKE_BINARY_DIR}/appleseed_shared_include.tar ${DEVKIT_APPLESEED_SHARED_INCLUDE}
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
)

# Create devkit staging area.
execute_process (
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/extras/devkit/include
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/extras/devkit/lib/${CMAKE_BUILD_TYPE}
)

# Archive extraction in preparation for installation commands.
execute_process (
    COMMAND ${CMAKE_COMMAND} -E tar xf ${CMAKE_BINARY_DIR}/appleseed_include.tar
    COMMAND ${CMAKE_COMMAND} -E tar xf ${CMAKE_BINARY_DIR}/appleseed_shared_include.tar
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/extras/devkit/include
)


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

install (
    PROGRAMS
        scripts/run-appleseed.sh
    DESTINATION bin
)

install (
    FILES
        scripts/convertmany.py
        scripts/rendermanager.py
        scripts/rendermany.py
        scripts/rendernode.py
        scripts/updatemany.py
    DESTINATION bin
)

install (
    FILES
        sandbox/schemas/settings.xsd
        sandbox/schemas/project.xsd
    DESTINATION schemas
)

if (WITH_OSL)
    install (
        FILES
            sandbox/shaders/oslutil.h
            sandbox/shaders/stdosl.h
        DESTINATION shaders
    )
endif ()

install (
    DIRECTORY
        sandbox/extras
        sandbox/images
        sandbox/tests
    DESTINATION .
)

install (
    DIRECTORY
        ${CMAKE_BINARY_DIR}/extras/devkit/include
    DESTINATION extras/devkit
)

install (
    DIRECTORY
        ${CMAKE_SOURCE_DIR}/sandbox/extras/devkit/cmake
    DESTINATION extras/devkit
)

install (
    DIRECTORY
        ${CMAKE_SOURCE_DIR}/sandbox/extras/devkit/sample
    DESTINATION extras/devkit
)


#--------------------------------------------------------------------------------------------------
# Packaging.
#--------------------------------------------------------------------------------------------------

set (CPACK_PACKAGE_VENDOR "appleseedhq")
set (CPACK_PACKAGE_VERSION_MAJOR ${APPLESEED_VERSION_MAJOR})
set (CPACK_PACKAGE_VERSION_MINOR ${APPLESEED_VERSION_MINOR})
set (CPACK_PACKAGE_VERSION_PATCH ${APPLESEED_VERSION_PATCH})

include (CPack)
