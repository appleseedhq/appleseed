
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
# Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

cmake_policy (SET CMP0008 NEW)      # libraries linked by full-path must have a valid library file name

if (POLICY CMP0026)
    cmake_policy (SET CMP0026 OLD)  # disallow use of the LOCATION target property (CMake 3.0)
endif ()

if (POLICY CMP0042)
    cmake_policy (SET CMP0042 OLD)  # MACOSX_RPATH is enabled by default (CMake 3.0)
endif ()

if (POLICY CMP0043)
    cmake_policy (SET CMP0043 OLD)  # consume the content of the suffixed COMPILE_DEFINITIONS_<CONFIG>
endif ()

include (src/cmake/utilities.txt)


#--------------------------------------------------------------------------------------------------
# Build configurations.
#--------------------------------------------------------------------------------------------------

# Prevent in-source builds.
if (${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message (FATAL_ERROR "In-source builds are not permitted; run CMake inside an empty build directory.")
endif ()

# Create the build configurations. Must come before project ().
set (CMAKE_CONFIGURATION_TYPES "Debug;Release;Profile;Ship" CACHE STRING "" FORCE)

# Select a build configuration if none is selected yet.
if (NOT CMAKE_BUILD_TYPE)
    set (CMAKE_BUILD_TYPE Ship
         CACHE STRING "Choose the type of build, options are: ${CMAKE_CONFIGURATION_TYPES}" FORCE)
    set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${CMAKE_CONFIGURATION_TYPES})
endif ()

project (appleseed)

# Setup the path for our FindPackage modules.
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/src/cmake/Modules)


#--------------------------------------------------------------------------------------------------
# Detect target architecture.
#--------------------------------------------------------------------------------------------------

detect_target_arch (TARGET_ARCH)
message ("Target architecture: ${TARGET_ARCH}")

# Detect x86-based platforms.
if (TARGET_ARCH MATCHES "i386|x86_64")
    set (is_x86 TRUE)
else ()
    set (is_x86 FALSE)
endif ()


#--------------------------------------------------------------------------------------------------
# Build options.
#--------------------------------------------------------------------------------------------------

option (USE_EXTERNAL_EXR                    "Use external OpenEXR library"                          OFF)
option (USE_EXTERNAL_OIIO                   "Use external OpenImageIO library"                      OFF)
option (USE_EXTERNAL_OSL                    "Use external OpenShadingLanguage library"              OFF)
option (USE_EXTERNAL_PNG                    "Use external libpng library"                           OFF)
option (USE_EXTERNAL_SEEXPR                 "Use external SeExpr library"                           OFF)
option (USE_EXTERNAL_XERCES                 "Use external Xerces library"                           OFF)
option (USE_EXTERNAL_ZLIB                   "Use external zlib library"                             OFF)

option (WITH_CLI                            "Build appleseed.cli"                                   ON)
option (WITH_STUDIO                         "Build appleseed.studio"                                ON)
option (WITH_TOOLS                          "Build appleseed tools"                                 ON)
option (WITH_PYTHON                         "Build Python bindings"                                 ON)
option (WITH_DISNEY_MATERIAL                "Build Disney material"                                 OFF)
option (WITH_NORMALIZED_DIFFUSION_BSSRDF    "Build Normalized Diffusion BSSRDF (patented)"          OFF)
option (WITH_PARTIO                         "Build Partio support (used in unit tests)"             OFF)

option (USE_CPP11                           "Use C++11"                                             OFF)
option (USE_STATIC_BOOST                    "Use static Boost libraries"                            ON)
option (USE_STATIC_OIIO                     "Use static OpenImageIO libraries"                      ON)
option (USE_STATIC_OSL                      "Use static OpenShadingLanguage libraries"              ON)
option (WARNINGS_AS_ERRORS                  "Treat compiler warnings as errors"                     ON)

option (HIDE_SYMBOLS                        "When using gcc, hide symbols not on the public API"    ON)

if (CMAKE_SYSTEM_NAME MATCHES "Linux|FreeBSD")
    option (USE_VISIBILITY_MAP              "Use GNU export map for libappleseed.so (experimental)" OFF)
endif ()

# SIMD options.
if (is_x86)
    option (USE_SSE                         "Use SSE instruction sets up to version 2"              ON)
    if (TARGET_ARCH MATCHES "x86_64")
        option (USE_SSE42                   "Use SSE instruction sets up to version 4.2"            OFF)
        option (USE_AVX                     "Use AVX instruction set"                               OFF)
        option (USE_AVX2                    "Use AVX2 instruction set"                              OFF)
    endif ()
elseif (TARGET_ARCH MATCHES "ARM")
    option (USE_NEON                        "Use NEON instruction set"                              OFF)
elseif (TARGET_ARCH MATCHES "PPC")
    option (USE_ALTIVEC                     "Use AltiVec instruction set"                           OFF)
else ()
    # Unknown architecture: No SIMD.
endif ()

if (USE_CPP11)
    message ("Building in C++11 mode.")
    add_definitions (-DAPPLESEED_USE_CPP11)
else ()
    message ("Building in C++03 mode.")
endif ()


#--------------------------------------------------------------------------------------------------
# Common settings.
#--------------------------------------------------------------------------------------------------

set (CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)


#--------------------------------------------------------------------------------------------------
# Platform-specific settings.
#--------------------------------------------------------------------------------------------------

# This must come after project () otherwise CMAKE_SYSTEM_NAME is undefined.
if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    include (src/cmake/config/win-vs.txt)
elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    include (src/cmake/config/mac-clang.txt)
elseif (CMAKE_SYSTEM_NAME MATCHES "Linux|FreeBSD")
    include (src/cmake/config/linux-gcc.txt)
else ()
    message (FATAL_ERROR "Unsupported platform or compiler (CMAKE_SYSTEM_NAME is \"${CMAKE_SYSTEM_NAME}\"). "
                         "Edit ${CMAKE_CURRENT_LIST_FILE} to add support for it.")
endif ()


#--------------------------------------------------------------------------------------------------
# Auto-generate version headers.
#--------------------------------------------------------------------------------------------------

set (appleseed_version_major    1)
set (appleseed_version_minor    7)
set (appleseed_version_patch    0)
set (appleseed_version_maturity beta)

configure_file (${PROJECT_SOURCE_DIR}/src/appleseed/foundation/core/version.h.in
                ${PROJECT_SOURCE_DIR}/src/appleseed/foundation/core/version.h)


#--------------------------------------------------------------------------------------------------
# Boost libraries.
#--------------------------------------------------------------------------------------------------

set (Boost_MULTITHREADED TRUE)

if (USE_STATIC_BOOST)
    set (Boost_USE_STATIC_LIBS TRUE)
endif ()

set (BOOST_NEEDED_LIBS chrono date_time filesystem regex system thread wave)

find_package (Boost 1.55 REQUIRED ${BOOST_NEEDED_LIBS})

add_definitions (-DBOOST_FILESYSTEM_VERSION=3 -DBOOST_FILESYSTEM_NO_DEPRECATED)

if (USE_CPP11)
    # Workaround for undefined reference to boost::filesystem::detail::copy_file
    # link error when building in C++11 mode.
    add_definitions (-DBOOST_NO_CXX11_SCOPED_ENUMS)
endif ()

include_directories (SYSTEM ${Boost_INCLUDE_DIRS})
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

if (USE_EXTERNAL_XERCES)
    add_definitions (-DAPPLESEED_WITH_EXTERNAL_XERCES)
    find_package (Xerces REQUIRED)
endif ()

if (USE_STATIC_OIIO)
    add_definitions (-DOIIO_STATIC_BUILD)
endif ()
if (USE_EXTERNAL_OIIO)
    find_package (OpenImageIO REQUIRED)
endif ()

if (USE_STATIC_OSL)
    add_definitions (-DOSL_STATIC_LIBRARY)
endif ()
if (USE_EXTERNAL_OSL)
    find_package (OSL REQUIRED)
endif ()

if (WITH_DISNEY_MATERIAL)
    add_definitions (-DAPPLESEED_WITH_DISNEY_MATERIAL)
    if (USE_EXTERNAL_SEEXPR)
        find_package (SeExpr REQUIRED)
        if (WITH_STUDIO)
            find_package (SeExprEditor REQUIRED)
        endif ()
    endif ()
endif ()

if (WITH_NORMALIZED_DIFFUSION_BSSRDF)
    add_definitions (-DAPPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF)
endif ()

if (WITH_PARTIO)
    add_definitions (-DAPPLESEED_WITH_PARTIO)
    find_package (Partio REQUIRED)
endif ()


#--------------------------------------------------------------------------------------------------
# Include paths.
#--------------------------------------------------------------------------------------------------

# Common include paths.
include_directories (
    src/appleseed
    src/lz4
    ${PYTHON_INCLUDE_DIRS}
)

if (USE_EXTERNAL_ZLIB)
    include_directories (${ZLIB_INCLUDE_DIRS})
else ()
    include_directories (${APPLESEED_DEPS_STAGE_DIR}/zlib/include)
endif ()

if (USE_EXTERNAL_PNG)
    include_directories (${PNG_INCLUDE_DIR})
else ()
    include_directories (${APPLESEED_DEPS_STAGE_DIR}/libpng-debug/include)
endif ()

if (USE_EXTERNAL_EXR)
    include_directories (
        ${IMATH_INCLUDE_DIRS}
        ${IMATH_INCLUDE_DIRS}/OpenEXR/
        ${OPENEXR_INCLUDE_DIRS}
        ${OPENEXR_INCLUDE_DIRS}/OpenEXR/
    )
else ()
    include_directories (
        ${APPLESEED_DEPS_STAGE_DIR}/ilmbase-debug/include
        ${APPLESEED_DEPS_STAGE_DIR}/ilmbase-debug/include/OpenEXR
        ${APPLESEED_DEPS_STAGE_DIR}/openexr-debug/include
        ${APPLESEED_DEPS_STAGE_DIR}/openexr-debug/include/OpenEXR
    )
endif ()

if (USE_EXTERNAL_XERCES)
    include_directories (${XERCES_INCLUDE_DIRS})
else ()
    include_directories (${APPLESEED_DEPS_STAGE_DIR}/xerces-c/include)
endif ()

if (USE_EXTERNAL_OIIO)
    include_directories (${OPENIMAGEIO_INCLUDE_DIRS})
else ()
    include_directories (${APPLESEED_DEPS_STAGE_DIR}/oiio-debug/include)
endif ()

if (USE_EXTERNAL_OSL)
    include_directories (${OSL_INCLUDE_DIRS})
else ()
    include_directories (${APPLESEED_DEPS_STAGE_DIR}/osl-debug/include)
endif ()

if (WITH_DISNEY_MATERIAL)
    if (USE_EXTERNAL_SEEXPR)
        include_directories (${SEEXPR_INCLUDE_DIRS})
        if (WITH_STUDIO)
            include_directories (${SEEXPREDITOR_INCLUDE_DIRS})
        endif ()
    else ()
        include_directories (${APPLESEED_DEPS_STAGE_DIR}/SeExpr-debug/include)
    endif ()
endif ()

if (WITH_PARTIO)
    include_directories (${PARTIO_INCLUDE_DIRS})
endif ()


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

#
# Definitions common to all configurations.
#

set (preprocessor_definitions_common
    ${preprocessor_definitions_common}
)

if (is_x86)
    set (preprocessor_definitions_common
        ${preprocessor_definitions_common}
        APPLESEED_X86
    )
endif ()

# $ORIGIN support in rpath.
if (UNIX AND NOT APPLE)
    set (USE_RPATH_ORIGIN TRUE)
endif ()

# SIMD.
if (is_x86)
    if (USE_AVX2)
        set (USE_AVX TRUE)
        set (preprocessor_definitions_common
            ${preprocessor_definitions_common}
            APPLESEED_USE_AVX2
        )
    endif ()
    if (USE_AVX)
        set (USE_SSE42 TRUE)
        set (preprocessor_definitions_common
            ${preprocessor_definitions_common}
            APPLESEED_USE_AVX
        )
    endif ()
    if (USE_SSE42)
        set (USE_SSE TRUE)
        set (preprocessor_definitions_common
            ${preprocessor_definitions_common}
            APPLESEED_USE_SSE42
        )
    endif ()
    if (USE_SSE)
        set (preprocessor_definitions_common
            ${preprocessor_definitions_common}
            APPLESEED_USE_SSE
        )
        if (CMAKE_SYSTEM_NAME STREQUAL "Linux" AND CMAKE_SIZEOF_VOID_P MATCHES 4)
            message (WARNING "Building appleseed with SSE/SSE2 instruction sets on 32-bit Linux is not supported.")
        endif ()
    endif ()
elseif (TARGET_ARCH MATCHES "ARM")
    if (USE_NEON)
        set (preprocessor_definitions_common
            ${preprocessor_definitions_common}
            APPLESEED_USE_NEON
        )
    endif ()
elseif (TARGET_ARCH MATCHES "PPC")
    if (USE_ALTIVEC)
        set (preprocessor_definitions_common
            ${preprocessor_definitions_common}
            APPLESEED_USE_ALTIVEC
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

# Debug configuration.
set (c_compiler_flags_debug             ${c_compiler_flags_common}          ${c_compiler_flags_debug})
set (cxx_compiler_flags_debug           ${cxx_compiler_flags_common}        ${cxx_compiler_flags_debug})
set (exe_linker_flags_debug             ${exe_linker_flags_common}          ${exe_linker_flags_debug})
set (shared_lib_linker_flags_debug      ${shared_lib_linker_flags_common}   ${shared_lib_linker_flags_debug})
set (static_lib_linker_flags_debug      ${static_lib_linker_flags_common}   ${static_lib_linker_flags_debug})

# Release configuration.
set (c_compiler_flags_release           ${c_compiler_flags_common}          ${c_compiler_flags_release})
set (cxx_compiler_flags_release         ${cxx_compiler_flags_common}        ${cxx_compiler_flags_release})
set (exe_linker_flags_release           ${exe_linker_flags_common}          ${exe_linker_flags_release})
set (shared_lib_linker_flags_release    ${shared_lib_linker_flags_common}   ${shared_lib_linker_flags_release})
set (static_lib_linker_flags_release    ${static_lib_linker_flags_common}   ${static_lib_linker_flags_release})

# Ship configuration (inherits from the Release configuration).
set (c_compiler_flags_ship              ${c_compiler_flags_release}         ${c_compiler_flags_ship})
set (cxx_compiler_flags_ship            ${cxx_compiler_flags_release}       ${cxx_compiler_flags_ship})
set (exe_linker_flags_ship              ${exe_linker_flags_release}         ${exe_linker_flags_ship})
set (shared_lib_linker_flags_ship       ${shared_lib_linker_flags_release}  ${shared_lib_linker_flags_ship})
set (static_lib_linker_flags_ship       ${static_lib_linker_flags_release}  ${static_lib_linker_flags_ship})

# Profile configuration (inherits from the Release configuration).
set (c_compiler_flags_profile           ${c_compiler_flags_release}         ${c_compiler_flags_profile})
set (cxx_compiler_flags_profile         ${cxx_compiler_flags_release}       ${cxx_compiler_flags_profile})
set (exe_linker_flags_profile           ${exe_linker_flags_release}         ${exe_linker_flags_profile})
set (shared_lib_linker_flags_profile    ${shared_lib_linker_flags_release}  ${shared_lib_linker_flags_profile})
set (static_lib_linker_flags_profile    ${static_lib_linker_flags_release}  ${static_lib_linker_flags_profile})

# Apply Debug configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_DEBUG                 "${c_compiler_flags_debug}")
convert_list_to_string (CMAKE_CXX_FLAGS_DEBUG               "${c_compiler_flags_debug} ${cxx_compiler_flags_debug}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_DEBUG        "${exe_linker_flags_debug}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_DEBUG     "${shared_lib_linker_flags_debug}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_DEBUG     "${shared_lib_linker_flags_debug}")

# Apply Release configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_RELEASE               "${c_compiler_flags_release}")
convert_list_to_string (CMAKE_CXX_FLAGS_RELEASE             "${c_compiler_flags_release} ${cxx_compiler_flags_release}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_RELEASE      "${exe_linker_flags_release}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_RELEASE   "${shared_lib_linker_flags_release}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_RELEASE   "${shared_lib_linker_flags_release}")

# Apply Ship configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_SHIP                  "${c_compiler_flags_ship}")
convert_list_to_string (CMAKE_CXX_FLAGS_SHIP                "${c_compiler_flags_ship} ${cxx_compiler_flags_ship}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_SHIP         "${exe_linker_flags_ship}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_SHIP      "${shared_lib_linker_flags_ship}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_SHIP      "${shared_lib_linker_flags_ship}")

# Apply Profile configuration compilation/linking flags.
convert_list_to_string (CMAKE_C_FLAGS_PROFILE               "${c_compiler_flags_profile}")
convert_list_to_string (CMAKE_CXX_FLAGS_PROFILE             "${c_compiler_flags_profile} ${cxx_compiler_flags_profile}")
convert_list_to_string (CMAKE_EXE_LINKER_FLAGS_PROFILE      "${exe_linker_flags_profile}")
convert_list_to_string (CMAKE_SHARED_LINKER_FLAGS_PROFILE   "${shared_lib_linker_flags_profile}")
convert_list_to_string (CMAKE_MODULE_LINKER_FLAGS_PROFILE   "${shared_lib_linker_flags_profile}")

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
add_subdirectory (src/appleseed.shaders)

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
    add_subdirectory (src/tools/dumpmetadata)
    add_subdirectory (src/tools/makefluffy)
    add_subdirectory (src/tools/projecttool)
endif ()


#--------------------------------------------------------------------------------------------------
# Documentation.
#--------------------------------------------------------------------------------------------------

find_package (Doxygen)

if (DOXYGEN_FOUND)
    configure_file (${PROJECT_SOURCE_DIR}/scripts/appleseed.doxyfile.in ${PROJECT_BINARY_DIR}/appleseed.doxyfile @ONLY)
    add_custom_target (
        doc ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/appleseed.doxyfile
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        COMMENT "Generating API Reference with Doxygen" VERBATIM
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

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

install (
    FILES
        ${OSL_COMPILER}
        ${OSL_QUERY_INFO}
        ${OSL_MAKETX}
    DESTINATION bin
)

install (
    DIRECTORY sandbox/shaders
    DESTINATION .
)

install (
    DIRECTORY
        sandbox/docs
        sandbox/icons
        sandbox/samples
        sandbox/settings
        sandbox/share
        sandbox/stylesheets
    DESTINATION .
)

install (
    CODE "FILE (MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/images)"
    CODE "FILE (MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/images/autosave)"
)

install (
    DIRECTORY
        "sandbox/tests/unit benchmarks"
        "sandbox/tests/unit tests"
    DESTINATION tests
)

install (
    DIRECTORY src/appleseed/foundation
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

install (
    DIRECTORY src/appleseed/main
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

install (
    DIRECTORY src/appleseed/renderer
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)
