
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


#
# Platform/Compiler-Specific Configuration.
#

if(WIN32 AND MSVC)
include(config.win32-vs90.txt)
elseif(APPLE)
include(config.mac32-gcc4.txt)
else()
message(FATAL_ERROR "Unsupported platform/compiler, edit ${CMAKE_CURRENT_LIST_FILE} to add support for it.")
endif()


#
# Include Paths.
#

include_directories(
    appleseed
    boost
    libpng
    openexr/vc/vc9/include                  # change to openexr/include
    openexr/vc/vc9/include/openexr          # change to openexr/include/openexr
    xerces-c/src
    zlib
)


#
# Preprocessor Definitions.
#

# All configurations.
set(preprocessor_definitions_common
    ${preprocessor_definitions_common}
)

# Debug configuration.
set(preprocessor_definitions_debug
    ${preprocessor_definitions_debug}
    ${preprocessor_definitions_common}
    DEBUG
    _DEBUG
)

# Release configuration.
set(preprocessor_definitions_release
    ${preprocessor_definitions_release}
    ${preprocessor_definitions_common}
    NDEBUG
)

# Ship configuration.
set(preprocessor_definitions_ship
    ${preprocessor_definitions_ship}
    ${preprocessor_definitions_release}     # the Ship configuration inherits from the Release configuration
)

# Profile configuration.
set(preprocessor_definitions_profile
    ${preprocessor_definitions_profile}
    ${preprocessor_definitions_ship}        # the Profile configuration inherits from the Ship configuration
)

# Add the SHIP preprocessor definition only after the Profile configuration is set.
set(preprocessor_definitions_ship
    ${preprocessor_definitions_ship}
    SHIP
)

# A convenience macro to apply the preprocessor definitions to all configurations of a given target.
macro(apply_preprocessor_definitions target)
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_DEBUG ${preprocessor_definitions_debug}
    )
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_RELEASE ${preprocessor_definitions_release}
    )
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_SHIP ${preprocessor_definitions_ship}
    )
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_PROFILE ${preprocessor_definitions_profile}
    )
endmacro()

# A convenience macro to append custom preprocessor definitions to all configurations of a given target.
macro(append_custom_preprocessor_definitions target first_definition)
    set(definitions ${first_definition} ${ARGN})
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_DEBUG ${definitions}
    )
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_RELEASE ${definitions}
    )
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_SHIP ${definitions}
    )
    set_property(TARGET ${target} APPEND PROPERTY
        COMPILE_DEFINITIONS_PROFILE ${definitions}
    )
endmacro()


#
# Compilation/Linking Flags.
#

# All configurations.
set(compiler_flags_common
    ${compiler_flags_common}
)
set(exe_linker_flags_common
    ${exe_linker_flags_common}
)
set(shared_lib_linker_flags_common
    ${shared_lib_linker_flags_common}
)

# Debug configuration.
set(compiler_flags_debug
    ${compiler_flags_debug}
    ${compiler_flags_common}
)
set(exe_linker_flags_debug
    ${exe_linker_flags_debug}
    ${exe_linker_flags_common}
)
set(shared_lib_linker_flags_debug
    ${shared_lib_linker_flags_debug}
    ${shared_lib_linker_flags_common}
)

# Release configuration.
set(compiler_flags_release
    ${compiler_flags_release}
    ${compiler_flags_common}
)
set(exe_linker_flags_release
    ${exe_linker_flags_release}
    ${exe_linker_flags_common}
)
set(shared_lib_linker_flags_release
    ${shared_lib_linker_flags_release}
    ${shared_lib_linker_flags_common}
)

# Ship configuration.
set(compiler_flags_ship
    ${compiler_flags_ship}
    ${compiler_flags_release}               # the Ship configuration inherits from the Release configuration
)
set(exe_linker_flags_ship
    ${exe_linker_flags_ship}
    ${exe_linker_flags_release}             # the Ship configuration inherits from the Release configuration
)
set(shared_lib_linker_flags_ship
    ${shared_lib_linker_flags_ship}
    ${shared_lib_linker_flags_release}      # the Ship configuration inherits from the Release configuration
)

# Profile configuration.
set(compiler_flags_profile
    ${compiler_flags_profile}
    ${compiler_flags_ship}                  # the Profile configuration inherits from the Ship configuration
)
set(exe_linker_flags_profile
    ${exe_linker_flags_profile}
    ${exe_linker_flags_ship}                # the Profile configuration inherits from the Ship configuration
)
set(shared_lib_linker_flags_profile
    ${shared_lib_linker_flags_profile}
    ${shared_lib_linker_flags_ship}         # the Profile configuration inherits from the Ship configuration
)

# Apply Debug configuration compilation/linking flags.
convert_list_to_string(CMAKE_CXX_FLAGS_DEBUG "${compiler_flags_debug}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_DEBUG "${exe_linker_flags_debug}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_DEBUG "${shared_lib_linker_flags_debug}")

# Apply Release configuration compilation/linking flags.
convert_list_to_string(CMAKE_CXX_FLAGS_RELEASE "${compiler_flags_release}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_RELEASE "${exe_linker_flags_release}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${shared_lib_linker_flags_release}")

# Apply Ship configuration compilation/linking flags.
convert_list_to_string(CMAKE_CXX_FLAGS_SHIP "${compiler_flags_ship}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_SHIP "${exe_linker_flags_ship}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_SHIP "${shared_lib_linker_flags_ship}")

# Apply Profile configuration compilation/linking flags.
convert_list_to_string(CMAKE_CXX_FLAGS_PROFILE "${compiler_flags_profile}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_PROFILE "${exe_linker_flags_profile}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_PROFILE "${shared_lib_linker_flags_profile}")


#
# Build the individual products.
#

add_subdirectory(appleseed)
add_subdirectory(appleseed.shared)
add_subdirectory(appleseed.cli)
add_subdirectory(appleseed.studio)
