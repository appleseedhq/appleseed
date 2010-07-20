
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
# Include Paths.
#

include_directories(
    appleseed
    boost
    libpng
    openexr/vc/vc9/include
    openexr/vc/vc9/include/openexr
    xerces-c/src
    zlib
)


#
# Preprocessor definitions common to all configurations.
#

# All Platforms.
set(preprocessor_definitions_common
)

# Windows.
if(WIN32)
    set(preprocessor_definitions_common
        ${preprocessor_definitions_common}
        WIN32
        XML_LIBRARY                         # allow static linking against Xerces-C
        _CRT_SECURE_NO_WARNINGS
        _USRDLL
        _WINDOWS
    )
endif()


#
# Preprocessor definitions for Debug configuration.
#

# All Platforms.
set(preprocessor_definitions_debug
    ${preprocessor_definitions_common}
    DEBUG
    _DEBUG
)

# Visual Studio (any version) on Windows.
if(WIN32 AND MSVC)
    set(preprocessor_definitions_debug
        ${preprocessor_definitions_debug}
        _HAS_ITERATOR_DEBUGGING=0
    )
endif()


#
# Preprocessor definitions for Release configuration.
#

# All Platforms.
set(preprocessor_definitions_release
    ${preprocessor_definitions_common}
    NDEBUG
)

# Visual Studio (any version) on Windows.
if(WIN32 AND MSVC)
    set(preprocessor_definitions_release
        ${preprocessor_definitions_release}
        _SECURE_SCL=0
    )
endif()


#
# Preprocessor definitions for Ship configuration.
#

# All Platforms.
set(preprocessor_definitions_ship
    ${preprocessor_definitions_release}
    SHIP
)


#
# Preprocessor definitions for Profile configuration.
#

# All Platforms.
set(preprocessor_definitions_profile
    ${preprocessor_definitions_release}
)


#
# Convenience macros to set preprocessor definitions.
#

macro(append_common_preprocessor_definitions target)
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
# Compilation/linking flags common to all configurations.
#

# Visual Studio (any version) on Windows.
if(WIN32 AND MSVC)
    set(compiler_flags_common
        /WX                                 # Treat Warnings As Errors
        /GF                                 # Enable String Pooling
        /wd4290                             # C4290: C++ exception specification ignored except to indicate a function is not __declspec(nothrow)
        /wd4355                             # C4355: 'this' : used in base member initializer list
    )

    set(linker_flags_common
        /WX                                 # Treat Warnings As Errors
    )
endif()


#
# Compilation/linking flags for Debug configuration.
#

# Visual Studio (any version) on Windows.
if(WIN32 AND MSVC)
    set(compiler_flags_debug
        ${compiler_flags_common}
        /ZI                                 # set Debug Information Format to Program Database for Edit & Continue
        /MDd                                # set Runtime Library to Multi-threaded Debug DLL
    )

    set(linker_flags_debug
        ${linker_flags_common}
    )
endif()


#
# Compilation/linking flags for Release configuration.
#

# Visual Studio (any version) on Windows.
if(WIN32 AND MSVC)
    set(compiler_flags_release
        ${compiler_flags_common}
        /Zi                                 # set Debug Information Format to Program Database
        /Ox                                 # Full Optimization
        /Ob2                                # set Inline Function Expansion to Any Suitable
        /Oi                                 # Enable Intrinsic Functions
        /Ot                                 # Favor Fast Code
        /Oy                                 # Omit Frame Pointers
        /MD                                 # set Runtime Library to Multi-threaded DLL
        /GS-                                # set Buffer Security Check to No
        /arch:SSE2                          # Streaming SIMD Extensions 2
        /fp:fast                            # set Floating Point Model to Fast
    )

    set(linker_flags_release
        ${linker_flags_common}
        /OPT:REF                            # Eliminate Unreferenced Data
        /OPT:ICF                            # Remove Redundant COMDATs
    )
endif()


#
# Compilation/linking flags for Ship configuration.
#

# Visual Studio (any version) on Windows.
if(WIN32 AND MSVC)
    set(compiler_flags_ship
        ${compiler_flags_release}
        /GL                                 # Enable link-time code generation
    )

    set(linker_flags_ship
        ${linker_flags_release}
        /LTCG                               # Use Link Time Code Generation
    )
endif()


#
# Compilation/linking flags for Profile configuration.
#

# Visual Studio (any version) on Windows.
if(WIN32 AND MSVC)
    set(compiler_flags_profile
        ${compiler_flags_ship}
    )

    set(linker_flags_profile
        ${linker_flags_ship}
        /DEBUG                              # Generate Debug Info
    )
endif()


#
# Set Compilation/Linking Flags.
#

# Debug.
convert_list_to_string(CMAKE_CXX_FLAGS_DEBUG "${compiler_flags_debug}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_DEBUG "${linker_flags_debug}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_DEBUG "${linker_flags_debug}")

# Release.
convert_list_to_string(CMAKE_CXX_FLAGS_RELEASE "${compiler_flags_release}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_RELEASE "${linker_flags_release}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${linker_flags_release}")

# Ship.
convert_list_to_string(CMAKE_CXX_FLAGS_SHIP "${compiler_flags_ship}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_SHIP "${linker_flags_ship}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_SHIP "${linker_flags_ship}")

# Profile.
convert_list_to_string(CMAKE_CXX_FLAGS_PROFILE "${compiler_flags_profile}")
convert_list_to_string(CMAKE_EXE_LINKER_FLAGS_PROFILE "${linker_flags_profile}")
convert_list_to_string(CMAKE_SHARED_LINKER_FLAGS_PROFILE "${linker_flags_profile}")


#
# Static Libraries.
#

if(WIN32 AND MSVC90)

    # boost.
    # We only provide the search path for boost libraries, since on Windows,
    # the boost libraries link themselves through #pragma statements. Note
    # that this instruction needs to appear before any targets that needs it,
    # hence it is not included in the macro below.
    link_directories(${CMAKE_SOURCE_DIR}/build/win32.vs90/boost/lib)

    macro(link_against_common_static_libraries target)

        # OpenEXR.
        target_link_libraries(${target}
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Half/Debug/Half.lib
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Iex/Debug/Iex.lib
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/IlmImf/Debug/IlmImf.lib
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/IlmThread/Debug/IlmThread.lib
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Imath/Debug/Imath.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Half/Release/Half.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Iex/Release/Iex.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/IlmImf/Release/IlmImf.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/IlmThread/Release/IlmThread.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Imath/Release/Imath.lib
        )

        # zlib.
        target_link_libraries(${target}
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/zlib/Win32_LIB_ASM_Debug/zlibd.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/zlib/Win32_LIB_ASM_Release/zlib.lib
        )

        # libpng.
        target_link_libraries(${target}
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/libpng/Win32_LIB_ASM_Debug/libpngd.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/libpng/Win32_LIB_ASM_Release/libpng.lib
        )

        # Xerces-C.
        target_link_libraries(${target}
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/xerces-c/Static\ Debug/xerces-c_static_2D.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/xerces-c/Static\ Release/xerces-c_static_2.lib
        )

    endmacro()

endif()


#
# Copy a target binary to the sandbox.
#

macro(add_copy_target_to_sandbox_command target)
    if(WIN32 AND MSVC90)
        slashes_to_backslashes(project_source_dir_win ${PROJECT_SOURCE_DIR})
        
        set(bin_directory ${project_source_dir_win}\\sandbox\\bin\\win32.vs90\\$(ConfigurationName))
        
        add_custom_command(TARGET ${target} POST_BUILD
            COMMAND if not exist ${bin_directory} mkdir ${bin_directory}
            COMMAND copy $(TargetPath) ${bin_directory}
        )
    endif()
endmacro()


#
# Build the individual products.
#

add_subdirectory(appleseed)
add_subdirectory(appleseed.shared)
add_subdirectory(appleseed.cli)
add_subdirectory(appleseed.studio)
