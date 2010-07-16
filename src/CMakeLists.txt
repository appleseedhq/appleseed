
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
# Compilation/Linking Flags.
#

# Profile Configuration.
set(CMAKE_CXX_FLAGS_PROFILE ${CMAKE_CXX_FLAGS_RELEASE})
set(CMAKE_EXE_LINKER_FLAGS_PROFILE ${CMAKE_EXE_LINKER_FLAGS_RELEASE})
set(CMAKE_SHARED_LINKER_FLAGS_PROFILE ${CMAKE_SHARED_LINKER_FLAGS_RELEASE})

# Ship Configuration.
set(CMAKE_CXX_FLAGS_SHIP ${CMAKE_CXX_FLAGS_RELEASE})
set(CMAKE_EXE_LINKER_FLAGS_SHIP ${CMAKE_EXE_LINKER_FLAGS_RELEASE})
set(CMAKE_SHARED_LINKER_FLAGS_SHIP ${CMAKE_SHARED_LINKER_FLAGS_RELEASE})


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
# Preprocessor Definitions.
#

set(preprocessor_definitions_common
    WIN32
    XML_LIBRARY                         # to allow static linking against Xerces-C
    _CRT_SECURE_NO_WARNINGS
    _USRDLL
    _WINDOWS
)    

set(preprocessor_definitions_debug
    ${preprocessor_definitions_common}
    DEBUG
    _DEBUG
    _HAS_ITERATOR_DEBUGGING=0
)    

set(preprocessor_definitions_release
    ${preprocessor_definitions_common}
    NDEBUG
    _SECURE_SCL=0
)    


#
# Platform-specific Options.
#

if(MSVC)

    # Treat warnings as errors.
    add_definitions(/WX)

    # Disable the following warnings:
    #   C4290: C++ exception specification ignored except to indicate a function is not __declspec(nothrow)
    #   C4355: 'this' : used in base member initializer list
    add_definitions(/wd4290 /wd4355)

endif()


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

    macro(link_against_static_libraries target)

        # OpenEXR.
        target_link_libraries(${target}
            ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Half/\$\(ConfigurationName\)/Half.lib
            ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Iex/\$\(ConfigurationName\)/Iex.lib
            ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/IlmImf/\$\(ConfigurationName\)/IlmImf.lib
            ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/IlmThread/\$\(ConfigurationName\)/IlmThread.lib
            ${CMAKE_SOURCE_DIR}/build/win32.vs90/openexr/Imath/\$\(ConfigurationName\)/Imath.lib
        )

        # zlib.
        target_link_libraries(${target}
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/zlib/Win32_LIB_ASM_\$\(ConfigurationName\)/zlibd.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/zlib/Win32_LIB_ASM_\$\(ConfigurationName\)/zlib.lib
        )

        # libpng.
        target_link_libraries(${target}
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/libpng/Win32_LIB_ASM_\$\(ConfigurationName\)/libpngd.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/libpng/Win32_LIB_ASM_\$\(ConfigurationName\)/libpng.lib
        )

        # Xerces-C.
        target_link_libraries(${target}
            debug       ${CMAKE_SOURCE_DIR}/build/win32.vs90/xerces-c/Static\ Debug/xerces-c_static_2D.lib
            optimized   ${CMAKE_SOURCE_DIR}/build/win32.vs90/xerces-c/Static\ Release/xerces-c_static_2.lib
        )

    endmacro(link_against_static_libraries)

endif()


#
# Recurse into subdirectories.
#

add_subdirectory(appleseed)
add_subdirectory(appleseed.shared)
add_subdirectory(appleseed.cli)
add_subdirectory(appleseed.studio)
