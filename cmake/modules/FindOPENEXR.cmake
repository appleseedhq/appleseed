
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013-2018 Esteban Tovagliari, The appleseedhq Organization
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
# Find OpenEXR headers and libraries.
#
# This module can take the following variables to define
# custom search locations:
#
#   OPENEXR_ROOT
#   OPENEXR_LOCATION

# This module defines the following variables:
#
#   OPENEXR_FOUND           True if OpenEXR was found
#   OPENEXR_INCLUDE_DIRS    Where to find OpenEXR header files
#   OPENEXR_LIBRARIES       List of OpenEXR libraries to link against
#

# If OPENEXR_ROOT is defined, it will try to look for OpenEXR libs there
# as well.
#

if (DEFINED OPENEXR_ROOT)
    list (APPEND CMAKE_PREFIX_PATH ${OPENEXR_ROOT})
endif()

include (FindPackageHandleStandardArgs)

# TODO: Make it work without the 'APPEND' above.
find_path (OPENEXR_INCLUDE_DIR NAMES ImfHeader.h
           PATH_SUFFIXES OpenEXR
           HINTS ${OPENEXR_ROOT}/include
                 ${OPENEXR_LOCATION}/include
                 /usr/local/include
                 /usr/include
)

# TODO: rename OPENEXR_IMF_LIBRARY
find_library (OPENEXR_IMF_LIBRARY NAMES OpenEXR
              PATH_SUFFIXES lib64 lib
              HINTS ${OPENEXR_ROOT}
                    ${OPENEXR_LOCATION}
                    /usr/local
                    /usr
)

find_library (OPENEXR_THREADS_LIBRARY
              NAMES IlmThread-2_3 IlmThread-2_2 IlmThread
              PATH_SUFFIXES lib64 lib
              HINTS ${OPENEXR_ROOT}
                    ${OPENEXR_LOCATION}
                    /usr/local
                    /usr
)

find_library (IMATH_IEX_LIBRARY NAMES Iex-2_3 Iex-2_2 Iex
              PATH_SUFFIXES lib64 lib
              HINTS ${ILMBASE_ROOT}
                    ${ILMBASE_LOCATION}
                    /usr/local
                    /usr
)

# Handle the QUIETLY and REQUIRED arguments and set OPENEXR_FOUND.
find_package_handle_standard_args (OPENEXR DEFAULT_MSG
    OPENEXR_INCLUDE_DIR
    OPENEXR_IMF_LIBRARY
    OPENEXR_THREADS_LIBRARY
    IMATH_IEX_LIBRARY
)

# Set the output variables.
if (OPENEXR_FOUND)
    set (OPENEXR_INCLUDE_DIRS ${OPENEXR_INCLUDE_DIR})
    set (OPENEXR_LIBRARIES
        ${OPENEXR_IMF_LIBRARY}
        ${OPENEXR_THREADS_LIBRARY}
        ${IMATH_IEX_LIBRARY}
    )
else ()
    set (OPENEXR_INCLUDE_DIRS)
    set (OPENEXR_LIBRARIES)
endif ()

mark_as_advanced (
    OPENEXR_INCLUDE_DIR
    OPENEXR_IMF_LIBRARY
    OPENEXR_THREADS_LIBRARY
    IMATH_IEX_LIBRARY
)
