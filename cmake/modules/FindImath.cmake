
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
# Find Imath headers and libraries.
#
# This module can take the following variables to define
# custom search locations:
#
#   ILMBASE_ROOT
#   ILMBASE_LOCATION
#
# This module defines the following variables:
#
#   IMATH_FOUND         True if Imath was found
#   IMATH_INCLUDE_DIRS  Where to find Imath header files
#   IMATH_LIBRARIES     List of Imath libraries to link against
#

# TODO: ILMBASE_ROOT to IMATH_ROOT
# If ILMBASE_ROOT is defined, it will try to look for Imath libs there
# as well.
#

if (DEFINED ILMBASE_ROOT)
    list (APPEND CMAKE_PREFIX_PATH ${ILMBASE_ROOT})
endif()

include (FindPackageHandleStandardArgs)

# TODO: Make it work without the 'APPEND' above.
find_path (IMATH_INCLUDE_DIR NAMES ImathVec.h
           PATH_SUFFIXES Imath
           HINTS ${ILMBASE_ROOT}
                 ${ILMBASE_LOCATION}
                 /usr/local/include
                 /usr/include
)

# find_library (IMATH_HALF_LIBRARY NAMES Half-2_3 Half-2_2 Half
#               PATH_SUFFIXES lib64 lib
#               HINTS ${ILMBASE_ROOT}
#                     ${ILMBASE_LOCATION}
#                     /usr/local
#                     /usr
# )

find_library (IMATH_MATH_LIBRARY NAMES Imath-2_3 Imath-2_2 Imath
              PATH_SUFFIXES lib64 lib
              HINTS ${ILMBASE_ROOT}
                    ${ILMBASE_LOCATION}
                    /usr/local
                    /usr
)

# Handle the QUIETLY and REQUIRED arguments and set IMATH_FOUND.
find_package_handle_standard_args (IMATH DEFAULT_MSG
    IMATH_INCLUDE_DIR
    IMATH_MATH_LIBRARY
)

# Set the output variables.
if (IMATH_FOUND)
    set (IMATH_INCLUDE_DIRS ${IMATH_INCLUDE_DIR})
    set (IMATH_LIBRARIES
        ${IMATH_MATH_LIBRARY}
    )
else ()
    set (IMATH_INCLUDE_DIRS)
    set (IMATH_LIBRARIES)
endif ()

mark_as_advanced (
    IMATH_INCLUDE_DIR
    IMATH_MATH_LIBRARY
)
