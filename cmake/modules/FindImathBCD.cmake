
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
#   BCD_ILMBASE_ROOT
#
# This module defines the following variables:
#
#   BCD_IMATH_FOUND         True if Imath was found
#   BCD_IMATH_INCLUDE_DIRS  Where to find Imath header files
#   BCD_IMATH_LIBRARIES     List of Imath libraries to link against
#

if (DEFINED BCD_ILMBASE_ROOT)
    list (APPEND CMAKE_PREFIX_PATH ${BCD_ILMBASE_ROOT})
endif()

include (FindPackageHandleStandardArgs)

find_path (BCD_IMATH_INCLUDE_DIR NAMES ImathVec.h
           PATH_SUFFIXES OpenEXR
           HINTS ${BCD_ILMBASE_ROOT}
                 /usr/local/include
                 /usr/include
)

find_library (BCD_IMATH_HALF_LIBRARY NAMES Half-2_3 Half-2_2 Half
              PATH_SUFFIXES lib64 lib
              HINTS ${BCD_ILMBASE_ROOT}
                    /usr/local
                    /usr
)
find_library (BCD_IMATH_IEX_LIBRARY NAMES Iex-2_3 Iex-2_2 Iex
              PATH_SUFFIXES lib64 lib
              HINTS ${BCD_ILMBASE_ROOT}
                    /usr/local
                    /usr
)
find_library (BCD_IMATH_MATH_LIBRARY NAMES Imath-2_3 Imath-2_2 Imath
              PATH_SUFFIXES lib64 lib
              HINTS ${BCD_ILMBASE_ROOT}
                    /usr/local
                    /usr
)

# Handle the QUIETLY and REQUIRED arguments and set IMATH_FOUND.
find_package_handle_standard_args (BCD_IMATH DEFAULT_MSG
    BCD_IMATH_INCLUDE_DIR
    BCD_IMATH_HALF_LIBRARY
    BCD_IMATH_IEX_LIBRARY
    BCD_IMATH_MATH_LIBRARY
)

# Set the output variables.
if (BCD_IMATH_FOUND)
    set (BCD_IMATH_INCLUDE_DIRS ${IMATH_INCLUDE_DIR})
    set (BCD_IMATH_LIBRARIES
        ${BCD_IMATH_HALF_LIBRARY}
        ${BCD_IMATH_IEX_LIBRARY}
        ${BCD_IMATH_MATH_LIBRARY}
    )
else ()
    set (BCD_IMATH_INCLUDE_DIRS)
    set (BCD_IMATH_LIBRARIES)
endif ()

mark_as_advanced (
    BCD_IMATH_INCLUDE_DIR
    BCD_IMATH_HALF_LIBRARY
    BCD_IMATH_IEX_LIBRARY
    BCD_IMATH_MATH_LIBRARY
)
