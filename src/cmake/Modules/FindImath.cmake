
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013-2014 Esteban Tovagliari, The appleseedhq Organization
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

# Find Imath headers and libraries.
#
# This module defines:
#  IMATH_INCLUDE_DIRS - where to find IMATH uncludes.
#  IMATH_LIBRARIES    - List of libraries when using IMATH.
#  IMATH_FOUND        - True if IMATH found.

# Look for the header file.
find_path( IMATH_INCLUDE_DIR NAMES OpenEXR/ImathVec.h)

# Look for the libraries.
find_library( IMATH_HALF_LIBRARY NAMES Half)
find_library( IMATH_IEX_LIBRARY NAMES Iex)
find_library( IMATH_MATH_LIBRARY NAMES Imath)

# handle the QUIETLY and REQUIRED arguments and set IMATH_FOUND to TRUE if
# all listed variables are TRUE
include( FindPackageHandleStandardArgs)
find_package_handle_standard_args( IMATH DEFAULT_MSG    IMATH_HALF_LIBRARY
                                                        IMATH_IEX_LIBRARY
                                                        IMATH_MATH_LIBRARY
                                                        IMATH_INCLUDE_DIR
                                                        )
# Copy the results to the output variables.
if( IMATH_FOUND)
    set( IMATH_LIBRARIES ${IMATH_HALF_LIBRARY}
                         ${IMATH_IEX_LIBRARY}
                         ${IMATH_MATH_LIBRARY}
                         )

    set( IMATH_INCLUDE_DIRS ${IMATH_INCLUDE_DIR})
else( IMATH_FOUND)
    set( IMATH_LIBRARIES)
    set( IMATH_INCLUDE_DIRS)
endif( IMATH_FOUND)

mark_as_advanced( IMATH_HALF_LIBRARY
                  IMATH_IEX_LIBRARY
                  IMATH_MATH_LIBRARY
                  IMATH_INCLUDE_DIR
                  )
