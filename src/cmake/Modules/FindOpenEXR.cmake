
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013-2015 Esteban Tovagliari, The appleseedhq Organization
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

# Find OPENEXR headers and libraries.
#
# This module defines:
#  OPENEXR_INCLUDE_DIRS - where to find OPENEXR uncludes.
#  OPENEXR_LIBRARIES    - List of libraries when using OPENEXR.
#  OPENEXR_FOUND        - True if OPENEXR found.

# Look for the header file.
find_path( OPENEXR_INCLUDE_DIR NAMES OpenEXR/ImfHeader.h)

# Look for the libraries.
find_library( OPENEXR_IMF_LIBRARY NAMES IlmImf)
find_library( OPENEXR_THREADS_LIBRARY NAMES IlmThread)

# handle the QUIETLY and REQUIRED arguments and set OPENEXR_FOUND to TRUE if
# all listed variables are TRUE
include( FindPackageHandleStandardArgs)
find_package_handle_standard_args( OPENEXR DEFAULT_MSG  OPENEXR_INCLUDE_DIR
                                                        OPENEXR_IMF_LIBRARY
                                                        OPENEXR_THREADS_LIBRARY
                                                        )

# Copy the results to the output variables.
if( OPENEXR_FOUND)
    set( OPENEXR_LIBRARIES  ${OPENEXR_IMF_LIBRARY} ${OPENEXR_THREADS_LIBRARY})
    set( OPENEXR_INCLUDE_DIRS ${OPENEXR_INCLUDE_DIR})
else( OPENEXR_FOUND)
    set( OPENEXR_LIBRARIES)
    set( OPENEXR_INCLUDE_DIRS)
endif( OPENEXR_FOUND)

mark_as_advanced( OPENEXR_IMF_LIBRARY
                  OPENEXR_INCLUDE_DIR
                  OPENEXR_THREADS_LIBRARY
                  )
