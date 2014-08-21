
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

# Find Open Shading Language.
#
# This module defines:
#  OSL_INCLUDE_DIRS - where to find OSL includes.
#  OSL_LIBRARIES    - List of libraries when using OSL.
#  OSL_FOUND        - True if OSL found.

# Look for the header file.
find_path(OSL_INCLUDE_DIR NAMES OSL/oslexec.h)

# Look for the libraries.
find_library(OSL_EXEC_LIBRARY NAMES oslexec)
find_library(OSL_COMP_LIBRARY NAMES oslcomp)
find_library(OSL_QUERY_LIBRARY NAMES oslquery)

# handle the QUIETLY and REQUIRED arguments and set OSL_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(OSL DEFAULT_MSG OSL_INCLUDE_DIR
                                                  OSL_EXEC_LIBRARY
                                                  OSL_COMP_LIBRARY
                                                  OSL_QUERY_LIBRARY
                                                  )

# Copy the results to the output variables.
if(OSL_FOUND)
    set(OSL_LIBRARIES ${OSL_EXEC_LIBRARY} ${OSL_COMP_LIBRARY} ${OSL_QUERY_LIBRARY})
    set(OSL_INCLUDE_DIRS ${OSL_INCLUDE_DIR})
else(OSL_FOUND)
    set(OSL_LIBRARIES)
    set(OSL_INCLUDE_DIRS)
endif(OSL_FOUND)

#mark_as_advanced(OSL_INCLUDE_DIR OSL_EXEC_LIBRARY OSL_COMP_LIBRARY OSL_QUERY_LIBRARY)
