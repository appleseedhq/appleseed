
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013-2016 Esteban Tovagliari, The appleseedhq Organization
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
# Find OSL headers and libraries.
#
# This module defines the following variables:
#
#   OSL_FOUND           True if OSL was found
#   OSL_INCLUDE_DIRS    Where to find OSL header files
#   OSL_LIBRARIES       List of OSL libraries to link against
#   OSL_COMPILER        Path to oslc binary
#   OSL_QUERY_INFO      Path to oslinfo binary
#   OSL_MAKETX          Path to OpenImageIO's maketx binary
#

include (FindPackageHandleStandardArgs)

find_path (OSL_INCLUDE_DIR NAMES OSL/oslexec.h)

find_library (OSL_EXEC_LIBRARY NAMES oslexec)
find_library (OSL_COMP_LIBRARY NAMES oslcomp)
find_library (OSL_QUERY_LIBRARY NAMES oslquery)

find_program (OSL_COMPILER NAMES oslc)
find_program (OSL_QUERY_INFO NAMES oslinfo)
find_program (OSL_MAKETX NAMES maketx)

# Handle the QUIETLY and REQUIRED arguments and set OSL_FOUND.
find_package_handle_standard_args (OSL DEFAULT_MSG
    OSL_INCLUDE_DIR
    OSL_EXEC_LIBRARY
    OSL_COMP_LIBRARY
    OSL_QUERY_LIBRARY
    OSL_COMPILER
    OSL_QUERY_INFO
    OSL_MAKETX
)

# Set the output variables.
if (OSL_FOUND)
    set (OSL_INCLUDE_DIRS ${OSL_INCLUDE_DIR})
    set (OSL_LIBRARIES ${OSL_EXEC_LIBRARY} ${OSL_COMP_LIBRARY} ${OSL_QUERY_LIBRARY})
else ()
    set (OSL_INCLUDE_DIRS)
    set (OSL_LIBRARIES)
endif ()

mark_as_advanced (
    OSL_INCLUDE_DIR
    OSL_EXEC_LIBRARY
    OSL_COMP_LIBRARY
    OSL_QUERY_LIBRARY
)
