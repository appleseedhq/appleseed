
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
# Find Python3 headers and libraries.
#
# This module defines the following variables:
#
#   PYTHON3_FOUND            True if Python3 was found
#   PYTHON3_INCLUDE_DIRS     Where to find Python3 header files
#   PYTHON3_LIBRARIES        List of Python3 libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (PYTHON3_INCLUDE_DIR NAMES Python.h)
find_library (PYTHON3_LIBRARY NAMES python3)

# Handle the QUIETLY and REQUIRED arguments and set PYTHON3_FOUND.
find_package_handle_standard_args (PYTHON3 DEFAULT_MSG
    PYTHON3_INCLUDE_DIR
    PYTHON3_LIBRARY
)

# Set the output variables.
if (PYTHON3_FOUND)
    set (PYTHON3_INCLUDE_DIRS ${PYTHON3_INCLUDE_DIR})
    set (PYTHON3_LIBRARIES ${PYTHON3_LIBRARY})
else ()
    set (PYTHON3_INCLUDE_DIRS)
    set (PYTHON3_LIBRARIES)
endif ()

mark_as_advanced (
    PYTHON3_INCLUDE_DIR
    PYTHON3_LIBRARY
)
