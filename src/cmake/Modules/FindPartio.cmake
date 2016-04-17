
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2015-2016 Esteban Tovagliari, The appleseedhq Organization
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
# Find Partio headers and libraries.
#
# This module defines the following variables:
#
#   PARTIO_FOUND            True if Partio was found
#   PARTIO_INCLUDE_DIRS     Where to find Partio header files
#   PARTIO_LIBRARIES        List of Partio libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (PARTIO_INCLUDE_DIR NAMES Partio.h)

find_library (PARTIO_LIBRARY NAMES partio)

# Handle the QUIETLY and REQUIRED arguments and set PARTIO_FOUND.
find_package_handle_standard_args (PARTIO DEFAULT_MSG
    PARTIO_INCLUDE_DIR
    PARTIO_LIBRARY
)

# Set the output variables.
if (PARTIO_FOUND)
    set (PARTIO_INCLUDE_DIRS ${PARTIO_INCLUDE_DIR})
    set (PARTIO_LIBRARIES ${PARTIO_LIBRARY})
else ()
    set (PARTIO_INCLUDE_DIRS)
    set (PARTIO_LIBRARIES)
endif ()

mark_as_advanced (
    PARTIO_INCLUDE_DIR
    PARTIO_LIBRARY
)
