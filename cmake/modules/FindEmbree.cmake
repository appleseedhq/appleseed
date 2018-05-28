
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2018 Fedor Matantsev, The appleseedhq Organization
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
# Find Embree headers and libraries.
#
# This module defines the following variables:
#
#   EMBREE_FOUND            True if Embree was found
#   EMBREE_INCLUDE_DIRS     Where to find Embree header files
#   EMBREE_LIBRARIES        List of Embree libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (EMBREE_INCLUDE_DIR NAMES embree3/rtcore.h)

find_library (EMBREE_LIBRARY NAMES embree3)

# Handle the QUIETLY and REQUIRED arguments and set EMBREE_FOUND.
find_package_handle_standard_args (EMBREE DEFAULT_MSG
    EMBREE_INCLUDE_DIR
    EMBREE_LIBRARY
)

# Set the output variables.
if (EMBREE_FOUND)
    set (EMBREE_INCLUDE_DIRS ${EMBREE_INCLUDE_DIR})
    set (EMBREE_LIBRARIES ${EMBREE_LIBRARY})
else ()
    set (EMBREE_INCLUDE_DIRS)
    set (EMBREE_LIBRARIES)
endif ()

mark_as_advanced (
    EMBREE_INCLUDE_DIR
    EMBREE_LIBRARY
)
