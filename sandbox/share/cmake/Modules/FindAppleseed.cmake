
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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
# Find appleseed headers and libraries.
#
# This module requires the following variables to be defined:
#
#   APPLESEED_INCLUDE_DIR   Path to the src/appleseed directory
#   APPLESEED_LIBRARY       Path to the appleseed library file
#
# This module defines the following variables:
#
#   APPLESEED_FOUND         True if appleseed was found
#   APPLESEED_INCLUDE_DIRS  Where to find appleseed header files
#   APPLESEED_LIBRARIES     List of appleseed libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (APPLESEED_INCLUDE_DIR NAMES renderer/api/project.h)

find_library (APPLESEED_LIBRARY NAMES appleseed)

# Handle the QUIETLY and REQUIRED arguments and set APPLESEED_FOUND.
find_package_handle_standard_args (APPLESEED DEFAULT_MSG
    APPLESEED_INCLUDE_DIR
    APPLESEED_LIBRARY
)

# Set the output variables.
if (APPLESEED_FOUND)
    set (APPLESEED_INCLUDE_DIRS ${APPLESEED_INCLUDE_DIR})
    set (APPLESEED_LIBRARIES ${APPLESEED_LIBRARY})
else ()
    set (APPLESEED_INCLUDE_DIRS)
    set (APPLESEED_LIBRARIES)
endif ()

mark_as_advanced (
    APPLESEED_INCLUDE_DIR
    APPLESEED_LIBRARY
)
