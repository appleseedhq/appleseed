
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2014-2015 Marius Avram, The appleseedhq Organization
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
# Find SeExprEditor headers and libraries.
#
# This module defines the following variables:
#
#   SEEXPREDITOR_FOUND          True if SeExprEditor was found
#   SEEXPREDITOR_INCLUDE_DIRS   Where to find SeExprEditor header files
#   SEEXPREDITOR_LIBRARIES      List of SeExprEditor libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (SEEXPREDITOR_INCLUDE_DIR NAMES SeExprEditor/SeExprEditor.h)

find_library (SEEXPREDITOR_LIBRARY NAMES SeExprEditor)

# Handle the QUIETLY and REQUIRED arguments and set SEEXPREDITOR_FOUND.
find_package_handle_standard_args (SEEXPREDITOR DEFAULT_MSG
    SEEXPREDITOR_INCLUDE_DIR
    SEEXPREDITOR_LIBRARY
)

# Set the output variables.
if (SEEXPREDITOR_FOUND)
    set (SEEXPREDITOR_INCLUDE_DIRS ${SEEXPR_INCLUDE_DIR})
    set (SEEXPREDITOR_LIBRARIES ${SEEXPREDITOR_LIBRARY})
else ()
    set (SEEXPREDITOR_INCLUDE_DIRS)
    set (SEEXPREDITOR_LIBRARIES)
endif ()

mark_as_advanced (
    SEEXPREDITOR_INCLUDE_DIR
    SEEXPREDITOR_LIBRARY
)
