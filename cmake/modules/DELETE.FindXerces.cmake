
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
# Find Xerces-C headers and libraries.
#
# This module defines the following variables:
#
#   XERCES_FOUND            True if Xerces-C was found
#   XERCES_INCLUDE_DIRS     Where to find Xerces-C header files
#   XERCES_LIBRARIES        List of Xerces-C libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (XERCES_INCLUDE_DIR NAMES xercesc/parsers/SAXParser.hpp)

find_library (XERCES_LIBRARY NAMES xerces-c)

# Handle the QUIETLY and REQUIRED arguments and set XERCES_FOUND.
find_package_handle_standard_args (XERCES DEFAULT_MSG
    XERCES_INCLUDE_DIR
    XERCES_LIBRARY
)

# Set the output variables.
if (XERCES_FOUND)
    set (XERCES_INCLUDE_DIRS ${XERCES_INCLUDE_DIR})
    set (XERCES_LIBRARIES ${XERCES_LIBRARY})
else ()
    set (XERCES_INCLUDE_DIRS)
    set (XERCES_LIBRARIES)
endif ()

mark_as_advanced (
    XERCES_INCLUDE_DIR
    XERCES_LIBRARY
)
