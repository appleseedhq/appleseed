
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


# Find OpenColorIO headers and libraries.
#
#  This module defines
#  OPENCOLORIO_INCLUDE_DIRS - where to find OpenColorIO uncludes.
#  OPENCOLORIO_LIBRARIES    - List of libraries when using OpenColorIO.
#  OPENCOLORIO_FOUND        - True if OpenColorIO found.

# If OCIO is defined, it will try to look for OpenEXR libs there as well.

if (DEFINED OpenColorIO_ROOT)
    list (APPEND CMAKE_PREFIX_PATH ${OpenColorIO_ROOT})
endif()

# Look for the header file.
find_path (OPENCOLORIO_INCLUDE_DIR NAMES OpenColorIO/OpenColorIO.h)

# Look for the library.
find_library (OPENCOLORIO_LIBRARY NAMES OpenColorIO)

# handle the QUIETLY and REQUIRED arguments and set OPENCOLORIO_FOUND to TRUE if all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (OPENCOLORIO DEFAULT_MSG OPENCOLORIO_LIBRARY OPENCOLORIO_INCLUDE_DIR)

# Copy the results to the output variables.
if (OPENCOLORIO_FOUND)
    set (OPENCOLORIO_LIBRARIES ${OPENCOLORIO_LIBRARY})
    set (OPENCOLORIO_INCLUDE_DIRS ${OPENCOLORIO_INCLUDE_DIR})
else ()
    set (OPENCOLORIO_LIBRARIES)
    set (OPENCOLORIO_INCLUDE_DIRS)
endif ()

mark_as_advanced (OPENCOLORIO_INCLUDE_DIR OPENCOLORIO_LIBRARY)
