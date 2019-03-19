
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
# Find OpenImageIO headers and libraries.
#
# This module defines the following variables:
#
#   OPENIMAGEIO_FOUND           True if OpenImageIO was found
#   OPENIMAGEIO_INCLUDE_DIRS    Where to find OpenImageIO header files
#   OPENIMAGEIO_LIBRARIES       List of OpenImageIO libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (OPENIMAGEIO_INCLUDE_DIR NAMES OpenImageIO/imageio.h)

find_library (OPENIMAGEIO_LIBRARY NAMES OpenImageIO)

find_program (OPENIMAGEIO_OIIOTOOL NAMES oiiotool)
find_program (OPENIMAGEIO_IDIFF NAMES idiff)

# Handle the QUIETLY and REQUIRED arguments and set OPENIMAGEIO_FOUND.
find_package_handle_standard_args (OPENIMAGEIO DEFAULT_MSG
    OPENIMAGEIO_INCLUDE_DIR
    OPENIMAGEIO_LIBRARY
    OPENIMAGEIO_OIIOTOOL
    OPENIMAGEIO_IDIFF
)

# Set the output variables.
if (OPENIMAGEIO_FOUND)
    set (OPENIMAGEIO_INCLUDE_DIRS ${OPENIMAGEIO_INCLUDE_DIR})
    set (OPENIMAGEIO_LIBRARIES ${OPENIMAGEIO_LIBRARY})
else ()
    set (OPENIMAGEIO_INCLUDE_DIRS)
    set (OPENIMAGEIO_LIBRARIES)
endif ()

mark_as_advanced (
    OPENIMAGEIO_INCLUDE_DIR
    OPENIMAGEIO_LIBRARY
)
