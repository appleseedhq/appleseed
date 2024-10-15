
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
# Find OpenEXR headers and libraries.
#
# This module can take the following variables to define
# custom search locations:
#
#   BCD_OPENEXR_ROOT

# This module defines the following variables:
#
#   BCD_OPENEXR_FOUND           True if OpenEXR was found
#   BCD_OPENEXR_INCLUDE_DIRS    Where to find OpenEXR header files
#   BCD_OPENEXR_LIBRARIES       List of OpenEXR libraries to link against
#

include (FindPackageHandleStandardArgs)

find_path (BCD_OPENEXR_INCLUDE_DIR NAMES ImfHeader.h
           PATH_SUFFIXES OpenEXR
           HINTS ${BCD_OPENEXR_ROOT}/include
                 /usr/local/include
                 /usr/include
)

find_library (BCD_OPENEXR_IMF_LIBRARY
              NAMES IlmImf-2_3 IlmImf-2_2
              PATH_SUFFIXES lib64 lib
              HINTS ${BCD_OPENEXR_ROOT}
                    /usr/local
                    /usr
)

find_library (BCD_OPENEXR_THREADS_LIBRARY
              NAMES IlmThread-2_3 IlmThread-2_2
              PATH_SUFFIXES lib64 lib
              HINTS ${BCD_OPENEXR_ROOT}
                    /usr/local
                    /usr
)

# Handle the QUIETLY and REQUIRED arguments and set OPENEXR_FOUND.
find_package_handle_standard_args (BCD_OPENEXR DEFAULT_MSG
    BCD_OPENEXR_INCLUDE_DIR
    BCD_OPENEXR_IMF_LIBRARY
    BCD_OPENEXR_THREADS_LIBRARY
)

# Set the output variables.
if (BCD_OPENEXR_FOUND)
    set (BCD_OPENEXR_INCLUDE_DIRS ${BCD_OPENEXR_INCLUDE_DIR})
    set (BCD_OPENEXR_LIBRARIES  ${BCD_OPENEXR_IMF_LIBRARY} ${BCD_OPENEXR_THREADS_LIBRARY})
else ()
    set (BCD_OPENEXR_INCLUDE_DIRS)
    set (BCD_OPENEXR_LIBRARIES)
endif ()

mark_as_advanced (
    BCD_OPENEXR_INCLUDE_DIR
    BCD_OPENEXR_IMF_LIBRARY
    BCD_OPENEXR_THREADS_LIBRARY
)
