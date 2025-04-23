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
# Find Alembic headers and libraries.
#
# This module requires the following variables to be defined:
#
#   ALEMBIC_INCLUDE_DIR
#   ALEMBIC_ABC_LIBRARY
#
# The following variables are optional:
#
#   ALEMBIC_ABCGEOM_LIBRARY
#   ALEMBIC_ABCCORE_ABS_LIBRARY
#   ALEMBIC_ABCUTIL_LIBRARY
#   ALEMBIC_ABCCORE_HDF5_LIBRARY
#
# This module defines the following variables:
#
#   ALEMBIC_FOUND                   True if Alembic was found
#   ALEMBIC_INCLUDE_DIRS            Where to find Alembic header files
#   ALEMBIC_LIBRARIES               List of Alembic libraries to link against
#   ALEMBIC_ROOT_DIR                The base directory to search for Alembic
#                                   This can also be an environment variable
#

include (FindPackageHandleStandardArgs)

# If ALEMBIC_ROOT_DIR was defined in the environment, use it.
if (NOT ALEMBIC_ROOT_DIR AND NOT $ENV{ALEMBIC_ROOT_DIR} STREQUAL "")
    set (ALEMBIC_ROOT_DIR $ENV{ALEMBIC_ROOT_DIR})
endif ()

set (_alembic_SEARCH_DIRS
    ${ALEMBIC_ROOT_DIR}
    /usr/local
    /sw             # Fink
    /opt/local      # DarwinPorts
    /opt/csw        # Blastwave
)

find_path (ALEMBIC_INCLUDE_DIR NAMES Alembic/Abc/All.h HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES include)

find_library (ALEMBIC_ABC_LIBRARY            NAMES AlembicAbc HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library (ALEMBIC_ABCGEOM_LIBRARY        NAMES AlembicAbcGeom HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library (ALEMBIC_ABCCORE_ABS_LIBRARY    NAMES AlembicAbcCoreAbstract HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library (ALEMBIC_ABCUTIL_LIBRARY        NAMES AlembicUtil HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library (ALEMBIC_ABCCORE_HDF5_LIBRARY   NAMES AlembicAbcCoreHDF5 HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)

# Handle the QUIETLY and REQUIRED arguments and set ALEMBIC_FOUND.
find_package_handle_standard_args (ALEMBIC DEFAULT_MSG
    ALEMBIC_INCLUDE_DIR
    ALEMBIC_ABC_LIBRARY
)

# Set the output variables.
if (ALEMBIC_FOUND)
    set (ALEMBIC_INCLUDE_DIRS ${ALEMBIC_INCLUDE_DIR})
    set (ALEMBIC_LIBRARIES
        ${ALEMBIC_ABC_LIBRARY}
        ${ALEMBIC_ABCGEOM_LIBRARY}
        ${ALEMBIC_ABCCORE_ABS_LIBRARY}
        ${ALEMBIC_ABCUTIL_LIBRARY}
        ${ALEMBIC_ABCCORE_HDF5_LIBRARY}
    )
else ()
    set (ALEMBIC_INCLUDE_DIRS)
    set (ALEMBIC_LIBRARIES)
endif ()

mark_as_advanced (
    ALEMBIC_INCLUDE_DIR
    ALEMBIC_LIBRARY
)
