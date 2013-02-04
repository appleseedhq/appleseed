#  Find Alembic library
# Find the native Alembic includes and libraries
# This module defines
#  ALEMBIC_INCLUDE_DIRS, where to find Alembic/Abc/All.h, set when
#                        ALEMBIC_INCLUDE_DIR is found.
#  ALEMBIC_LIBRARIES, libraries to link against to use Alembic.
#  ALEMBIC_ROOT_DIR, The base directory to search for Alembic.
#                    This can also be an environment variable.
#  ALEMBIC_FOUND, if false, do not try to use Alembic.
#

# if ALEMBIC_ROOT_DIR was defined in the environment, use it.
if( NOT ALEMBIC_ROOT_DIR AND NOT $ENV{ALEMBIC_ROOT_DIR} STREQUAL "")
  set( ALEMBIC_ROOT_DIR $ENV{ALEMBIC_ROOT_DIR})
endif()

set(_alembic_SEARCH_DIRS
  ${ALEMBIC_ROOT_DIR}
  /usr/local
  /sw # Fink
  /opt/local # DarwinPorts
  /opt/csw # Blastwave
)

find_path(ALEMBIC_INCLUDE_DIR NAMES Alembic/Abc/All.h HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES include)

find_library(ALEMBIC_ABC_LIBRARY 		NAMES AlembicAbc HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library(ALEMBIC_ABCGEOM_LIBRARY 		NAMES AlembicAbcGeom HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library(ALEMBIC_ABCCORE_ABS_LIBRARY 	NAMES AlembicAbcCoreAbstract HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library(ALEMBIC_ABCUTIL_LIBRARY 		NAMES AlembicUtil HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)
find_library(ALEMBIC_ABCCORE_HDF5_LIBRARY 	NAMES AlembicAbcCoreHDF5 HINTS ${_alembic_SEARCH_DIRS} PATH_SUFFIXES lib64 lib)

# handle the QUIETLY and REQUIRED arguments and set ALEMBIC_FOUND to TRUE if
# all listed variables are TRUE
include( FindPackageHandleStandardArgs)
find_package_handle_standard_args( ALEMBIC DEFAULT_MSG ALEMBIC_ABC_LIBRARY ALEMBIC_INCLUDE_DIR)

if( ALEMBIC_FOUND)
    set( ALEMBIC_LIBRARIES  ${ALEMBIC_ABC_LIBRARY}
                            ${ALEMBIC_ABCGEOM_LIBRARY}
                            ${ALEMBIC_ABCCORE_ABS_LIBRARY}
                            ${ALEMBIC_ABCUTIL_LIBRARY}
                            ${ALEMBIC_ABCCORE_HDF5_LIBRARY})

    set( ALEMBIC_INCLUDE_DIRS ${ALEMBIC_INCLUDE_DIR})
endif( ALEMBIC_FOUND)

mark_as_advanced( ALEMBIC_INCLUDE_DIR ALEMBIC_LIBRARY)
