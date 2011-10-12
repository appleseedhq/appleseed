#
# To be used by projects that make use of Cmakeified hdf5-1.8
#

#
# Find the HDF5 includes and get all installed hdf5 library settings from
# HDF5-config.cmake file : Requires a CMake compatible hdf5-1.8.5 or later 
# for this feature to work. The following vars are set if hdf5 is found.
#
# HDF5_FOUND               - True if found, otherwise all other vars are undefined
# HDF5_INCLUDE_DIR         - The include dir for main *.h files
# HDF5_FORTRAN_INCLUDE_DIR - The include dir for fortran modules and headers
# HDF5_VERSION_STRING      - full version (e.g. 1.8.5)
# HDF5_VERSION_MAJOR       - major part of version (e.g. 1.8)
# HDF5_VERSION_MINOR       - minor part (e.g. 5)
# 
# The following boolean vars will be defined
# HDF5_ENABLE_PARALLEL  - 1 if HDF5 parallel supported
# HDF5_BUILD_FORTRAN    - 1 if HDF5 was compiled with fortran on
# HDF5_BUILD_CPP_LIB    - 1 if HDF5 was compiled with cpp on
# HDF5_BUILD_TOOLS      - 1 if HDF5 was compiled with tools on
# HDF5_BUILD_HL_LIB     - 1 if HDF5 was compiled with high level on
# HDF5_BUILD_HL_CPP_LIB - 1 if HDF5 was compiled with high level and cpp on
# 
# Target names that are valid (depending on enabled options)
# will be the following
#
# hdf5              : HDF5 C library
# hdf5_tools        : the tools library
# hdf5_f90cstub     : used by Fortran to C interface
# hdf5_fortran      : Fortran HDF5 library
# hdf5_cpp          : HDF5 cpp interface library
# hdf5_hl           : High Level library
# hdf5_hl_f90cstub  : used by Fortran to C interface to High Level library
# hdf5_hl_fortran   : Fortran High Level library
# hdf5_hl_cpp       : High Level cpp interface library
# 
# To aid in finding HDF5 as part of a subproject set
# HDF5_ROOT_DIR_HINT to the location where hdf5-config.cmake lies

INCLUDE (SelectLibraryConfigurations)
INCLUDE (FindPackageHandleStandardArgs)

# The HINTS option should only be used for values computed from the system.
SET (_HDF5_HINTS
    $ENV{HOME}/.local
    $ENV{HDF5_ROOT}
    $ENV{HDF5_ROOT_DIR_HINT}
)
# Hard-coded guesses should still go in PATHS. This ensures that the user
# environment can always override hard guesses.
SET (_HDF5_PATHS
    $ENV{HOME}/.local
    $ENV{HDF5_ROOT}
    $ENV{HDF5_ROOT_DIR_HINT}
    /usr/lib/hdf5
    /usr/share/hdf5
    /usr/local/hdf5
    /usr/local/hdf5/share
)

FIND_PATH (HDF5_ROOT_DIR "hdf5-config.cmake"
    HINTS ${_HDF5_HINTS}
    PATHS ${_HDF5_PATHS}
    PATH_SUFFIXES
        lib/cmake/hdf5-1.8.7
        share/cmake/hdf5-1.8.7
)

FIND_PATH (HDF5_INCLUDE_DIRS "H5public.h"
    HINTS ${_HDF5_HINTS}
    PATHS ${_HDF5_PATHS}
    PATH_SUFFIXES
        include
        Include
)

# For backwards compatibility we set HDF5_INCLUDE_DIR to the value of
# HDF5_INCLUDE_DIRS
SET ( HDF5_INCLUDE_DIR "${HDF5_INCLUDE_DIRS}" )

IF (HDF5_INCLUDE_DIR)
  SET (HDF5_FOUND "YES")
  INCLUDE (${HDF5_ROOT_DIR}/hdf5-config.cmake)
ENDIF (HDF5_INCLUDE_DIR)
