cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
ADD_SUBDIRECTORY (${HDF5_F90_SOURCE_DIR}/src ${HDF5_F90_BINARY_DIR}/src)

#-----------------------------------------------------------------------------
# Build the Fortran Examples
#-----------------------------------------------------------------------------
IF (HDF5_BUILD_EXAMPLES)
  ADD_SUBDIRECTORY (${HDF5_F90_SOURCE_DIR}/examples ${HDF5_F90_BINARY_DIR}/examples)
ENDIF (HDF5_BUILD_EXAMPLES)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
IF (BUILD_TESTING)
  ADD_SUBDIRECTORY (${HDF5_F90_SOURCE_DIR}/test ${HDF5_F90_BINARY_DIR}/test)
  IF (H5_HAVE_PARALLEL)
    ADD_SUBDIRECTORY (${HDF5_F90_SOURCE_DIR}/testpar ${HDF5_F90_BINARY_DIR}/testpar)
  ENDIF (H5_HAVE_PARALLEL)
ENDIF (BUILD_TESTING)
