cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
ADD_SUBDIRECTORY (${HDF5_HL_F90_SOURCE_DIR}/src ${HDF5_HL_F90_BINARY_DIR}/src)

#-----------------------------------------------------------------------------
# Build the HL Fortran Examples
#-----------------------------------------------------------------------------
IF (HDF5_BUILD_FORTRAN_EXAMPLES)
  ADD_SUBDIRECTORY (${HDF5_HL_F90_SOURCE_DIR}/examples ${HDF5_HL_F90_BINARY_DIR}/examples)
ENDIF (HDF5_BUILD_FORTRAN_EXAMPLES)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
IF (BUILD_TESTING)
  ADD_SUBDIRECTORY (${HDF5_HL_F90_SOURCE_DIR}/test ${HDF5_HL_F90_BINARY_DIR}/test)
ENDIF (BUILD_TESTING)
