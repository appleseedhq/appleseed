cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_FORTRAN_TESTPAR C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for parallel_test
ADD_EXECUTABLE (parallel_test
    ptest.f90
    hyper.f90
    mdset.f90
)
TARGET_NAMING (parallel_test ${LIB_TYPE})
TARGET_FORTRAN_WIN_PROPERTIES (parallel_test "")
TARGET_LINK_LIBRARIES (parallel_test 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
IF (WIN32 AND MSVC)
  TARGET_LINK_LIBRARIES (parallel_test "ws2_32.lib")
ENDIF (WIN32 AND MSVC)
SET_TARGET_PROPERTIES (parallel_test PROPERTIES LINKER_LANGUAGE Fortran)

ADD_TEST (NAME parallel_test COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:parallel_test>)
