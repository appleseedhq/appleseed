cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_FORTRAN_TESTS C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Add Test Lib
#-----------------------------------------------------------------------------
ADD_LIBRARY (${HDF5_F90_C_TEST_LIB_TARGET} ${LIB_TYPE} t.c)
TARGET_LINK_LIBRARIES (${HDF5_F90_C_TEST_LIB_TARGET}
    ${HDF5_F90_C_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
)
H5_SET_LIB_OPTIONS (${HDF5_F90_C_TEST_LIB_TARGET} ${HDF5_F90_C_TEST_LIB_NAME} ${LIB_TYPE})

ADD_LIBRARY (${HDF5_F90_TEST_LIB_TARGET} ${LIB_TYPE} tf.f90)
SET (SHARED_LINK_FLAGS " ")
IF (WIN32 AND NOT CYGWIN)
  IF (BUILD_SHARED_LIBS)
    SET_PROPERTY (TARGET ${HDF5_F90_TEST_LIB_TARGET}
	    APPEND PROPERTY COMPILE_DEFINITIONS
		    BUILD_HDF5_DLL
	)
    IF (MSVC)
      SET (SHARED_LINK_FLAGS "/DLL")
    ENDIF (MSVC)
  ENDIF (BUILD_SHARED_LIBS)
  SET_PROPERTY (TARGET ${HDF5_F90_TEST_LIB_TARGET}
      APPEND PROPERTY COMPILE_DEFINITIONS
  	    HDF5F90_WINDOWS
  )
ENDIF (WIN32 AND NOT CYGWIN)
TARGET_FORTRAN_WIN_PROPERTIES (${HDF5_F90_TEST_LIB_TARGET} ${SHARED_LINK_FLAGS})
SET_TARGET_PROPERTIES (${HDF5_F90_TEST_LIB_TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
TARGET_LINK_LIBRARIES (${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_C_TEST_LIB_TARGET})
H5_SET_LIB_OPTIONS (${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_TEST_LIB_NAME} ${LIB_TYPE})

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for testhdf5_fortran
ADD_EXECUTABLE (testhdf5_fortran
    fortranlib_test.f90
    tH5A.f90
    tH5D.f90
    tH5E.f90
    tH5F.f90
    tH5G.f90
    tH5I.f90
    tH5P.f90
    tH5R.f90
    tH5S.f90
    tH5Sselect.f90
    tH5T.f90
    tH5VL.f90
    tH5Z.f90
)
TARGET_NAMING (testhdf5_fortran ${LIB_TYPE})
TARGET_FORTRAN_WIN_PROPERTIES (testhdf5_fortran "")
TARGET_LINK_LIBRARIES (testhdf5_fortran 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
IF (WIN32 AND MSVC)
  TARGET_LINK_LIBRARIES (testhdf5_fortran "ws2_32.lib")
ENDIF (WIN32 AND MSVC)
SET_TARGET_PROPERTIES (testhdf5_fortran PROPERTIES LINKER_LANGUAGE Fortran)

ADD_TEST (NAME testhdf5_fortran COMMAND $<TARGET_FILE:testhdf5_fortran>)

#-- Adding test for testhdf5_fortran_1_8
ADD_EXECUTABLE (testhdf5_fortran_1_8
    fortranlib_test_1_8.f90
    tH5F.f90
    tH5O.f90
    tH5A_1_8.f90
    tH5G_1_8.f90
)
TARGET_NAMING (testhdf5_fortran_1_8 ${LIB_TYPE})
TARGET_FORTRAN_WIN_PROPERTIES (testhdf5_fortran_1_8 "")
TARGET_LINK_LIBRARIES (testhdf5_fortran_1_8 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
IF (WIN32 AND MSVC)
  TARGET_LINK_LIBRARIES (testhdf5_fortran_1_8 "ws2_32.lib")
ENDIF (WIN32 AND MSVC)
SET_TARGET_PROPERTIES (testhdf5_fortran_1_8 PROPERTIES LINKER_LANGUAGE Fortran)

ADD_TEST (NAME testhdf5_fortran_1_8 COMMAND $<TARGET_FILE:testhdf5_fortran_1_8>)

#-- Adding test for fflush1
ADD_EXECUTABLE (fflush1 fflush1.f90)
TARGET_NAMING (fflush1 ${LIB_TYPE})
TARGET_FORTRAN_WIN_PROPERTIES (fflush1 "")
TARGET_LINK_LIBRARIES (fflush1 
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
IF (WIN32 AND MSVC)
  TARGET_LINK_LIBRARIES (fflush1 "ws2_32.lib")
ENDIF (WIN32 AND MSVC)
SET_TARGET_PROPERTIES (fflush1 PROPERTIES LINKER_LANGUAGE Fortran)

ADD_TEST (NAME fflush1 COMMAND $<TARGET_FILE:fflush1>)

#-- Adding test for fflush2
ADD_EXECUTABLE (fflush2 fflush2.f90)
TARGET_NAMING (fflush2 ${LIB_TYPE})
TARGET_FORTRAN_WIN_PROPERTIES (fflush2 "")
TARGET_LINK_LIBRARIES (fflush2 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
IF (WIN32 AND MSVC)
  TARGET_LINK_LIBRARIES (fflush2 "ws2_32.lib")
ENDIF (WIN32 AND MSVC)
SET_TARGET_PROPERTIES (fflush2 PROPERTIES LINKER_LANGUAGE Fortran)

ADD_TEST (NAME fflush2 COMMAND $<TARGET_FILE:fflush2>)
SET_TESTS_PROPERTIES(fflush2 PROPERTIES DEPENDS fflush1)
