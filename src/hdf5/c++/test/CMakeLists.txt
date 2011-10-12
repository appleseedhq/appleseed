cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_CPP_TEST)
# --------------------------------------------------------------------
# Notes: When creating unit test executables they should be prefixed
# with "cpp_". This allows for easier filtering of the test suite when
# using ctest. An example would be
#  ctest -R cpp_
# which would only run the C++ based unit tests.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (CPP_TEST_SRCS
    ${HDF5_CPP_TEST_SOURCE_DIR}/testhdf5.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tattr.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tcompound.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tfile.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tfilter.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/th5s.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tlinks.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/trefer.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/ttypes.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tvlstr.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/dsets.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/h5cpputil.cpp
)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
SET (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
CONFIGURE_FILE (${HDF5_CPP_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})

INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR} )

ADD_EXECUTABLE (cpp_testhdf5 ${CPP_TEST_SRCS} )
TARGET_NAMING (cpp_testhdf5 ${LIB_TYPE})
TARGET_LINK_LIBRARIES (cpp_testhdf5
    ${HDF5_CPP_LIB_TARGET}
    ${HDF5_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
)

ADD_TEST (NAME cpp_testhdf5 COMMAND $<TARGET_FILE:cpp_testhdf5>)

IF (HDF5_TEST_VFD)

  SET (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )
  
  IF (DIRECT_VFD)
    SET (VFD_LIST ${VFD_LIST} direct)
  ENDIF (DIRECT_VFD)

  MACRO (ADD_VFD_TEST vfdname resultcode)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
        NAME VFD-${vfdname}-cpp_testhdf5 
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:cpp_testhdf5>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=cpp_testhdf5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
      )
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_VFD_TEST)
  
  # Run test with different Virtual File Driver
  FOREACH (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  ENDFOREACH (vfd ${VFD_LIST})

ENDIF (HDF5_TEST_VFD)
