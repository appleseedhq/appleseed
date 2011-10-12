cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_CPP_EXAMPLES)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_HL_CPP_SRC_DIR}/src)

# --------------------------------------------------------------------
#  Add in the examples for the Packet Table codes
# --------------------------------------------------------------------
ADD_EXECUTABLE (ptExampleFL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleFL.cpp)
TARGET_NAMING (ptExampleFL ${LIB_TYPE})
TARGET_LINK_LIBRARIES (
    ptExampleFL
    ${HDF5_HL_CPP_LIB_TARGET}
    ${HDF5_HL_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)

ADD_EXECUTABLE (ptExampleVL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleVL.cpp)
TARGET_NAMING (ptExampleVL ${LIB_TYPE})
TARGET_LINK_LIBRARIES (
    ptExampleVL
    ${HDF5_HL_CPP_LIB_TARGET}
    ${HDF5_HL_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)

IF (BUILD_TESTING)
  ADD_TEST (NAME cpp_hl_ex_ptExampleFL COMMAND $<TARGET_FILE:ptExampleFL>)
  ADD_TEST (NAME cpp_hl_ex_ptExampleVL COMMAND $<TARGET_FILE:ptExampleVL>)
ENDIF (BUILD_TESTING)
