cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_F90_EXAMPLES C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${HDF5_F90_BINARY_DIR}
    ${HDF5_F90_SRC_DIR}/src
)

SET (examples
    exlite
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (hl_f90_ex_${example} ${HDF5_HL_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
  TARGET_NAMING (hl_f90_ex_${example} ${LIB_TYPE})
  TARGET_FORTRAN_WIN_PROPERTIES (hl_f90_ex_${example} "")
  TARGET_LINK_LIBRARIES (hl_f90_ex_${example} 
      ${HDF5_HL_F90_LIB_TARGET}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  SET_TARGET_PROPERTIES (hl_f90_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)

  IF (BUILD_TESTING)
    ADD_TEST (NAME hl_f90_ex_${example} COMMAND $<TARGET_FILE:hl_f90_ex_${example}>)
  ENDIF (BUILD_TESTING)

ENDFOREACH (example ${examples})
