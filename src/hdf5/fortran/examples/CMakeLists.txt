cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_F90_EXAMPLES C CXX Fortran)
# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "f90_ex_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
    dsetexample
    fileexample
    rwdsetexample
    attrexample
    groupexample
    grpsexample
    grpdsetexample
    hyperslab
    selectele
    grpit
    refobjexample
    refregexample
    mountexample
    compound
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (f90_ex_${example} ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
  TARGET_NAMING (f90_ex_${example} ${LIB_TYPE})
  TARGET_FORTRAN_WIN_PROPERTIES (f90_ex_${example} "")
  IF (WIN32 AND NOT CYGWIN)
    SET_PROPERTY (TARGET f90_ex_${example} 
        APPEND PROPERTY COMPILE_DEFINITIONS 
  	      HDF5F90_WINDOWS
    )
  ENDIF (WIN32 AND NOT CYGWIN)
  TARGET_LINK_LIBRARIES (f90_ex_${example}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  SET_TARGET_PROPERTIES (f90_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)

  IF (BUILD_TESTING)
    ADD_TEST (NAME f90_ex_${example} COMMAND $<TARGET_FILE:f90_ex_${example}>)
  ENDIF (BUILD_TESTING)

ENDFOREACH (example ${examples})

IF (H5_HAVE_PARALLEL)
  ADD_EXECUTABLE (f90_ex_ph5example ${HDF5_F90_EXAMPLES_SOURCE_DIR}/ph5example.f90)
  TARGET_NAMING (f90_ex_ph5example ${LIB_TYPE})
  TARGET_FORTRAN_WIN_PROPERTIES (f90_ex_ph5example "")
  IF (WIN32 AND NOT CYGWIN)
    SET_PROPERTY (TARGET f90_ex_ph5example 
        APPEND PROPERTY COMPILE_DEFINITIONS 
          HDF5F90_WINDOWS
    )
  ENDIF (WIN32 AND NOT CYGWIN)
  TARGET_LINK_LIBRARIES (f90_ex_ph5example
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  SET_TARGET_PROPERTIES (f90_ex_ph5example PROPERTIES LINKER_LANGUAGE Fortran)

  IF (BUILD_TESTING)
    ADD_TEST (NAME f90_ex_ph5example COMMAND $<TARGET_FILE:f90_ex_ph5example>)
  ENDIF (BUILD_TESTING)

ENDIF (H5_HAVE_PARALLEL)
