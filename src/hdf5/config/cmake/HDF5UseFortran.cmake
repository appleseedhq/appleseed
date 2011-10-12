#
# This file provides functions for Fortran support.
#
#-------------------------------------------------------------------------------
ENABLE_LANGUAGE (Fortran)
  
#-----------------------------------------------------------------------------
# Detect name mangling convention used between Fortran and C
#-----------------------------------------------------------------------------
INCLUDE (FortranCInterface)
FortranCInterface_HEADER (
    ${CMAKE_BINARY_DIR}/FCMangle.h
    MACRO_NAMESPACE "H5_FC_"
    SYMBOL_NAMESPACE "H5_FC_"
    SYMBOLS mysub mymod:my_sub
)

FILE (STRINGS ${CMAKE_BINARY_DIR}/FCMangle.h CONTENTS REGEX "H5_FC_GLOBAL\\(.*,.*\\) +(.*)")
STRING (REGEX MATCH "H5_FC_GLOBAL\\(.*,.*\\) +(.*)" RESULT ${CONTENTS})
SET (H5_FC_FUNC "H5_FC_FUNC(name,NAME) ${CMAKE_MATCH_1}")

FILE (STRINGS ${CMAKE_BINARY_DIR}/FCMangle.h CONTENTS REGEX "H5_FC_GLOBAL_\\(.*,.*\\) +(.*)")
STRING (REGEX MATCH "H5_FC_GLOBAL_\\(.*,.*\\) +(.*)" RESULT ${CONTENTS})
SET (H5_FC_FUNC_ "H5_FC_FUNC_(name,NAME) ${CMAKE_MATCH_1}")

#-----------------------------------------------------------------------------
# The provided CMake Fortran macros don't provide a general check function
# so this one is used for a sizeof test.
#-----------------------------------------------------------------------------
MACRO (CHECK_FORTRAN_FEATURE FUNCTION CODE VARIABLE)
  IF (NOT DEFINED ${VARIABLE})
    MESSAGE (STATUS "Testing Fortran ${FUNCTION}")
    IF (CMAKE_REQUIRED_LIBRARIES)
      SET (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES
          "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    ELSE (CMAKE_REQUIRED_LIBRARIES)
      SET (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES)
    ENDIF (CMAKE_REQUIRED_LIBRARIES)
    FILE (WRITE
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler.f
        "${CODE}"
    )
    TRY_COMPILE (${VARIABLE}
        ${CMAKE_BINARY_DIR}
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler.f
        CMAKE_FLAGS "${CHECK_FUNCTION_EXISTS_ADD_LIBRARIES}"
        OUTPUT_VARIABLE OUTPUT
    )

#    MESSAGE ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
#    MESSAGE ( "Test result ${OUTPUT}")
#    MESSAGE ( "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")

    IF (${VARIABLE})
      SET (${VARIABLE} 1 CACHE INTERNAL "Have Fortran function ${FUNCTION}")
      MESSAGE (STATUS "Testing Fortran ${FUNCTION} - OK")
      FILE (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
          "Determining if the Fortran ${FUNCTION} exists passed with the following output:\n"
          "${OUTPUT}\n\n"
      )
    ELSE (${VARIABLE})
      MESSAGE (STATUS "Testing Fortran ${FUNCTION} - Fail")
      SET (${VARIABLE} "" CACHE INTERNAL "Have Fortran function ${FUNCTION}")
      FILE (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
          "Determining if the Fortran ${FUNCTION} exists failed with the following output:\n"
          "${OUTPUT}\n\n")
    ENDIF (${VARIABLE})
  ENDIF (NOT DEFINED ${VARIABLE})
ENDMACRO (CHECK_FORTRAN_FEATURE)

#-----------------------------------------------------------------------------
# Configure Checks which require Fortran compilation must go in here
# not in the main ConfigureChecks.cmake files, because if the user has
# no Fortran compiler, problems arise.
#
# Be careful with leading spaces here, do not remove them.
#-----------------------------------------------------------------------------
CHECK_FORTRAN_FEATURE(sizeof
  "
       PROGRAM main
       i = sizeof(x)
       END PROGRAM
  "
  FORTRAN_HAVE_SIZEOF
)

CHECK_FORTRAN_FEATURE(RealIsNotDouble
  "
       MODULE type_mod
         INTERFACE h5t
           MODULE PROCEDURE h5t_real
           MODULE PROCEDURE h5t_dble
         END INTERFACE
       CONTAINS
         SUBROUTINE h5t_real(r)
           REAL :: r
         END SUBROUTINE h5t_real
         SUBROUTINE h5t_dble(d)
           DOUBLE PRECISION :: d
         END SUBROUTINE h5t_dble
       END MODULE type_mod
       PROGRAM main
         USE type_mod
         REAL :: r
         DOUBLE PRECISION :: d
         CALL h5t(r)
         CALL h5t(d)
       END PROGRAM main
  "
  FORTRAN_DEFAULT_REAL_NOT_DOUBLE
)

#-----------------------------------------------------------------------------
# Add debug information (intel Fortran : JB)
#-----------------------------------------------------------------------------
IF (CMAKE_Fortran_COMPILER MATCHES ifort)
    IF (WIN32)
        SET (CMAKE_Fortran_FLAGS_DEBUG "/debug:full /dbglibs " CACHE "flags" STRING FORCE)
        SET (CMAKE_EXE_LINKER_FLAGS_DEBUG "/DEBUG" CACHE "flags" STRING FORCE)
    ENDIF (WIN32)
ENDIF (CMAKE_Fortran_COMPILER MATCHES ifort)
