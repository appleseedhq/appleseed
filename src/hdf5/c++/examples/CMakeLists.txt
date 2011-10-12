cmake_minimum_required (VERSION 2.8)
# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "cpp_ex_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

PROJECT (HDF5_CPP_EXAMPLES)

#-----------------------------------------------------------------------------
# Define examples
#-----------------------------------------------------------------------------

SET (examples
    create
    readdata
    writedata
    compound
    extend_ds
    chunks
    h5group
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (cpp_ex_${example} ${HDF5_CPP_EXAMPLES_SOURCE_DIR}/${example}.cpp)
  TARGET_NAMING (cpp_ex_${example} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (cpp_ex_${example} ${HDF5_CPP_LIB_TARGET} ${HDF5_LIB_TARGET})
ENDFOREACH (example ${examples})

IF (BUILD_TESTING)
  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME cpp_ex-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          Group.h5
          SDS.h5
          SDScompound.h5
          SDSextendible.h5
          Select.h5
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (cpp_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "cpp_ex-clear-objects")

  FOREACH (example ${examples})
    ADD_TEST (NAME cpp_ex_${example} COMMAND $<TARGET_FILE:cpp_ex_${example}>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (cpp_ex_${example} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "cpp_ex_${example}")
  ENDFOREACH (example ${examples})
ENDIF (BUILD_TESTING)
