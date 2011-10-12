cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_EXAMPLES)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
    h5_write
    h5_read
    h5_extend_write
    h5_chunk_read
    h5_compound
    h5_group
    h5_select
    h5_attribute
    h5_mount
    h5_reference
    h5_drivers
    h5_ref2reg
    h5_extlink
    h5_elink_unix2win
    h5_shared_mesg
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (${example} ${HDF5_EXAMPLES_SOURCE_DIR}/${example}.c)
  TARGET_NAMING (${example} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (${example} ${HDF5_LIB_TARGET})
ENDFOREACH (example ${examples})

IF (H5_HAVE_PARALLEL)
  ADD_EXECUTABLE (ph5example ${HDF5_EXAMPLES_SOURCE_DIR}/ph5example.c)
  TARGET_NAMING (ph5example ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (ph5example ${HDF5_LIB_TARGET})
ENDIF (H5_HAVE_PARALLEL)

IF (BUILD_TESTING)
  FILE (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME EXAMPLES-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          Attributes.h5
          btrees_file.h5
          default_file.h5
          extlink_prefix_source.h5
          extlink_source.h5
          extlink_target.h5
          group.h5
          hard_link.h5
          mount1.h5
          mount2.h5
          one_index_file.h5
          only_dspaces_and_attrs_file.h5
          only_huge_mesgs_file.h5
          REF_REG.h5
          refere.h5
          SDS.h5
          SDScompound.h5
          SDSextendible.h5
          Select.h5
          separate_indexes_file.h5
          small_lists_file.h5
          soft_link.h5
          unix2win.h5
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (EXAMPLES-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "EXAMPLES-clear-objects")

  FOREACH (example ${examples})
    ADD_TEST (NAME EXAMPLES-${example} COMMAND $<TARGET_FILE:${example}>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (EXAMPLES-${example} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "EXAMPLES-${example}")
  ENDFOREACH (example ${examples})

  IF (H5_HAVE_PARALLEL)
    ADD_TEST (NAME EXAMPLES-ph5example COMMAND $<TARGET_FILE:ph5example>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (EXAMPLES-ph5example PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "EXAMPLES-ph5example")
  ENDIF (H5_HAVE_PARALLEL)
ENDIF (BUILD_TESTING)
