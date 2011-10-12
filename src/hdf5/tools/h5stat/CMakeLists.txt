cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5STAT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5stat executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5stat ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat.c)
TARGET_NAMING (h5stat ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5stat  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5stat)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5stat test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5stat_gentest ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat_gentest.c)
    TARGET_NAMING (h5stat_gentest ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (h5stat_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    
    #ADD_TEST (NAME h5stat_gentest COMMAND $<TARGET_FILE:h5stat_gentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_FILES
      h5stat_help1.ddl
      h5stat_help2.ddl
      h5stat_filters.ddl
      h5stat_filters-file.ddl
      h5stat_filters-F.ddl
      h5stat_filters-d.ddl
      h5stat_filters-g.ddl
      h5stat_filters-dT.ddl
      h5stat_filters-UD.ddl
      h5stat_filters-UT.ddl
      h5stat_tsohm.ddl
      h5stat_newgrat.ddl
      h5stat_newgrat-UG.ddl
      h5stat_newgrat-UA.ddl
  )
  SET (HDF5_REFERENCE_TEST_FILES
      h5stat_filters.h5
      h5stat_tsohm.h5
      h5stat_newgrat.h5
  )

  FOREACH (ddl_file ${HDF5_REFERENCE_FILES})
    SET (ddldest "${PROJECT_BINARY_DIR}/${ddl_file}")
    #MESSAGE (STATUS " Translating ${ddl_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5stat
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/testfiles/${ddl_file} ${ddldest} -l3
    )
  ENDFOREACH (ddl_file ${HDF5_REFERENCE_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5stat
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5STAT-${resultfile} COMMAND $<TARGET_FILE:h5stat> ${ARGN})
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5STAT-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5STAT-${resultfile} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5STAT-${resultfile}")
  ENDMACRO (ADD_H5_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5STAT-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          h5stat_help1.out
          h5stat_help1.out.err
          h5stat_help2.out
          h5stat_help2.out.err
          h5stat_filters.out
          h5stat_filters.out.err
          h5stat_filters-file.out
          h5stat_filters-file.out.err
          h5stat_filters-F.out
          h5stat_filters-F.out.err
          h5stat_filters-d.out
          h5stat_filters-d.out.err
          h5stat_filters-g.out
          h5stat_filters-g.out.err
          h5stat_filters-dT.out
          h5stat_filters-dT.out.err
          h5stat_filters-UD.out
          h5stat_filters-UD.out.err
          h5stat_filters-UT.out
          h5stat_filters-UT.out.err
          h5stat_tsohm.out
          h5stat_tsohm.out.err
          h5stat_newgrat.out
          h5stat_newgrat.out.err
          h5stat_newgrat-UG.out
          h5stat_newgrat-UG.out.err
          h5stat_newgrat-UA.out
          h5stat_newgrat-UA.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5STAT-clearall-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5STAT-clearall-objects")

  # If using memchecker add tests without using scripts
  IF (HDF5_ENABLE_USING_MEMCHECKER)
    ADD_TEST (NAME H5STAT-help COMMAND h5stat -h)
    ADD_TEST (NAME H5STAT-filters COMMAND h5stat h5stat_filters.h5)
  ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

# Test for help flag
  ADD_H5_TEST (h5stat_help1 0 -h)
  ADD_H5_TEST (h5stat_help2 0 --help)

# Test file with groups, compressed datasets, user-applied fileters, etc.
# h5stat_filters.h5 is a copy of ../../testfiles/tfilters.h5 as of release 1.8.0-alpha4
  ADD_H5_TEST (h5stat_filters 0 h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-file 0 -f h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-F 0 -F h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-d 0 -d h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-g 0 -g h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-dT 0 -dT h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-UD 0 -D h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-UT 0 -T h5stat_filters.h5)
# h5stat_tsohm.h5 is a copy of ../../../test/tsohm.h5 generated by tsohm.c 
# as of release 1.8.0-alpha4
  ADD_H5_TEST (h5stat_tsohm 0 h5stat_tsohm.h5)
# h5stat_newgrat.h5 is generated by h5stat_gentest.c
  ADD_H5_TEST (h5stat_newgrat 0 h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UG 0 -G h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UA 0 -A h5stat_newgrat.h5)
ENDIF (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5stat
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
