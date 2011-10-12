cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_MISC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the misc executables
# --------------------------------------------------------------------
#-- Misc Executables
ADD_EXECUTABLE (h5debug ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5debug.c)
TARGET_NAMING (h5debug ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5debug ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_EXECUTABLE (h5repart ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart.c)
TARGET_NAMING (h5repart ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5repart ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_EXECUTABLE (h5mkgrp ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5mkgrp.c)
TARGET_NAMING (h5mkgrp ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5mkgrp  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES
    h5debug
    h5repart
    h5mkgrp
)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the misc test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5repart_gentest ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart_gentest.c)
    TARGET_NAMING (h5repart_gentest ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (h5repart_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    #ADD_TEST (NAME h5repart_gentest COMMAND $<TARGET_FILE:h5repart_gentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

  ADD_EXECUTABLE (h5repart_test ${HDF5_TOOLS_MISC_SOURCE_DIR}/repart_test.c)
  TARGET_NAMING (h5repart_test ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (h5repart_test ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_TEST_FILES
      family_file00000.h5
      family_file00001.h5
      family_file00002.h5
      family_file00003.h5
      family_file00004.h5
      family_file00005.h5
      family_file00006.h5
      family_file00007.h5
      family_file00008.h5
      family_file00009.h5
      family_file00010.h5
      family_file00011.h5
      family_file00012.h5
      family_file00013.h5
      family_file00014.h5
      family_file00015.h5
      family_file00016.h5
      family_file00017.h5
  )

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5repart
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
  SET (HDF5_MKGRP_TEST_FILES
      #h5mkgrp_help
      #h5mkgrp_version
      h5mkgrp_single
      h5mkgrp_single_latest
      h5mkgrp_several
      h5mkgrp_several_latest
      h5mkgrp_nested
      h5mkgrp_nested_latest
      h5mkgrp_nested_mult
      h5mkgrp_nested_mult_latest
  )

  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/../testfiles")
  FOREACH (h5_mkgrp_file ${HDF5_MKGRP_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_mkgrp_file}")
    #MESSAGE (STATUS " Copying ${h5_mkgrp_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5mkgrp
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_mkgrp_file}.ls ${dest}.ls -l3
    )
  ENDFOREACH (h5_mkgrp_file ${HDF5_MKGRP_TEST_FILES})

  ADD_CUSTOM_COMMAND (
      TARGET     h5mkgrp
      POST_BUILD
      COMMAND    ${XLATE_UTILITY}
      ARGS       ${PROJECT_SOURCE_DIR}/testfiles/h5mkgrp_help.txt ${PROJECT_BINARY_DIR}/h5mkgrp_help.txt -l0
  )
  CONFIGURE_FILE (${PROJECT_SOURCE_DIR}/testfiles/h5mkgrp_version.txt.in ${PROJECT_BINARY_DIR}/h5mkgrp_version.txt @ONLY)

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST resultfile resultcode resultoption)
    IF (NOT ${resultoption} STREQUAL " ")
      ADD_TEST (
          NAME H5MKGRP-clear-${resultfile}${resultoption}
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
                  ${PROJECT_BINARY_DIR}/../testfiles/${resultfile}.h5 
                  ${PROJECT_BINARY_DIR}/${resultfile}.out
                  ${PROJECT_BINARY_DIR}/${resultfile}.out.err
      )
      ADD_TEST (
          NAME H5MKGRP-${resultfile}${resultoption}
          COMMAND $<TARGET_FILE:h5mkgrp> ${resultoption} ${PROJECT_BINARY_DIR}/../testfiles/${resultfile}.h5 ${ARGN}
      )
      SET_TESTS_PROPERTIES (H5MKGRP-${resultfile}${resultoption} PROPERTIES DEPENDS H5MKGRP-clear-${resultfile}${resultoption})
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5MKGRP-H5LS-${resultfile}${resultoption}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
                -D "TEST_ARGS:STRING=-v;-r;../testfiles/${resultfile}.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${resultfile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_MASK_MOD=true"
                -D "TEST_REFERENCE=${resultfile}.ls"
                -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
        )
        SET_TESTS_PROPERTIES (H5MKGRP-H5LS-${resultfile}${resultoption} PROPERTIES DEPENDS H5MKGRP-${resultfile}${resultoption})
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (NOT ${resultoption} STREQUAL " ")
      ADD_TEST (
          NAME H5MKGRP-clear-${resultfile}
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
                  ${PROJECT_BINARY_DIR}/../testfiles/${resultfile}.h5 
                  ${PROJECT_BINARY_DIR}/${resultfile}.out
                  ${PROJECT_BINARY_DIR}/${resultfile}.out.err
      )
      ADD_TEST (
          NAME H5MKGRP-${resultfile}
          COMMAND $<TARGET_FILE:h5mkgrp> ${PROJECT_BINARY_DIR}/../testfiles/${resultfile}.h5 ${ARGN}
      )
      SET_TESTS_PROPERTIES (H5MKGRP-${resultfile} PROPERTIES DEPENDS H5MKGRP-clear-${resultfile})
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5MKGRP-H5LS-${resultfile}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
                -D "TEST_ARGS:STRING=-v;-r;../testfiles/${resultfile}.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${resultfile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_MASK_MOD=true"
                -D "TEST_REFERENCE=${resultfile}.ls"
                -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
        )
        SET_TESTS_PROPERTIES (H5MKGRP-H5LS-${resultfile} PROPERTIES DEPENDS H5MKGRP-${resultfile})
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ENDIF (NOT ${resultoption} STREQUAL " ")
  ENDMACRO (ADD_H5_TEST resultfile resultcode resultoption)

  MACRO (ADD_H5_CMP resultfile resultcode)
    ADD_TEST (
        NAME H5MKGRP_CMP-clear-${resultfile}
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
                ${PROJECT_BINARY_DIR}/../testfiles/${resultfile}.h5 
                ${PROJECT_BINARY_DIR}/${resultfile}.out
                ${PROJECT_BINARY_DIR}/${resultfile}.out.err
    )
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5MKGRP_CMP-${resultfile} COMMAND $<TARGET_FILE:h5mkgrp> ${ARGN})
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5MKGRP_CMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5mkgrp>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.txt"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    SET_TESTS_PROPERTIES (H5MKGRP_CMP-${resultfile} PROPERTIES DEPENDS H5MKGRP_CMP-clear-${resultfile})
  ENDMACRO (ADD_H5_CMP resultfile resultcode)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME h5repart_20K-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          fst_family00000.h5
  )
  # repartition family member size to 20,000 bytes.
  ADD_TEST (NAME h5repart_20K COMMAND $<TARGET_FILE:h5repart> -m 20000 family_file%05d.h5 fst_family%05d.h5)
  SET_TESTS_PROPERTIES (h5repart_20K PROPERTIES DEPENDS h5repart_20K-clear-objects)

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME h5repart_5K-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          scd_family00000.h5
          scd_family00001.h5
          scd_family00002.h5
          scd_family00003.h5
  )
  SET_TESTS_PROPERTIES (h5repart_5K-clear-objects PROPERTIES DEPENDS h5repart_20K)
  # repartition family member size to 5 KB.
  ADD_TEST (NAME h5repart_5K COMMAND $<TARGET_FILE:h5repart> -m 5k family_file%05d.h5 scd_family%05d.h5)
  SET_TESTS_PROPERTIES (h5repart_5K PROPERTIES DEPENDS h5repart_5K-clear-objects)

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME h5repart_sec2-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          family_to_sec2.h5
  )
  SET_TESTS_PROPERTIES (h5repart_sec2-clear-objects PROPERTIES DEPENDS h5repart_5K)
  # convert family file to sec2 file of 20,000 bytes
  ADD_TEST (NAME h5repart_sec2 COMMAND $<TARGET_FILE:h5repart> -m 20000 -family_to_sec2 family_file%05d.h5 family_to_sec2.h5)
  SET_TESTS_PROPERTIES (h5repart_sec2 PROPERTIES DEPENDS h5repart_sec2-clear-objects)

  # test the output files repartitioned above.
  ADD_TEST (NAME h5repart_test COMMAND $<TARGET_FILE:h5repart_test>)
  SET_TESTS_PROPERTIES (h5repart_test PROPERTIES DEPENDS h5repart_sec2)

  SET (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
        h5repart_test
  )

  # Check that help & version is displayed properly
  ADD_H5_CMP (h5mkgrp_help 0 "-h")
  ADD_H5_CMP (h5mkgrp_version 0 "-V")

  # Create single group at root level
  ADD_H5_TEST (h5mkgrp_single 0 " " single)
  ADD_H5_TEST (h5mkgrp_single 0 "-v" single)
  ADD_H5_TEST (h5mkgrp_single 0 "-p" single)
  ADD_H5_TEST (h5mkgrp_single_latest 0 "-l" latest)

  # Create several groups at root level
  ADD_H5_TEST (h5mkgrp_several 0 " " one two)
  ADD_H5_TEST (h5mkgrp_several 0 "-v" one two)
  ADD_H5_TEST (h5mkgrp_several 0 "-p" one two)
  ADD_H5_TEST (h5mkgrp_several_latest 0 "-l" one two)

  # Create various nested groups 
  ADD_H5_TEST (h5mkgrp_nested 0 "-p" /one/two)
  ADD_H5_TEST (h5mkgrp_nested_latest 0 "-lp" /one/two)
  ADD_H5_TEST (h5mkgrp_nested_mult 0 "-p" /one/two /three/four)
  ADD_H5_TEST (h5mkgrp_nested_mult_latest 0 "-lp" /one/two /three/four)
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
        h5debug h5repart h5mkgrp
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
