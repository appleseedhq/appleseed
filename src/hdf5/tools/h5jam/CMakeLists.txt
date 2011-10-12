cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5JAM)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5jam executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5jam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jam.c)
TARGET_NAMING (h5jam ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5jam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_EXECUTABLE (getub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/getub.c)
TARGET_NAMING (getub ${LIB_TYPE})
TARGET_LINK_LIBRARIES (getub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_EXECUTABLE (tellub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/tellub.c)
TARGET_NAMING (tellub ${LIB_TYPE})
TARGET_LINK_LIBRARIES (tellub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_EXECUTABLE (h5unjam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5unjam.c)
TARGET_NAMING (h5unjam ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5unjam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES
    h5jam
    getub
    tellub
    h5unjam
)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5jam test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5jamgentest ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jamgentest.c)
    TARGET_NAMING (h5jamgentest ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (h5jamgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    
    #ADD_TEST (NAME h5jamgentest COMMAND $<TARGET_FILE:h5jamgentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

  SET (HDF5_REFERENCE_TXT_FILES
      u10.txt
      u511.txt
      u512.txt
      u513.txt
  )
  SET (HDF5_REFERENCE_TEST_FILES
      tall.h5
      twithub.h5
      twithub513.h5
  )

  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5jam
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

  FOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${txt_file}")
    #MESSAGE (STATUS " Copying ${txt_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5jam
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/testfiles/${txt_file} ${dest}
    )
  ENDFOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})
    
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (CLEANUP testname)
    ADD_TEST (
        NAME H5JAM-CLEANUP-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ${ARGN}
    )
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5JAM-CLEANUP-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5JAM-CLEANUP-${testname}-clear-objects")
  ENDMACRO(CLEANUP testname testfile)

  MACRO (CHECKFILE testname expected actual)
    # If using memchecker add tests without using scripts
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-CHECKFILE-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${expected}.new
              ${expected}.new.err
              ${actual}.out
              ${actual}.out.err
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5JAM-CHECKFILE-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      ADD_TEST (
          NAME H5JAM-CHECKFILE-H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=testfiles/${expected}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${expected}.new"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-CHECKFILE-H5DUMP-${testname} PROPERTIES DEPENDS H5JAM-CHECKFILE-${testname}-clear-objects)
      ADD_TEST (
          NAME H5JAM-CHECKFILE-H5DUMP_CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${actual}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${actual}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_REFERENCE=${expected}.new"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-CHECKFILE-H5DUMP_CMP-${testname} PROPERTIES DEPENDS H5JAM-CHECKFILE-H5DUMP-${testname})
      SET (last_test "H5JAM-CHECKFILE-H5DUMP_CMP-${testname}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO(CHECKFILE testname expected actual)

  MACRO (SETUP testname infile outfile)
    ADD_TEST (
        NAME H5JAM-SETUP-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ${outfile}
    )
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5JAM-SETUP-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    ADD_TEST (
        NAME H5JAM-SETUP-${testname}
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/testfiles/${infile} ${PROJECT_BINARY_DIR}/${outfile}
    )
    SET_TESTS_PROPERTIES (H5JAM-SETUP-${testname} PROPERTIES DEPENDS H5JAM-SETUP-${testname}-clear-objects)
    SET (last_test "H5JAM-SETUP-${testname}")
  ENDMACRO(SETUP testname infile outfile)

  MACRO (CHECK_UB_1 testname testfile ufile)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-CHECK_UB_1-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testfile}.len.txt
              ${testfile}.cmp
              ${ufile}.cmp
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5JAM-CHECK_UB_1-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      ADD_TEST (
          NAME H5JAM-CHECK_UB_1-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:tellub>"
              -D "TEST_GET_PROGRAM=$<TARGET_FILE:getub>"
              -D "TEST_CHECKUB=YES"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_HFILE=${testfile}"
              -D "TEST_UFILE=${ufile}"
              -D "TEST_EXPECT=0"
              -D "TEST_OFILE=${ARGN}"
              -P "${HDF5_RESOURCES_DIR}/userblockTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-CHECK_UB_1-${testname} PROPERTIES DEPENDS H5JAM-CHECK_UB_1-${testname}-clear-objects)
      SET (last_test "H5JAM-CHECK_UB_1-${testname}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO(CHECK_UB_1 testname testfile ufile)

  MACRO (CHECK_NOUB testname testfile)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-CHECK_NOUB-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:tellub>"
              -D "TEST_GET_PROGRAM=$<TARGET_FILE:getub>"
              -D "TEST_CHECKUB=NO"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_HFILE=${testfile}"
              -D "TEST_EXPECT=0"
              -D "TEST_UFILE=NULL"
              -D "TEST_OFILE=NULL"
              -P "${HDF5_RESOURCES_DIR}/userblockTest.cmake"
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5JAM-CHECK_NOUB-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      SET (last_test "H5JAM-CHECK_NOUB-${testname}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO(CHECK_NOUB testname testfile)

  MACRO (UNJAMTEST testname infile ufile outfile)
    ADD_TEST (
        NAME H5JAM-UNJAM-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ${outfile}
    )
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5JAM-UNJAM-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    IF (NOT "${ufile}" STREQUAL "NONE")
      ADD_TEST (
          NAME H5JAM-UNJAM_D-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${ufile}
      )
      SET_TESTS_PROPERTIES (H5JAM-UNJAM_D-${testname}-clear-objects PROPERTIES DEPENDS H5JAM-UNJAM-${testname}-clear-objects)
      ADD_TEST (NAME H5JAM-UNJAM-${testname} COMMAND $<TARGET_FILE:h5unjam> -i ${infile} -u ${ufile} -o ${outfile})
      SET_TESTS_PROPERTIES (H5JAM-UNJAM-${testname} PROPERTIES DEPENDS H5JAM-UNJAM_D-${testname}-clear-objects)
      SET (compare_test ${ufile})
    ELSE (NOT "${ufile}" STREQUAL "NONE")
      IF (NOT "${ARGN}" STREQUAL "--delete")
        ADD_TEST (
            NAME H5JAM-UNJAM_D-${testname}-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove ${outfile}.ufile.txt ${outfile}.ufile.txt.err
        )
        SET_TESTS_PROPERTIES (H5JAM-UNJAM_D-${testname}-clear-objects PROPERTIES DEPENDS H5JAM-UNJAM-${testname}-clear-objects)
        IF (HDF5_ENABLE_USING_MEMCHECKER)
          ADD_TEST (NAME H5JAM-UNJAM-${testname} COMMAND $<TARGET_FILE:h5unjam> -i ${infile} -o ${outfile})
        ELSE (HDF5_ENABLE_USING_MEMCHECKER)
          ADD_TEST (
              NAME H5JAM-UNJAM-${testname}
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5unjam>"
                  -D "TEST_ARGS:STRING=-i;${infile};-o;${outfile}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                  -D "TEST_OUTPUT=${outfile}.ufile.txt"
                  -D "TEST_EXPECT=0"
                  -D "TEST_SKIP_COMPARE=TRUE"
                  -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
          )
        ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
        SET_TESTS_PROPERTIES (H5JAM-UNJAM-${testname} PROPERTIES DEPENDS H5JAM-UNJAM_D-${testname}-clear-objects)
        SET (compare_test "${outfile}.ufile.txt")
      ELSE (NOT "${ARGN}" STREQUAL "--delete")
        ADD_TEST (NAME H5JAM-UNJAM-${testname} COMMAND $<TARGET_FILE:h5unjam> -i ${infile} -o ${outfile})
        SET_TESTS_PROPERTIES (H5JAM-UNJAM-${testname} PROPERTIES DEPENDS H5JAM-UNJAM-${testname}-clear-objects)
        SET (compare_test "")
      ENDIF (NOT "${ARGN}" STREQUAL "--delete")
    ENDIF (NOT "${ufile}" STREQUAL "NONE")
    SET (last_test "H5JAM-UNJAM-${testname}")
    IF (NOT "${compare_test}" STREQUAL "")
      CHECK_UB_1 (${testname} ${infile} ${compare_test})
    ENDIF (NOT "${compare_test}" STREQUAL "")
    CHECK_NOUB (${testname} ${outfile})
  ENDMACRO(UNJAMTEST testname infile ufile outfile)

  MACRO (JAMTEST testname jamfile infile outfile)
    ADD_TEST (
        NAME H5JAM-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ${outfile} ${infile}.cpy.h5
    )
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5JAM-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5JAM-${testname}-clear-objects")
    IF ("${outfile}" STREQUAL "NONE")
      ADD_TEST (
          NAME H5JAM-NONE_COPY-${testname}
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${infile} ${infile}.cpy.h5
      )
      SET_TESTS_PROPERTIES (H5JAM-NONE_COPY-${testname} PROPERTIES DEPENDS ${last_test})
      SET (last_test "H5JAM-NONE_COPY-${testname}")
      ADD_TEST (NAME H5JAM-${testname} COMMAND $<TARGET_FILE:h5jam> -u testfiles/${jamfile} -i ${infile} ${ARGN})
      SET (compare_test ${infile})
      SET (compare_orig ${infile}.cpy.h5)
    ELSE ("${outfile}" STREQUAL "NONE")
      ADD_TEST (NAME H5JAM-${testname} COMMAND $<TARGET_FILE:h5jam> -u testfiles/${jamfile} -i testfiles/${infile} -o ${outfile} ${ARGN})
      SET (compare_test ${outfile})
      SET (compare_orig testfiles/${infile})
    ENDIF ("${outfile}" STREQUAL "NONE")
    SET_TESTS_PROPERTIES (H5JAM-${testname} PROPERTIES DEPENDS ${last_test})
    SET (last_test "H5JAM-${testname}")
    IF ("${ARGN}" STREQUAL "--clobber")
      SET (compare_orig "")
    ENDIF ("${ARGN}" STREQUAL "--clobber")
    CHECK_UB_1 (${testname} ${compare_test} testfiles/${jamfile} ${compare_orig})
  ENDMACRO (JAMTEST testname jamfile infile outfile)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  JAMTEST (tall_u10 u10.txt tall.h5 ta2.h5)
  CHECKFILE (tall_u10 tall.h5 ta2.h5)
#  CLEANUP (tall_u10 ta2.h5)
  JAMTEST (tall_u511 u511.txt tall.h5 ta3.h5)
  CHECKFILE (tall_u511 tall.h5 ta3.h5)
#  CLEANUP (tall_u511 ta3.h5)
  JAMTEST (tall_u512 u512.txt tall.h5 ta4.h5)
  CHECKFILE (tall_u512 tall.h5 ta4.h5)
#  CLEANUP (tall_u512 ta4.h5)
  JAMTEST (tall_u513 u513.txt tall.h5 ta5.h5)
  CHECKFILE (tall_u513 tall.h5 ta5.h5)
#  CLEANUP (tall_u513 ta5.h5)

  SETUP (ta_u10 tall.h5 ta.h5)
  JAMTEST (ta_u10 u10.txt ta.h5 NONE)
  CHECKFILE (ta_u10 tall.h5 ta.h5)
  SETUP (ta_u511 tall.h5 ta.h5 NOCLEAN)
  JAMTEST (ta_u511 u511.txt ta.h5 NONE)
  CHECKFILE (ta_u511 tall.h5 ta.h5)
  SETUP (ta_u512 tall.h5 ta.h5 NOCLEAN)
  JAMTEST (ta_u512 u512.txt ta.h5 NONE)
  CHECKFILE (ta_u512 tall.h5 ta.h5)
  SETUP (ta_u513 tall.h5 ta.h5 NOCLEAN)
  JAMTEST (ta_u513 u513.txt ta.h5 NONE)
  CHECKFILE (ta_u513 tall.h5 ta.h5)
#  CLEANUP (ta ta.h5)

  JAMTEST (twithub_u10 u10.txt twithub.h5 tax2.h5)
  CHECKFILE (twithub_u10 tall.h5 tax2.h5)
#  CLEANUP (twithub_u10 tax2.h5)
  JAMTEST (twithub_u511 u511.txt twithub.h5 tax3.h5)
  CHECKFILE (twithub_u511 tall.h5 tax3.h5)
#  CLEANUP (twithub_u511 tax3.h5)
  JAMTEST (twithub_u512 u512.txt twithub.h5 tax4.h5)
  CHECKFILE (twithub_u512 tall.h5 tax4.h5)
#  CLEANUP (twithub_u512 tax4.h5)
  JAMTEST (twithub_u513 u513.txt twithub.h5 tax5.h5)
  CHECKFILE (twithub_u513 tall.h5 tax5.h5)
#  CLEANUP (twithub_u513 tax5.h5)

  JAMTEST (twithub513_u10 u10.txt twithub513.h5 tax6.h5)
  CHECKFILE (twithub513_u10 tall.h5 tax6.h5)
#  CLEANUP (twithub513_u10 tax6.h5)
  JAMTEST (twithub513_u511 u511.txt twithub513.h5 tax7.h5)
  CHECKFILE (twithub513_u511 tall.h5 tax7.h5)
#  CLEANUP (twithub513_u511 tax7.h5)
  JAMTEST (twithub513_u512 u512.txt twithub513.h5 tax8.h5)
  CHECKFILE (twithub513_u512 tall.h5 tax8.h5)
#  CLEANUP (twithub513_u512 tax8.h5)
  JAMTEST (twithub513_u513 u513.txt twithub513.h5 tax9.h5)
  CHECKFILE (twithub513_u513 tall.h5 tax9.h5)
#  CLEANUP (twithub513_u513 tax9.h5)

  JAMTEST (twithub_u10_c u10.txt twithub.h5 taz2.h5 --clobber)
  CHECKFILE (twithub_u10_c tall.h5 taz2.h5)
#  CLEANUP (twithub_u10_c taz2.h5)
  JAMTEST (twithub_u511_c u511.txt twithub.h5 taz3.h5 --clobber)
  CHECKFILE (twithub_u511_c tall.h5 taz3.h5)
#  CLEANUP (twithub_u511_c taz3.h5)
  JAMTEST (twithub_u512_c u512.txt twithub.h5 taz4.h5 --clobber)
  CHECKFILE (twithub_u512_c tall.h5 taz4.h5)
#  CLEANUP (twithub_u512_c taz4.h5)
  JAMTEST (twithub_u513_c u513.txt twithub.h5 taz5.h5 --clobber)
  CHECKFILE (twithub_u513_c tall.h5 taz5.h5)
#  CLEANUP (twithub_u513_c taz5.h5)

  JAMTEST (twithub513_u10_c u10.txt twithub513.h5 taz6.h5 --clobber)
  CHECKFILE (twithub513_u10_c tall.h5 taz6.h5)
#  CLEANUP (twithub513_u10_c taz6.h5)
  JAMTEST (twithub513_u511_c u511.txt twithub513.h5 taz7.h5 --clobber)
  CHECKFILE (twithub513_u511_c tall.h5 taz7.h5)
#  CLEANUP (twithub513_u511_c taz7.h5)
  JAMTEST (twithub513_u512_c u512.txt twithub513.h5 taz8.h5 --clobber)
  CHECKFILE (twithub513_u512_c tall.h5 taz8.h5)
#  CLEANUP (twithub513_u512_c taz8.h5)
  JAMTEST (twithub513_u513_c u513.txt twithub513.h5 taz9.h5 --clobber)
  CHECKFILE (twithub513_u513_c tall.h5 taz9.h5)
#  CLEANUP (twithub513_u513_c taz9.h5)

  SETUP (N_twithub_u10_c twithub.h5 tay2.h5)
  JAMTEST (N_twithub_u10_c u10.txt tay2.h5 NONE --clobber)
  CHECKFILE (N_twithub_u10_c tall.h5 tay2.h5)
#  CLEANUP (N_twithub_u10_c tay2.h5)
  SETUP (N_twithub_u511_c twithub.h5 tay3.h5)
  JAMTEST (N_twithub_u511_c u511.txt tay3.h5 NONE --clobber)
  CHECKFILE (N_twithub_u511_c tall.h5 tay3.h5)
#  CLEANUP (N_twithub_u511_c tay3.h5)
  SETUP (N_twithub_u512_c twithub.h5 tay4.h5)
  JAMTEST (N_twithub_u512_c u512.txt tay4.h5 NONE --clobber)
  CHECKFILE (N_twithub_u512_c tall.h5 tay4.h5)
#  CLEANUP (N_twithub_u512_c tay4.h5)
  SETUP (N_twithub_u513_c twithub.h5 tay5.h5)
  JAMTEST (N_twithub_u513_c u513.txt tay5.h5 NONE --clobber)
  CHECKFILE (N_twithub_u513_c tall.h5 tay5.h5)
#  CLEANUP (N_twithub_u513_c tay5.h5)

  SETUP (N_twithub513_u10_c twithub513.h5 tay6.h5)
  JAMTEST (N_twithub513_u10_c u10.txt tay6.h5 NONE --clobber)
  CHECKFILE (N_twithub513_u10_c tall.h5 tay6.h5)
#  CLEANUP (N_twithub513_u10_c tay6.h5)
  SETUP (N_twithub513_u511_c twithub513.h5 tay7.h5)
  JAMTEST (N_twithub513_u511_c u511.txt tay7.h5 NONE --clobber)
  CHECKFILE (N_twithub513_u511_c tall.h5 tay7.h5)
#  CLEANUP (N_twithub513_u511_c tay7.h5)
  SETUP (N_twithub513_u512_c twithub513.h5 tay8.h5)
  JAMTEST (N_twithub513_u512_c u512.txt tay8.h5 NONE --clobber)
  CHECKFILE (N_twithub513_u512_c tall.h5 tay8.h5)
#  CLEANUP (N_twithub513_u512_c tay8.h5)
  SETUP (N_twithub513_u513_c twithub513.h5 tay9.h5)
  JAMTEST (N_twithub513_u513_c u513.txt tay9.h5 NONE --clobber)
  CHECKFILE (N_twithub513_u513_c tall.h5 tay9.h5)
#  CLEANUP (N_twithub513_u513_c tay9.h5)

  SETUP (twithub_tall twithub.h5 tai1.h5)
  UNJAMTEST (twithub_tall tai1.h5 o10.txt taa1.h5)
  CHECKFILE (twithub_tall tall.h5 taa1.h5)
#  CLEANUP (twithub_tall taa1.h5 tai1.h5 o10.txt)
  SETUP (twithub513_tall twithub513.h5 tai2.h5)
  UNJAMTEST (twithub513_tall tai2.h5 o512.txt taa2.h5)
  CHECKFILE (twithub513_tall tall.h5 taa2.h5)
#  CLEANUP (twithub513_tall taa2.h5 tai2.h5 o512.txt)

  SETUP (N_twithub_tall twithub.h5 tai3.h5)
  UNJAMTEST (N_twithub_tall tai3.h5 NONE taa3.h5)
  CHECKFILE (N_twithub_tall tall.h5 taa3.h5)
#  CLEANUP (N_twithub_tall taa3.h5 tai3.h5)
  SETUP (N_twithub513_tall twithub513.h5 tai4.h5)
  UNJAMTEST (N_twithub513_tall tai4.h5 NONE taa4.h5)
  CHECKFILE (N_twithub513_tall tall.h5 taa4.h5)
#  CLEANUP (N_twithub513_tall taa4.h5 tai4.h5)

  SETUP (D_twithub_tall twithub.h5 taj2.h5)
  UNJAMTEST (D_twithub_tall taj2.h5 NONE tac2.h5 --delete)
  CHECKFILE (D_twithub_tall tall.h5 tac2.h5)
#  CLEANUP (D_twithub_tall tac2.h5 taj2.h5)
  SETUP (D_twithub513_tall twithub513.h5 taj3.h5)
  UNJAMTEST (D_twithub513_tall taj3.h5 NONE tac3.h5 --delete)
  CHECKFILE (D_twithub513_tall tall.h5 tac3.h5)
#  CLEANUP (D_twithub513_tall tac3.h5 taj3.h5)

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
        h5jam h5unjam
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
