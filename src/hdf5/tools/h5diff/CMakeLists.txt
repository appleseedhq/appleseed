cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5DIFF)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5diff executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5diff
    ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_common.c
    ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_main.c
)
TARGET_NAMING (h5diff ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5diff  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5diff)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5diff and test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5diffgentest ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diffgentest.c)
    TARGET_NAMING (h5diffgentest ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (h5diffgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    
    #ADD_TEST (NAME h5diffgentest COMMAND $<TARGET_FILE:h5diffgentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_FILES
      h5diff_10.txt
      h5diff_100.txt
      #h5diff_101.txt
      #h5diff_102.txt
      #h5diff_103.txt
      #h5diff_104.txt
      h5diff_11.txt
      h5diff_12.txt
      h5diff_13.txt
      h5diff_14.txt
      h5diff_15.txt
      h5diff_16_1.txt
      h5diff_16_2.txt
      h5diff_16_3.txt
      h5diff_17.txt
      h5diff_171.txt
      h5diff_172.txt
      h5diff_18_1.txt
      h5diff_18.txt
      h5diff_20.txt
      h5diff_200.txt
      h5diff_201.txt
      h5diff_202.txt
      h5diff_203.txt
      h5diff_204.txt
      h5diff_205.txt
      h5diff_206.txt
      h5diff_207.txt
      h5diff_21.txt
      h5diff_22.txt
      h5diff_23.txt
      h5diff_24.txt
      h5diff_25.txt
      h5diff_26.txt
      h5diff_27.txt
      h5diff_28.txt
      h5diff_300.txt
      h5diff_400.txt
      h5diff_401.txt
      h5diff_402.txt
      h5diff_403.txt
      h5diff_404.txt
      h5diff_405.txt
      h5diff_406.txt
      h5diff_407.txt
      h5diff_408.txt
      h5diff_409.txt
      h5diff_410.txt
      h5diff_411.txt
      h5diff_412.txt
      h5diff_413.txt
      h5diff_414.txt
      h5diff_415.txt
      h5diff_416.txt
      h5diff_417.txt
      h5diff_418.txt
      h5diff_419.txt
      h5diff_420.txt
      h5diff_421.txt
      h5diff_422.txt
      h5diff_423.txt
      h5diff_424.txt
      h5diff_425.txt
      h5diff_450.txt
      h5diff_451.txt
      h5diff_452.txt
      h5diff_453.txt
      h5diff_454.txt
      h5diff_455.txt
      h5diff_456.txt
      h5diff_457.txt
      h5diff_458.txt
      h5diff_459.txt
      h5diff_480.txt
      h5diff_481.txt
      h5diff_482.txt
      h5diff_483.txt
      h5diff_484.txt
      h5diff_50.txt
      h5diff_51.txt
      h5diff_52.txt
      h5diff_53.txt
      h5diff_54.txt
      h5diff_55.txt
      h5diff_56.txt
      h5diff_57.txt
      h5diff_58.txt
      h5diff_500.txt
      h5diff_501.txt
      h5diff_502.txt
      h5diff_503.txt
      h5diff_504.txt
      h5diff_505.txt
      h5diff_506.txt
      h5diff_507.txt
      h5diff_508.txt
      h5diff_509.txt
      h5diff_510.txt
      h5diff_511.txt
      h5diff_512.txt
      h5diff_513.txt
      h5diff_514.txt
      h5diff_515.txt
      h5diff_516.txt
      h5diff_517.txt
      h5diff_518.txt
      h5diff_530.txt
      h5diff_600.txt
      h5diff_601.txt
      h5diff_603.txt
      h5diff_604.txt
      h5diff_605.txt
      h5diff_606.txt
      h5diff_607.txt
      h5diff_608.txt
      h5diff_609.txt
      h5diff_610.txt
      h5diff_612.txt
      h5diff_613.txt
      h5diff_614.txt
      h5diff_615.txt
      h5diff_616.txt
      h5diff_617.txt
      h5diff_618.txt
      h5diff_619.txt
      h5diff_621.txt
      h5diff_622.txt
      h5diff_623.txt
      h5diff_624.txt
      h5diff_625.txt
      h5diff_626.txt
      h5diff_627.txt
      h5diff_628.txt
      h5diff_629.txt
      h5diff_70.txt
      h5diff_700.txt
      h5diff_701.txt
      h5diff_702.txt
      h5diff_703.txt
      h5diff_704.txt
      h5diff_705.txt
      h5diff_706.txt
      h5diff_707.txt
      h5diff_708.txt
      h5diff_709.txt
      h5diff_710.txt
      h5diff_80.txt
      h5diff_90.txt
  )
  SET (HDF5_REFERENCE_TEST_FILES
      h5diff_basic1.h5
      h5diff_basic2.h5
      h5diff_types.h5
      h5diff_dtypes.h5
      h5diff_attr1.h5
      h5diff_attr2.h5
      h5diff_dset1.h5
      h5diff_dset2.h5
      h5diff_hyper1.h5
      h5diff_hyper2.h5
      h5diff_empty.h5
      h5diff_links.h5
      h5diff_softlinks.h5
      h5diff_linked_softlink.h5
      h5diff_extlink_src.h5
      h5diff_extlink_trg.h5
      h5diff_ext2softlink_src.h5
      h5diff_ext2softlink_trg.h5
      h5diff_danglelinks1.h5
      h5diff_danglelinks2.h5
      h5diff_grp_recurse1.h5
      h5diff_grp_recurse2.h5
      h5diff_grp_recurse_ext1.h5
      h5diff_grp_recurse_ext2-1.h5
      h5diff_grp_recurse_ext2-2.h5
      h5diff_grp_recurse_ext2-3.h5
      h5diff_exclude1-1.h5
      h5diff_exclude1-2.h5
      h5diff_exclude2-1.h5
      h5diff_exclude2-2.h5
      h5diff_comp_vl_strs.h5
      h5diff_attr_v_level1.h5
      h5diff_attr_v_level2.h5
  )

  FOREACH (txt_file ${HDF5_REFERENCE_FILES})
    SET (txtdest "${PROJECT_BINARY_DIR}/${txt_file}")
    #MESSAGE (STATUS " Copying ${txt_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/${txt_file} ${txtdest}
    )
  ENDFOREACH (txt_file ${HDF5_REFERENCE_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

  IF (WIN32 AND NOT CYGWIN)
    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_101w.txt ${PROJECT_BINARY_DIR}/h5diff_101.txt
    )

    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_102w.txt ${PROJECT_BINARY_DIR}/h5diff_102.txt
    )
    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_103w.txt ${PROJECT_BINARY_DIR}/h5diff_103.txt
    )

    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_104w.txt ${PROJECT_BINARY_DIR}/h5diff_104.txt
    )
  ELSE (WIN32 AND NOT CYGWIN)
    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_101.txt ${PROJECT_BINARY_DIR}/h5diff_101.txt
    )

    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_102.txt ${PROJECT_BINARY_DIR}/h5diff_102.txt
    )
    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_103.txt ${PROJECT_BINARY_DIR}/h5diff_103.txt
    )

    ADD_CUSTOM_COMMAND (
        TARGET     h5diff
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_104.txt ${PROJECT_BINARY_DIR}/h5diff_104.txt
    )
  ENDIF (WIN32 AND NOT CYGWIN)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DIFF-${resultfile} COMMAND $<TARGET_FILE:h5diff> ${ARGN})
      IF (NOT ${resultcode} STREQUAL "0")
        SET_TESTS_PROPERTIES (H5DIFF-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT ${resultcode} STREQUAL "0")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DIFF-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.txt"
              -D "TEST_APPEND=EXIT CODE:"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5DIFF-${resultfile} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5DIFF-${resultfile}")
  ENDMACRO (ADD_H5_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # test file names 
  # --------------------------------------------------------------------
  SET (FILE1 h5diff_basic1.h5)
  SET (FILE2 h5diff_basic2.h5)
  SET (FILE3 h5diff_types.h5)
  SET (FILE4 h5diff_dtypes.h5)
  SET (FILE5 h5diff_attr1.h5)
  SET (FILE6 h5diff_attr2.h5)
  SET (FILE7 h5diff_dset1.h5)
  SET (FILE8 h5diff_dset2.h5)
  SET (FILE9 h5diff_hyper1.h5)
  SET (FILE10 h5diff_hyper2.h5)
  SET (FILE11 h5diff_empty.h5)
  SET (FILE12 h5diff_links.h5)
  SET (FILE13 h5diff_softlinks.h5)
  SET (FILE14 h5diff_linked_softlink.h5)
  SET (FILE15 h5diff_extlink_src.h5)
  SET (FILE16 h5diff_extlink_trg.h5)
  SET (FILE17 h5diff_ext2softlink_src.h5)
  SET (FILE18 h5diff_ext2softlink_trg.h5)
  SET (DANGLE_LINK_FILE1 h5diff_danglelinks1.h5)
  SET (DANGLE_LINK_FILE2 h5diff_danglelinks2.h5)
  SET (GRP_RECURSE_FILE1 h5diff_grp_recurse1.h5)
  SET (GRP_RECURSE_FILE2 h5diff_grp_recurse2.h5)
  # group recursive - same structure via external links through files 
  SET (GRP_RECURSE1_EXT h5diff_grp_recurse_ext1.h5)
  SET (GRP_RECURSE2_EXT1 h5diff_grp_recurse_ext2-1.h5)
  SET (GRP_RECURSE2_EXT2 h5diff_grp_recurse_ext2-2.h5)
  SET (GRP_RECURSE2_EXT3 h5diff_grp_recurse_ext2-3.h5)
  # same structure, same obj name with different value
  SET (EXCLUDE_FILE1_1 h5diff_exclude1-1.h5)
  SET (EXCLUDE_FILE1_2 h5diff_exclude1-2.h5)
  # different structure and obj names
  SET (EXCLUDE_FILE2_1 h5diff_exclude2-1.h5)
  SET (EXCLUDE_FILE2_2 h5diff_exclude2-2.h5)
  # compound type with multiple vlen string types
  SET (COMP_VL_STRS_FILE h5diff_comp_vl_strs.h5)
  # attrs with verbose option level
  SET (ATTR_VERBOSE_LEVEL_FILE1 h5diff_attr_v_level1.h5)
  SET (ATTR_VERBOSE_LEVEL_FILE2 h5diff_attr_v_level2.h5)


  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5DIFF-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          h5diff_10.out
          h5diff_10.out.err
          h5diff_100.out
          h5diff_100.out.err
          h5diff_101.out
          h5diff_101.out.err
          h5diff_102.out
          h5diff_102.out.err
          h5diff_103.out
          h5diff_103.out.err
          h5diff_104.out
          h5diff_104.out.err
          h5diff_11.out
          h5diff_11.out.err
          h5diff_12.out
          h5diff_12.out.err
          h5diff_13.out
          h5diff_13.out.err
          h5diff_14.out
          h5diff_14.out.err
          h5diff_15.out
          h5diff_15.out.err
          h5diff_16_1.out
          h5diff_16_1.out.err
          h5diff_16_2.out
          h5diff_16_2.out.err
          h5diff_16_3.out
          h5diff_16_3.out.err
          h5diff_17.out
          h5diff_17.out.err
          h5diff_171.out
          h5diff_171.out.err
          h5diff_172.out
          h5diff_172.out.err
          h5diff_18_1.out
          h5diff_18_1.out.err
          h5diff_18.out
          h5diff_18.out.err
          h5diff_20.out
          h5diff_20.out.err
          h5diff_200.out
          h5diff_200.out.err
          h5diff_201.out
          h5diff_201.out.err
          h5diff_202.out
          h5diff_202.out.err
          h5diff_203.out
          h5diff_203.out.err
          h5diff_204.out
          h5diff_204.out.err
          h5diff_205.out
          h5diff_205.out.err
          h5diff_206.out
          h5diff_206.out.err
          h5diff_207.out
          h5diff_207.out.err
          h5diff_21.out
          h5diff_21.out.err
          h5diff_22.out
          h5diff_22.out.err
          h5diff_23.out
          h5diff_23.out.err
          h5diff_24.out
          h5diff_24.out.err
          h5diff_25.out
          h5diff_25.out.err
          h5diff_26.out
          h5diff_26.out.err
          h5diff_27.out
          h5diff_27.out.err
          h5diff_28.out
          h5diff_28.out.err
          h5diff_300.out
          h5diff_300.out.err
          h5diff_400.out
          h5diff_400.out.err
          h5diff_401.out
          h5diff_401.out.err
          h5diff_402.out
          h5diff_402.out.err
          h5diff_403.out
          h5diff_403.out.err
          h5diff_404.out
          h5diff_404.out.err
          h5diff_405.out
          h5diff_405.out.err
          h5diff_406.out
          h5diff_406.out.err
          h5diff_407.out
          h5diff_407.out.err
          h5diff_408.out
          h5diff_408.out.err
          h5diff_409.out
          h5diff_409.out.err
          h5diff_410.out
          h5diff_410.out.err
          h5diff_411.out
          h5diff_411.out.err
          h5diff_412.out
          h5diff_412.out.err
          h5diff_413.out
          h5diff_413.out.err
          h5diff_414.out
          h5diff_414.out.err
          h5diff_415.out
          h5diff_415.out.err
          h5diff_416.out
          h5diff_416.out.err
          h5diff_417.out
          h5diff_417.out.err
          h5diff_418.out
          h5diff_418.out.err
          h5diff_419.out
          h5diff_419.out.err
          h5diff_420.out
          h5diff_420.out.err
          h5diff_421.out
          h5diff_421.out.err
          h5diff_422.out
          h5diff_422.out.err
          h5diff_423.out
          h5diff_423.out.err
          h5diff_424.out
          h5diff_424.out.err
          h5diff_425.out
          h5diff_425.out.err
          h5diff_450.out
          h5diff_450.out.err
          h5diff_451.out
          h5diff_451.out.err
          h5diff_452.out
          h5diff_452.out.err
          h5diff_453.out
          h5diff_453.out.err
          h5diff_454.out
          h5diff_454.out.err
          h5diff_455.out
          h5diff_455.out.err
          h5diff_456.out
          h5diff_456.out.err
          h5diff_457.out
          h5diff_457.out.err
          h5diff_458.out
          h5diff_458.out.err
          h5diff_459.out
          h5diff_459.out.err
          h5diff_480.out
          h5diff_480.out.err
          h5diff_481.out
          h5diff_481.out.err
          h5diff_482.out
          h5diff_482.out.err
          h5diff_483.out
          h5diff_483.out.err
          h5diff_484.out
          h5diff_484.out.err
          h5diff_50.out
          h5diff_50.out.err
          h5diff_51.out
          h5diff_51.out.err
          h5diff_52.out
          h5diff_52.out.err
          h5diff_53.out
          h5diff_53.out.err
          h5diff_54.out
          h5diff_54.out.err
          h5diff_55.out
          h5diff_55.out.err
          h5diff_56.out
          h5diff_56.out.err
          h5diff_57.out
          h5diff_57.out.err
          h5diff_58.out
          h5diff_58.out.err
          h5diff_500.out
          h5diff_500.out.err
          h5diff_501.out
          h5diff_501.out.err
          h5diff_502.out
          h5diff_502.out.err
          h5diff_503.out
          h5diff_503.out.err
          h5diff_504.out
          h5diff_504.out.err
          h5diff_505.out
          h5diff_505.out.err
          h5diff_506.out
          h5diff_506.out.err
          h5diff_507.out
          h5diff_507.out.err
          h5diff_508.out
          h5diff_508.out.err
          h5diff_509.out
          h5diff_509.out.err
          h5diff_510.out
          h5diff_510.out.err
          h5diff_511.out
          h5diff_511.out.err
          h5diff_512.out
          h5diff_512.out.err
          h5diff_513.out
          h5diff_513.out.err
          h5diff_514.out
          h5diff_514.out.err
          h5diff_515.out
          h5diff_515.out.err
          h5diff_516.out
          h5diff_516.out.err
          h5diff_517.out
          h5diff_517.out.err
          h5diff_518.out
          h5diff_518.out.err
          h5diff_530.out
          h5diff_530.out.err
          h5diff_600.out
          h5diff_600.out.err
          h5diff_601.out
          h5diff_601.out.err
          h5diff_603.out
          h5diff_603.out.err
          h5diff_604.out
          h5diff_604.out.err
          h5diff_605.out
          h5diff_605.out.err
          h5diff_606.out
          h5diff_606.out.err
          h5diff_607.out
          h5diff_607.out.err
          h5diff_608.out
          h5diff_608.out.err
          h5diff_609.out
          h5diff_609.out.err
          h5diff_610.out
          h5diff_610.out.err
          h5diff_612.out
          h5diff_612.out.err
          h5diff_613.out
          h5diff_613.out.err
          h5diff_614.out
          h5diff_614.out.err
          h5diff_615.out
          h5diff_615.out.err
          h5diff_616.out
          h5diff_616.out.err
          h5diff_617.out
          h5diff_617.out.err
          h5diff_618.out
          h5diff_618.out.err
          h5diff_619.out
          h5diff_619.out.err
          h5diff_621.out
          h5diff_621.out.err
          h5diff_622.out
          h5diff_622.out.err
          h5diff_623.out
          h5diff_623.out.err
          h5diff_624.out
          h5diff_624.out.err
          h5diff_625.out
          h5diff_625.out.err
          h5diff_626.out
          h5diff_626.out.err
          h5diff_627.out
          h5diff_627.out.err
          h5diff_628.out
          h5diff_628.out.err
          h5diff_629.out
          h5diff_629.out.err
          h5diff_70.out
          h5diff_70.out.err
          h5diff_700.out
          h5diff_700.out.err
          h5diff_701.out
          h5diff_701.out.err
          h5diff_702.out
          h5diff_702.out.err
          h5diff_703.out
          h5diff_703.out.err
          h5diff_704.out
          h5diff_704.out.err
          h5diff_705.out
          h5diff_705.out.err
          h5diff_706.out
          h5diff_706.out.err
          h5diff_707.out
          h5diff_707.out.err
          h5diff_708.out
          h5diff_708.out.err
          h5diff_709.out
          h5diff_709.out.err
          h5diff_710.out
          h5diff_710.out.err
          h5diff_80.out
          h5diff_80.out.err
          h5diff_90.out
          h5diff_90.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5DIFF-clearall-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5DIFF-clearall-objects")

# ############################################################################
# # Common usage
# ############################################################################

# 1.0
ADD_H5_TEST (h5diff_10 0 -h)

# 1.1 normal mode
ADD_H5_TEST (h5diff_11 1  ${FILE1} ${FILE2}) 

# 1.2 normal mode with objects
ADD_H5_TEST (h5diff_12 1  ${FILE1} ${FILE2}  g1/dset1 g1/dset2)

# 1.3 report mode
ADD_H5_TEST (h5diff_13 1 -r ${FILE1} ${FILE2}) 

# 1.4 report  mode with objects
ADD_H5_TEST (h5diff_14 1  -r ${FILE1} ${FILE2} g1/dset1 g1/dset2)

# 1.5 with -d
ADD_H5_TEST (h5diff_15 1 --report --delta=5 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 1.6.1 with -p (int)
ADD_H5_TEST (h5diff_16_1 1 -v -p 0.02 ${FILE1} ${FILE1} g1/dset5 g1/dset6)

# 1.6.2 with -p (unsigned long_long)
ADD_H5_TEST (h5diff_16_2 1 --verbose --relative=0.02 ${FILE1} ${FILE1} g1/dset7 g1/dset8)

# 1.6.3 with -p (double)
ADD_H5_TEST (h5diff_16_3 1 -v -p 0.02 ${FILE1} ${FILE1} g1/dset9 g1/dset10)

# 1.7 verbose mode
ADD_H5_TEST (h5diff_17 1 -v ${FILE1} ${FILE2})   

# 1.7 test 32-bit INFINITY
ADD_H5_TEST (h5diff_171 0 -v ${FILE1} ${FILE1} /g1/fp19)

# 1.7 test 64-bit INFINITY
ADD_H5_TEST (h5diff_172 0 -v ${FILE1} ${FILE1} /g1/fp20)

# 1.8 quiet mode 
ADD_H5_TEST (h5diff_18 1 -q ${FILE1} ${FILE2}) 

# 1.8 -v and -q
ADD_H5_TEST (h5diff_18_1 2 -v -q ${FILE1} ${FILE2})


# ##############################################################################
# # not comparable types
# ##############################################################################

# 2.0
ADD_H5_TEST (h5diff_20 0 -v ${FILE3} ${FILE3}  dset g1)

# 2.1
ADD_H5_TEST (h5diff_21 0 -v ${FILE3} ${FILE3} dset l1)

# 2.2
ADD_H5_TEST (h5diff_22 0 -v  ${FILE3} ${FILE3} dset t1)

# ##############################################################################
# # compare groups, types, links (no differences and differences)
# ##############################################################################

# 2.3
ADD_H5_TEST (h5diff_23 0 -v ${FILE3} ${FILE3} g1 g1)

# 2.4
ADD_H5_TEST (h5diff_24 0 -v ${FILE3} ${FILE3} t1 t1)

# 2.5
ADD_H5_TEST (h5diff_25 0 -v ${FILE3} ${FILE3} l1 l1) 

# 2.6
ADD_H5_TEST (h5diff_26 0 -v ${FILE3} ${FILE3} g1 g2)

# 2.7
ADD_H5_TEST (h5diff_27 1 -v ${FILE3} ${FILE3} t1 t2)

# 2.8
ADD_H5_TEST (h5diff_28 1 -v ${FILE3} ${FILE3} l1 l2)



# ##############################################################################
# # Dataset datatypes
# ##############################################################################

# 5.0
ADD_H5_TEST (h5diff_50 1 -v ${FILE4} ${FILE4} dset0a dset0b)

# 5.1
ADD_H5_TEST (h5diff_51 1 -v ${FILE4} ${FILE4} dset1a dset1b)

# 5.2
ADD_H5_TEST (h5diff_52 1 -v ${FILE4} ${FILE4} dset2a dset2b)

# 5.3
ADD_H5_TEST (h5diff_53 1 -v ${FILE4} ${FILE4} dset3a dset4b)

# 5.4
ADD_H5_TEST (h5diff_54 1 -v ${FILE4} ${FILE4} dset4a dset4b)

# 5.5
ADD_H5_TEST (h5diff_55 1 -v ${FILE4} ${FILE4} dset5a dset5b)

# 5.6
ADD_H5_TEST (h5diff_56 1 -v ${FILE4} ${FILE4} dset6a dset6b)

# 5.7
ADD_H5_TEST (h5diff_57 0 -v ${FILE4} ${FILE4} dset7a dset7b)

# 5.8 (region reference)
ADD_H5_TEST (h5diff_58 1 -v ${FILE7} ${FILE8} refreg)

# ##############################################################################
# # Error messages
# ##############################################################################


# 6.0: Check if the command line number of arguments is less than 3
ADD_H5_TEST (h5diff_600 1 ${FILE1}) 

# 6.1: Check if non-exist object name is specified 
ADD_H5_TEST (h5diff_601 2 ${FILE1} ${FILE1} nono_obj)


# ##############################################################################
# # -d 
# ##############################################################################


# 6.3: negative value
ADD_H5_TEST (h5diff_603 1 -d -4 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.4: zero
ADD_H5_TEST (h5diff_604 1 -d 0 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.5: non number
ADD_H5_TEST (h5diff_605 1 -d u ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.6: hexadecimal
ADD_H5_TEST (h5diff_606 1 -d 0x1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.7: string
ADD_H5_TEST (h5diff_607 1 -d "1" ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.8: use system epsilon 
ADD_H5_TEST (h5diff_608 1 --use-system-epsilon ${FILE1} ${FILE2}  g1/dset3 g1/dset4)

# 6.9: number larger than biggest difference
ADD_H5_TEST (h5diff_609 0 -d 200 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.10: number smaller than smallest difference
ADD_H5_TEST (h5diff_610 1 -d 1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# ##############################################################################
# # -p
# ##############################################################################

# 6.12: negative value
ADD_H5_TEST (h5diff_612 1 -p -4 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.13: zero
ADD_H5_TEST (h5diff_613 1 -p 0 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.14: non number
ADD_H5_TEST (h5diff_614 1 -p u ${FILE1} ${FILE2}  g1/dset3 g1/dset4)

# 6.15: hexadecimal
ADD_H5_TEST (h5diff_615 1 -p 0x1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.16: string
ADD_H5_TEST (h5diff_616 1 -p "0.21" ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.17: repeated option
ADD_H5_TEST (h5diff_617 1 -p 0.21 -p 0.22 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.18: number larger than biggest difference
ADD_H5_TEST (h5diff_618 0 -p 2 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.19: number smaller than smallest difference
ADD_H5_TEST (h5diff_619 1 -p 0.005 ${FILE1} ${FILE2} g1/dset3 g1/dset4)


# ##############################################################################
# # -n
# ##############################################################################

# 6.21: negative value
ADD_H5_TEST (h5diff_621 1 -n -4 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.22: zero
ADD_H5_TEST (h5diff_622 1 -n 0 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.23: non number
ADD_H5_TEST (h5diff_623 1 -n u ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.24: hexadecimal
ADD_H5_TEST (h5diff_624 1 -n 0x1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.25: string
ADD_H5_TEST (h5diff_625 1 -n "2" ${FILE1} ${FILE2}  g1/dset3 g1/dset4)

# 6.26: repeated option
ADD_H5_TEST (h5diff_626 1 -n 2 -n 3 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.27: number larger than biggest difference
ADD_H5_TEST (h5diff_627 1 --count=200 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# 6.28: number smaller than smallest difference
ADD_H5_TEST (h5diff_628 1 -n 1 ${FILE1} ${FILE2} g1/dset3 g1/dset4)

# Disabling this test as it hangs - LRK 20090618
# 6.29  non valid files
#ADD_H5_TEST (h5diff_629 2 file1.h6 file2.h6)

# ##############################################################################
# 7.  attributes
# ##############################################################################
ADD_H5_TEST (h5diff_70 1 -v ${FILE5} ${FILE6}) 

# ##################################################
#  attrs with verbose option level
# ##################################################
ADD_H5_TEST (h5diff_700 1 -v1 ${FILE5} ${FILE6}) 
ADD_H5_TEST (h5diff_701 1 -v2 ${FILE5} ${FILE6})
ADD_H5_TEST (h5diff_702 1 --verbose=1 ${FILE5} ${FILE6})
ADD_H5_TEST (h5diff_703 1 --verbose=2 ${FILE5} ${FILE6})

# same attr number , all same attr name
ADD_H5_TEST (h5diff_704 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g)

# same attr number , some same attr name
ADD_H5_TEST (h5diff_705 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /dset)

# same attr number , all different attr name
ADD_H5_TEST (h5diff_706 0 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /ntype)

# different attr number , same attr name (intersected)
ADD_H5_TEST (h5diff_707 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g2)

# different attr number , all different attr name 
ADD_H5_TEST (h5diff_708 0 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g3)

# when no attributes exist in both objects
ADD_H5_TEST (h5diff_709 0 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2} /g4)

# file vs file
ADD_H5_TEST (h5diff_710 1 -v2 ${ATTR_VERBOSE_LEVEL_FILE1} ${ATTR_VERBOSE_LEVEL_FILE2})



# ##############################################################################
# 8.  all dataset datatypes
# ##############################################################################
ADD_H5_TEST (h5diff_80 1 -v ${FILE7} ${FILE8}) 

# 9. compare a file with itself
ADD_H5_TEST (h5diff_90 0 -v ${FILE2} ${FILE2})

# 10. read by hyperslab, print indexes
#if test -n "$pmode" -a "$mydomainname" = hdfgroup.uiuc.edu; then
#    # skip this test which sometimes hangs in some THG machines
#    MESSAGE (STATUS "SKIP -v ${FILE9} ${FILE10})
#else
#    ADD_H5_TEST (h5diff_100 1 -v ${FILE9} ${FILE10}) 
#fi

# 11. floating point comparison
ADD_H5_TEST (h5diff_101 1 -v ${FILE1} ${FILE1} g1/d1  g1/d2) 

ADD_H5_TEST (h5diff_102 1 -v ${FILE1} ${FILE1} g1/fp1 g1/fp2) 

# with --use-system-epsilon for double value. expect less differences  
ADD_H5_TEST (h5diff_103 1 -v --use-system-epsilon ${FILE1} ${FILE1} g1/d1
g1/d2) 

# with --use-system-epsilon for float value. expect less differences
ADD_H5_TEST (h5diff_104 1 -v --use-system-epsilon ${FILE1} ${FILE1} g1/fp1
g1/fp2)

# not comparable -c flag
ADD_H5_TEST (h5diff_200 0 ${FILE2} ${FILE2} g2/dset1  g2/dset2) 

ADD_H5_TEST (h5diff_201 0 -c ${FILE2} ${FILE2} g2/dset1  g2/dset2) 

ADD_H5_TEST (h5diff_202 0 -c ${FILE2} ${FILE2} g2/dset2  g2/dset3)

ADD_H5_TEST (h5diff_203 0 -c ${FILE2} ${FILE2} g2/dset3  g2/dset4)

ADD_H5_TEST (h5diff_204 0 -c ${FILE2} ${FILE2} g2/dset4  g2/dset5)

ADD_H5_TEST (h5diff_205 0 -c ${FILE2} ${FILE2} g2/dset5  g2/dset6)


# not comparable in compound
ADD_H5_TEST (h5diff_206 0 -c ${FILE2} ${FILE2} g2/dset7  g2/dset8)

ADD_H5_TEST (h5diff_207 0 -c ${FILE2} ${FILE2} g2/dset8  g2/dset9)

# ##############################################################################
# # Links compare without --follow-symlinks nor --no-dangling-links
# ##############################################################################
# test for bug1749
ADD_H5_TEST (h5diff_300 1 -v ${FILE12} ${FILE12} /link_g1 /link_g2)

# ##############################################################################
# # Links compare with --follow-symlinks Only
# ##############################################################################
# soft links file to file
ADD_H5_TEST (h5diff_400 0 --follow-symlinks -v ${FILE13} ${FILE13})

# softlink vs dset"
ADD_H5_TEST (h5diff_401 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_dset1_1 /target_dset2)

# dset vs softlink"
ADD_H5_TEST (h5diff_402 1 --follow-symlinks -v ${FILE13} ${FILE13} /target_dset2 /softlink_dset1_1)

# softlink vs softlink"
ADD_H5_TEST (h5diff_403 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_dset1_1 /softlink_dset2)

# extlink vs extlink (FILE)"
ADD_H5_TEST (h5diff_404 0 --follow-symlinks -v ${FILE15} ${FILE15})

# extlink vs dset"
ADD_H5_TEST (h5diff_405 1 --follow-symlinks -v ${FILE15} ${FILE16} /ext_link_dset1 /target_group2/x_dset)

# dset vs extlink"
ADD_H5_TEST (h5diff_406 1 --follow-symlinks -v ${FILE16} ${FILE15} /target_group2/x_dset /ext_link_dset1)

# extlink vs extlink"
ADD_H5_TEST (h5diff_407 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_dset1 /ext_link_dset2)

# softlink vs extlink"
ADD_H5_TEST (h5diff_408 1 --follow-symlinks -v ${FILE13} ${FILE15} /softlink_dset1_1 /ext_link_dset2)

# extlink vs softlink "
ADD_H5_TEST (h5diff_409 1 --follow-symlinks -v ${FILE15} ${FILE13} /ext_link_dset2 /softlink_dset1_1)

# linked_softlink vs linked_softlink (FILE)"
ADD_H5_TEST (h5diff_410 0 --follow-symlinks -v ${FILE14} ${FILE14})

# dset2 vs linked_softlink_dset1"
ADD_H5_TEST (h5diff_411 1 --follow-symlinks -v ${FILE14} ${FILE14} /target_dset2 /softlink1_to_slink2)

# linked_softlink_dset1 vs dset2"
ADD_H5_TEST (h5diff_412 1 --follow-symlinks -v ${FILE14} ${FILE14} /softlink1_to_slink2 /target_dset2)

# linked_softlink_to_dset1 vs linked_softlink_to_dset2"
ADD_H5_TEST (h5diff_413 1 --follow-symlinks -v ${FILE14} ${FILE14} /softlink1_to_slink2 /softlink2_to_slink2)

# group vs linked_softlink_group1"
ADD_H5_TEST (h5diff_414 1 --follow-symlinks -v ${FILE14} ${FILE14} /target_group /softlink3_to_slink2)

# linked_softlink_group1 vs group"
ADD_H5_TEST (h5diff_415 1 --follow-symlinks -v ${FILE14} ${FILE14} /softlink3_to_slink2 /target_group)

# linked_softlink_to_group1 vs linked_softlink_to_group2"
ADD_H5_TEST (h5diff_416 0 --follow-symlinks -v ${FILE14} ${FILE14} /softlink3_to_slink2 /softlink4_to_slink2)

# non-exist-softlink vs softlink"
ADD_H5_TEST (h5diff_417 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_noexist /softlink_dset2)

# softlink vs non-exist-softlink"
ADD_H5_TEST (h5diff_418 1 --follow-symlinks -v ${FILE13} ${FILE13} /softlink_dset2 /softlink_noexist)

# non-exist-extlink_file vs extlink"
ADD_H5_TEST (h5diff_419 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_noexist2 /ext_link_dset2)

# exlink vs non-exist-extlink_file"
ADD_H5_TEST (h5diff_420 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_dset2 /ext_link_noexist2)

# extlink vs non-exist-extlink_obj"
ADD_H5_TEST (h5diff_421 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_dset2 /ext_link_noexist1)

# non-exist-extlink_obj vs extlink"
ADD_H5_TEST (h5diff_422 1 --follow-symlinks -v ${FILE15} ${FILE15} /ext_link_noexist1 /ext_link_dset2)

# extlink_to_softlink_to_dset1 vs dset2"
ADD_H5_TEST (h5diff_423 1 --follow-symlinks -v ${FILE17} ${FILE18} /ext_link_to_slink1 /dset2)

# dset2 vs extlink_to_softlink_to_dset1"
ADD_H5_TEST (h5diff_424 1 --follow-symlinks -v ${FILE18} ${FILE17} /dset2 /ext_link_to_slink1)

# extlink_to_softlink_to_dset1 vs extlink_to_softlink_to_dset2"
ADD_H5_TEST (h5diff_425 1 --follow-symlinks -v ${FILE17} ${FILE17} /ext_link_to_slink1 /ext_link_to_slink2)


# ##############################################################################
# # Dangling links compare (--follow-symlinks and --no-dangling-links)
# ##############################################################################
# dangling links --follow-symlinks (FILE to FILE)
ADD_H5_TEST (h5diff_450 1  --follow-symlinks -v ${DANGLE_LINK_FILE1} ${DANGLE_LINK_FILE2})

# dangling links --follow-symlinks and --no-dangling-links (FILE to FILE)
ADD_H5_TEST (h5diff_451 2  --follow-symlinks -v --no-dangling-links  ${DANGLE_LINK_FILE1} ${DANGLE_LINK_FILE2}) 

# try --no-dangling-links without --follow-symlinks options
ADD_H5_TEST (h5diff_452 2  --no-dangling-links  ${FILE13} ${FILE13})

# dangling link found for soft links (FILE to FILE)
ADD_H5_TEST (h5diff_453 2  --follow-symlinks -v --no-dangling-links  ${FILE13} ${FILE13})  

# dangling link found for soft links (obj to obj)
ADD_H5_TEST (h5diff_454 2  --follow-symlinks -v --no-dangling-links  ${FILE13} ${FILE13} /softlink_dset2 /softlink_noexist) 

# dangling link found for soft links (obj to obj) Both dangle links
ADD_H5_TEST (h5diff_455 2  --follow-symlinks -v --no-dangling-links  ${FILE13} ${FILE13} /softlink_noexist /softlink_noexist) 

# dangling link found for ext links (FILE to FILE)
ADD_H5_TEST (h5diff_456 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15}) 

# dangling link found for ext links (obj to obj). target file exist
ADD_H5_TEST (h5diff_457 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15} /ext_link_dset1 /ext_link_noexist1) 

# dangling link found for ext links (obj to obj). target file NOT exist
ADD_H5_TEST (h5diff_458 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15} /ext_link_dset1 /ext_link_noexist2)  

# dangling link found for ext links (obj to obj). Both dangle links
ADD_H5_TEST (h5diff_459 2  --follow-symlinks -v --no-dangling-links  ${FILE15} ${FILE15} /ext_link_noexist1 /ext_link_noexist2)

# ##############################################################################
# # test for group diff recursivly
# ##############################################################################
# root 
ADD_H5_TEST (h5diff_500 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} / /)
ADD_H5_TEST (h5diff_501 1 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} / /)

# root vs group
ADD_H5_TEST (h5diff_502 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} / /grp1/grp2/grp3)

# group vs group (same name and structure)
ADD_H5_TEST (h5diff_503 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /grp1)

# group vs group (different name and structure)
ADD_H5_TEST (h5diff_504 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1/grp2 /grp1/grp2/grp3)

# groups vs soft-link
ADD_H5_TEST (h5diff_505 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /slink_grp1)
ADD_H5_TEST (h5diff_506 0 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1/grp2 /slink_grp2)

# groups vs ext-link
ADD_H5_TEST (h5diff_507 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /elink_grp1)
ADD_H5_TEST (h5diff_508 0 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp1 /elink_grp1)

# soft-link vs ext-link
ADD_H5_TEST (h5diff_509 0 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp1 /elink_grp1)
ADD_H5_TEST (h5diff_510 0 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp1 /elink_grp1)

# circled ext links
ADD_H5_TEST (h5diff_511 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp10 /grp11)
ADD_H5_TEST (h5diff_512 1 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /grp10 /grp11)

# circled soft2ext-link vs soft2ext-link
ADD_H5_TEST (h5diff_513 1 -v ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp10 /slink_grp11)
ADD_H5_TEST (h5diff_514 1 -v --follow-symlinks ${GRP_RECURSE_FILE1} ${GRP_RECURSE_FILE2} /slink_grp10 /slink_grp11)

###############################################################################
# Test for group recursive diff via multi-linked external links 
# With follow-symlinks, file $GRP_RECURSE1_EXT and $GRP_RECURSE2_EXT1 should
# be same with the external links.
###############################################################################
# file vs file
ADD_H5_TEST (h5diff_515 1 -v ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1})
ADD_H5_TEST (h5diff_516 0 -v --follow-symlinks ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1})
# group vs group
ADD_H5_TEST (h5diff_517 1 -v ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1} /g1)
ADD_H5_TEST (h5diff_518 0 -v --follow-symlinks ${GRP_RECURSE1_EXT} ${GRP_RECURSE2_EXT1} /g1)


# ##############################################################################
# # Exclude path (--exclude-path)
# ##############################################################################
#
# Same structure, same names and different value.
#
# Exclude the object with different value. Expect return - same
ADD_H5_TEST (h5diff_480 0 -v --exclude-path /group1/dset3 ${EXCLUDE_FILE1_1} ${EXCLUDE_FILE1_2})
# Verify different by not excluding. Expect return - diff
ADD_H5_TEST (h5diff_481 1 -v ${EXCLUDE_FILE1_1} ${EXCLUDE_FILE1_2})

#
# Different structure, different names. 
#
# Exclude all the different objects. Expect return - same
ADD_H5_TEST (h5diff_482 0 -v --exclude-path "/group1" --exclude-path "/dset1" ${EXCLUDE_FILE2_1} ${EXCLUDE_FILE2_2})
# Exclude only some different objects. Expect return - diff
ADD_H5_TEST (h5diff_483 1 -v --exclude-path "/group1" ${EXCLUDE_FILE2_1} ${EXCLUDE_FILE2_2})

# Exclude from group compare
ADD_H5_TEST (h5diff_484 0 -v --exclude-path "/dset3" ${EXCLUDE_FILE1_1} ${EXCLUDE_FILE1_2} /group1)

# ##############################################################################
# # diff various multiple vlen and fixed strings in a compound type dataset
# ##############################################################################
ADD_H5_TEST (h5diff_530 0 -v  ${COMP_VL_STRS_FILE} ${COMP_VL_STRS_FILE})

  
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
        h5diff
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
