cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5LS)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

#-----------------------------------------------------------------------------
# Add the h5ls executable
#-----------------------------------------------------------------------------
ADD_EXECUTABLE (h5ls ${HDF5_TOOLS_H5LS_SOURCE_DIR}/h5ls.c)
TARGET_NAMING (h5ls ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5ls  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES
    h5ls
)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_FILES
      help-1.ls
      help-2.ls
      help-3.ls
      nosuchfile.ls
      tall-1.ls
      tall-2.ls
      tarray1.ls
      tattr2.ls
      tcomp-1.ls
      tdataregbe.ls
      tdataregle.ls
      tdset-1.ls
      tempty.ls
      textlink-1.ls
      textlinksrc-1.ls
      textlinksrc-2.ls
      textlinksrc-3.ls
      textlinksrc-4.ls
      textlinksrc-5.ls
      textlinksrc-6.ls
      textlinksrc-7.ls
      textlinksrc-1-old.ls
      textlinksrc-2-old.ls
      textlinksrc-3-old.ls
      textlinksrc-6-old.ls
      textlinksrc-7-old.ls
      tsoftlinks-1.ls
      tsoftlinks-2.ls
      tsoftlinks-3.ls
      tsoftlinks-4.ls
      tsoftlinks-5.ls
      textlinksrc-nodangle-1.ls
      textlinksrc-nodangle-2.ls
      tgrp_comments.ls
      tsoftlinks-nodangle-1.ls
      thlinks-nodangle-1.ls
      tgroup.ls
      tgroup-1.ls
      tgroup-2.ls
      tgroup-3.ls
      thlink-1.ls
      tloop-1.ls
      tnestcomp-1.ls
      tnestcomp-2.ls
      tnestcomp-3.ls
      tnestcomp-4.ls
      tsaf.ls
      tslink-1.ls
      tstr-1.ls
      tudlink-1.ls
      tvldtypes1.ls
      tvldtypes2le.ls
      tvldtypes2be.ls
  )
  SET (HDF5_REFERENCE_TEST_FILES
      tall.h5
      tarray1.h5
      tattr2.h5
      tcompound.h5
      tdatareg.h5
      tdset.h5
      tempty.h5
      textlink.h5
      textlinksrc.h5
      textlinktar.h5
      tgroup.h5
      tgrp_comments.h5
      thlink.h5
      tloop.h5
      tnestedcomp.h5
      tsaf.h5
      tslink.h5
      tsoftlinks.h5
      tstr.h5
      tudlink.h5
      tvldtypes1.h5
  )

  FOREACH (ls_file ${HDF5_REFERENCE_FILES})
    SET (lsdest "${PROJECT_BINARY_DIR}/${ls_file}")
    #MESSAGE (STATUS " Translating ${ls_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5ls
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_SRC_DIR}/testfiles/${ls_file} ${lsdest} -l3
    )
  ENDFOREACH (ls_file ${HDF5_REFERENCE_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5ls
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_file} ${dest}
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
      ADD_TEST (NAME H5LS-${resultfile} COMMAND $<TARGET_FILE:h5ls> ${ARGN})
      IF (${resultcode} STREQUAL "1")
        SET_TESTS_PROPERTIES (H5LS-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (${resultcode} STREQUAL "1")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5LS-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ls"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5LS-${resultfile} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5LS-${resultfile}")
  ENDMACRO (ADD_H5_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5LS-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          help-1.out
          help-1.out.err
          help-2.out
          help-2.out.err
          help-3.out
          help-3.out.err
          nosuchfile.out
          nosuchfile.out.err
          tall-1.out
          tall-1.out.err
          tall-2.out
          tall-2.out.err
          tarray1.out
          tarray1.out.err
          tattr2.out
          tattr2.out.err
          tcomp-1.out
          tcomp-1.out.err
          tdataregbe.out
          tdataregbe.out.err
          tdataregle.out
          tdataregle.out.err
          tdset-1.out
          tdset-1.out.err
          tempty.out
          tempty.out.err
          textlink-1.out
          textlink-1.out.err
          textlinksrc-1.out
          textlinksrc-1.out.err
          textlinksrc-2.out
          textlinksrc-2.out.err
          textlinksrc-3.out
          textlinksrc-3.out.err
          textlinksrc-4.out
          textlinksrc-4.out.err
          textlinksrc-5.out
          textlinksrc-5.out.err
          textlinksrc-6.out
          textlinksrc-6.out.err
          textlinksrc-7.out
          textlinksrc-7.out.err
          textlinksrc-1-old.out
          textlinksrc-1-old.out.err
          textlinksrc-2-old.out
          textlinksrc-2-old.out.err
          textlinksrc-3-old.out
          textlinksrc-3-old.out.err
          textlinksrc-6-old.out
          textlinksrc-6-old.out.err
          textlinksrc-7-old.out
          textlinksrc-7-old.out.err
          tgrp_comments.out
          tgrp_comments.out.err
          tsoftlinks-1.out
          tsoftlinks-1.out.err
          tsoftlinks-2.out
          tsoftlinks-2.out.err
          tsoftlinks-3.out
          tsoftlinks-3.out.err
          tsoftlinks-4.out
          tsoftlinks-4.out.err
          tsoftlinks-5.out
          tsoftlinks-5.out.err
          textlinksrc-nodangle-1.out
          textlinksrc-nodangle-1.out.err
          textlinksrc-nodangle-2.out
          textlinksrc-nodangle-2.out.err
          tsoftlinks-nodangle-1.out
          tsoftlinks-nodangle-1.out.err
          thlinks-nodangle-1.out
          thlinks-nodangle-1.out.err
          tgroup.out
          tgroup.out.err
          tgroup-1.out
          tgroup-1.out.err
          tgroup-2.out
          tgroup-2.out.err
          tgroup-3.out
          tgroup-3.out.err
          thlink-1.out
          thlink-1.out.err
          tloop-1.out
          tloop-1.out.err
          tnestcomp-1.out
          tnestcomp-1.out.err
          tnestcomp-2.out
          tnestcomp-2.out.err
          tnestcomp-3.out
          tnestcomp-3.out.err
          tnestcomp-4.out
          tnestcomp-4.out.err
          tsaf.out
          tsaf.out.err
          tslink-1.out
          tslink-1.out.err
          tstr-1.out
          tstr-1.out.err
          tudlink-1.out
          tudlink-1.out.err
          tvldtypes1.out
          tvldtypes1.out.err
          tvldtypes2le.out
          tvldtypes2le.out.err
          tvldtypes2be.out
          tvldtypes2be.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5LS-clearall-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5LS-clearall-objects")

  # test the help syntax
  ADD_H5_TEST (help-1 0 -w80 -h)
  ADD_H5_TEST (help-2 0 -w80 --help)
  ADD_H5_TEST (help-3 0 -w80 -?)

  # test simple command
  ADD_H5_TEST (tall-1 0 -w80 tall.h5)
  ADD_H5_TEST (tall-2 0 -w80 -r -d tall.h5)
  ADD_H5_TEST (tgroup 0 -w80 tgroup.h5)
  ADD_H5_TEST (tgroup-3 0 -w80 tgroup.h5/g1)

  # test for displaying groups
  # The following combination of arguments is expected to return an error message
  # and return value 1
  ADD_H5_TEST (tgroup-1 1 -w80 -r -g tgroup.h5)
  ADD_H5_TEST (tgroup-2 0 -w80 -g tgroup.h5/g1)

  # test for files with groups that have long comments
  ADD_H5_TEST (tgrp_comments 0 -w80 -v -g tgrp_comments.h5/glongcomment)

  # test for displaying simple space datasets
  ADD_H5_TEST (tdset-1 0 -w80 -r -d tdset.h5)

  # test for displaying soft links (dangle)
  ADD_H5_TEST (tslink-1 0 -w80 -r tslink.h5)

  # test for displaying more soft links with --follow-symlinks
  ADD_H5_TEST (tsoftlinks-1 0 --follow-symlinks tsoftlinks.h5)
  ADD_H5_TEST (tsoftlinks-2 0 --follow-symlinks -r tsoftlinks.h5)
  ADD_H5_TEST (tsoftlinks-3 0 --follow-symlinks tsoftlinks.h5/group1)
  ADD_H5_TEST (tsoftlinks-4 0 --follow-symlinks -r tsoftlinks.h5/group1)
  ADD_H5_TEST (tsoftlinks-5 0 --follow-symlinks tsoftlinks.h5/soft_dset1)

  # test for displaying external and user-defined links with --follow-symlinks
  ADD_H5_TEST (textlink-1 0 -w80 -r textlink.h5)
  ADD_H5_TEST (textlinksrc-1 0 -w80 --follow-symlinks -r textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-2 0 -w80 --follow-symlinks -rv textlinksrc.h5/ext_link5)
  ADD_H5_TEST (textlinksrc-3 0 -w80 --follow-symlinks -r textlinksrc.h5/ext_link1)
  ADD_H5_TEST (textlinksrc-4 0 -w80 -r textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-5 0 -w80 -r textlinksrc.h5/ext_link1)
  ADD_H5_TEST (textlinksrc-6 0 -w80 --follow-symlinks textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-7 0 -w80 --follow-symlinks textlinksrc.h5/ext_link1)
  ADD_H5_TEST (tudlink-1 0 -w80 -r tudlink.h5)

  # test for displaying external links with -E
  # the option -E will be depriciated but keep it for backward compatibility
  ADD_H5_TEST (textlinksrc-1-old 0 -w80 -Er textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-2-old 0 -w80 -Erv textlinksrc.h5/ext_link5)
  ADD_H5_TEST (textlinksrc-3-old 0 -w80 -Er textlinksrc.h5/ext_link1)
  ADD_H5_TEST (textlinksrc-6-old 0 -w80 -E textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-7-old 0 -w80 -E textlinksrc.h5/ext_link1)

  # tests for no-dangling-links 
  # if this option is given on dangling link, h5ls should return exit code 1
  # when used alone , expect to print out help and return exit code 1
  ADD_H5_TEST (textlinksrc-nodangle-1 1 -w80 --no-dangling-links textlinksrc.h5)
  # external dangling link - expected exit code 1
  ADD_H5_TEST (textlinksrc-nodangle-2 1 -w80 --follow-symlinks --no-dangling-links textlinksrc.h5)
  # soft dangling link - expected exit code 1
  ADD_H5_TEST (tsoftlinks-nodangle-1 1 -w80 --follow-symlinks --no-dangling-links tsoftlinks.h5)
  # when used file with no dangling links - expected exit code 0
  ADD_H5_TEST (thlinks-nodangle-1 0 -w80 --follow-symlinks --no-dangling-links thlink.h5)

  # tests for hard links
  ADD_H5_TEST (thlink-1 0 -w80 thlink.h5)

  # tests for compound data types
  ADD_H5_TEST (tcomp-1 0 -w80 -r -d tcompound.h5)

  #test for the nested compound type
  ADD_H5_TEST (tnestcomp-1 0 -w80 -r -d tnestedcomp.h5)

  ADD_H5_TEST (tnestcomp-2 0 -w80 -r -d -S tnestedcomp.h5)

  ADD_H5_TEST (tnestcomp-3 0 -w80 -r -d -l tnestedcomp.h5)

  ADD_H5_TEST (tnestcomp-4 0 -w80 -r -d -l -S tnestedcomp.h5)

  # test for loop detection
  ADD_H5_TEST (tloop-1 0 -w80 -r -d tloop.h5)

  # test for string
  ADD_H5_TEST (tstr-1 0 -w80 -r -d tstr.h5)

  # test test file created from lib SAF team
  ADD_H5_TEST (tsaf 0 -w80 -r -d tsaf.h5)

  # test for variable length data types
  ADD_H5_TEST (tvldtypes1 0 -w80 -r -d tvldtypes1.h5)

  # test for array data types
  ADD_H5_TEST (tarray1 0 -w80 -r -d tarray1.h5)

  # test for empty data
  ADD_H5_TEST (tempty 0 -w80 -d tempty.h5)

  # test for all dataset types written to attributes
  # enable -S for avoiding printing NATIVE types
  ADD_H5_TEST (tattr2 0 -w80 -v -S tattr2.h5)

  # test for non-existing file
  ADD_H5_TEST (nosuchfile 1 nosuchfile.h5)

  IF (WORDS_BIGENDIAN)
    # test for variable length data types in verbose mode
    ADD_H5_TEST (tvldtypes2be 0 -v tvldtypes1.h5)
    # test for dataset region references data types in verbose mode
    ADD_H5_TEST (tdataregbe 0 -v tdatareg.h5)
  ELSE (WORDS_BIGENDIAN)
    ADD_H5_TEST (tvldtypes2le 0 -v tvldtypes1.h5)
    ADD_H5_TEST (tdataregle 0 -v tdatareg.h5)
  ENDIF (WORDS_BIGENDIAN)

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
        h5ls
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
