cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5DUMP)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5dump executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5dump ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump.c)
TARGET_NAMING (h5dump ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5dump  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5dump)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5dump test executable
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5dumpgentest ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dumpgentest.c)
    TARGET_NAMING (h5dumpgentest ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (h5dumpgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    
    #ADD_TEST (NAME h5dumpgentest COMMAND $<TARGET_FILE:h5dumpgentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_FILES
      filter_fail.ddl
      packedbits.ddl
      tall-1.ddl
      tall-2.ddl
      tall-2A.ddl
      tall-2B.ddl
      tall-3.ddl
      tall-4s.ddl
      tall-5s.ddl
      tallfilters.ddl
      tarray1.ddl
      tarray1_big.ddl
      tarray2.ddl
      tarray3.ddl
      tarray4.ddl
      tarray5.ddl
      tarray6.ddl
      tarray7.ddl
      tarray8.ddl
      tattr-1.ddl
      tattr-2.ddl
      tattr-3.ddl
      tattrreg.ddl
      tattrregR.ddl
      tbinregR.ddl
      tbigdims.ddl
      tboot1.ddl
      tboot2.ddl
      tchar1.ddl
      tchunked.ddl
      tcomp-1.ddl
      tcomp-2.ddl
      tcomp-3.ddl
      tcomp-4.ddl
      tcompact.ddl
      tcontents.ddl
      tcontiguos.ddl
      tdatareg.ddl
      tdataregR.ddl
      tdeflate.ddl
      tdset-1.ddl
      tdset-2.ddl
      tdset-3s.ddl
      tempty.ddl
      texternal.ddl
      textlinksrc.ddl
      textlinkfar.ddl
      textlink.ddl
      tfamily.ddl
      tfill.ddl
      tfletcher32.ddl
      tfpformat.ddl
      tgroup-1.ddl
      tgroup-2.ddl
      tgrp_comments.ddl
      thlink-1.ddl
      thlink-2.ddl
      thlink-3.ddl
      thlink-4.ddl
      thlink-5.ddl
      thyperslab.ddl
      tindicesno.ddl
      tindicessub1.ddl
      tindicessub2.ddl
      tindicessub3.ddl
      tindicessub4.ddl
      tindicesyes.ddl
      tlarge_objname.ddl
      #tldouble.ddl
      tlonglinks.ddl
      tloop-1.ddl
      tmulti.ddl
      tnamed_dtype_attr.ddl
      tnestcomp-1.ddl
      tnbit.ddl
      tnofilename.ddl
      tnullspace.ddl
      tordergr1.ddl
      tordergr2.ddl
      tordergr3.ddl
      tordergr4.ddl
      tordergr5.ddl
      torderattr1.ddl
      torderattr2.ddl
      torderattr3.ddl
      torderattr4.ddl
      tperror.ddl
      treference.ddl
      tsaf.ddl
      tscaleoffset.ddl
      tshuffle.ddl
      tslink-1.ddl
      tslink-2.ddl
      tsplit_file.ddl
      tstr-1.ddl
      tstr-2.ddl
      tstring.ddl
      tstring2.ddl
      tstringe.ddl
      tszip.ddl
      tudlink-1.ddl
      tudlink-2.ddl
      tuserfilter.ddl
      tvldtypes1.ddl
      tvldtypes2.ddl
      tvldtypes3.ddl
      tvldtypes4.ddl
      tvldtypes5.ddl
      tvlstr.ddl
      tvms.ddl
  )
  SET (HDF5_XML_REFERENCE_FILES
      tall.h5.xml
      tall-2A.h5.xml
      tarray1.h5.xml
      tarray2.h5.xml
      tarray3.h5.xml
      tarray6.h5.xml
      tarray7.h5.xml
      tattr.h5.xml
      tbitfields.h5.xml
      tcompound_complex.h5.xml
      tcompound.h5.xml
      tcompound2.h5.xml
      tdatareg.h5.xml
      tdset.h5.xml
      tdset2.h5.xml
      tempty.h5.xml
      tempty-dtd.h5.xml
      tempty-dtd-2.h5.xml
      tempty-dtd-uri.h5.xml
      tempty-nons.h5.xml
      tempty-nons-2.h5.xml
      tempty-nons-uri.h5.xml
      tempty-ns.h5.xml
      tempty-ns-2.h5.xml
      tenum.h5.xml
      textlink.h5.xml
      tfpformat.h5.xml
      tgroup.h5.xml
      thlink.h5.xml
      tloop.h5.xml
      tloop2.h5.xml
      tmany.h5.xml
      tname-amp.h5.xml
      tname-apos.h5.xml
      tnamed_dtype_attr.h5.xml
      tname-gt.h5.xml
      tname-lt.h5.xml
      tname-quot.h5.xml
      tname-sp.h5.xml
      tnestedcomp.h5.xml
      tnodata.h5.xml
      tobjref.h5.xml
      topaque.h5.xml
      torderattr1.h5.xml
      torderattr2.h5.xml
      torderattr3.h5.xml
      torderattr4.h5.xml
      tref.h5.xml
      tref-escapes.h5.xml
      tref-escapes-at.h5.xml
      tsaf.h5.xml
      tslink.h5.xml
      tstr.h5.xml
      tstr2.h5.xml
      tstring.h5.xml
      tstring-at.h5.xml
      tudlink.h5.xml
      tvldtypes1.h5.xml
      tvldtypes2.h5.xml
      tvldtypes3.h5.xml
      tvldtypes4.h5.xml
      tvldtypes5.h5.xml
      tvlstr.h5.xml
  )
  SET (HDF5_REFERENCE_TEST_FILES
      tbin1.ddl
      tbin1.ddl
      tbin2.ddl
      tbin3.ddl
      tbin4.ddl
      tall-6.ddl
      texceedsubstart.ddl
      texceedsubcount.ddl
      texceedsubstride.ddl
      texceedsubblock.ddl
      out3.h5import
      filter_fail.h5
      packedbits.h5
      taindices.h5
      tall.h5
      tarray1.h5
      tarray1_big.h5
      tarray2.h5
      tarray3.h5
      tarray4.h5
      tarray5.h5
      tarray6.h5
      tarray7.h5
      tarray8.h5
      tattr.h5
      tattr2.h5
      tattrreg.h5
      tbigdims.h5
      tbinary.h5
      tchar.h5
      tcompound.h5
      tcompound_complex.h5
      tdatareg.h5
      tdset.h5
      tempty.h5
      textlinkfar.h5
      textlinksrc.h5
      textlinktar.h5
      textlink.h5
      tfamily00000.h5
      tfamily00001.h5
      tfamily00002.h5
      tfamily00003.h5
      tfamily00004.h5
      tfamily00005.h5
      tfamily00006.h5
      tfamily00007.h5
      tfamily00008.h5
      tfamily00009.h5
      tfamily00010.h5
      tfcontents1.h5
      tfcontents2.h5
      tfilters.h5
      tfpformat.h5
      tfvalues.h5
      tgroup.h5
      tgrp_comments.h5
      thlink.h5
      thyperslab.h5
      tlarge_objname.h5
      #tldouble.h5
      tlonglinks.h5
      tloop.h5
      tmulti-b.h5
      tmulti-g.h5
      tmulti-l.h5
      tmulti-o.h5
      tmulti-r.h5
      tmulti-s.h5
      tnamed_dtype_attr.h5
      tnestedcomp.h5
      tnullspace.h5
      torderattr.h5
      tordergr.h5
      tsaf.h5
      tslink.h5
      tsplit_file-m.h5
      tsplit_file-r.h5
      tstr.h5
      tstr2.h5
      tstr3.h5
      tudlink.h5
      tvldtypes1.h5
      tvldtypes2.h5
      tvldtypes3.h5
      tvldtypes4.h5
      tvldtypes5.h5
      tvlstr.h5
      tvms.h5
  )
  SET (HDF5_XML_REFERENCE_TEST_FILES
      tbitfields.h5
      tcompound2.h5
      tdset2.h5
      tenum.h5
      textlink.h5
      tloop2.h5
      tmany.h5
      tname-amp.h5
      tname-apos.h5
      tname-gt.h5
      tname-lt.h5
      tname-quot.h5
      tname-sp.h5
      tnodata.h5
      tobjref.h5
      topaque.h5
      tref.h5
      tref-escapes.h5
      tref-escapes-at.h5
      tstring.h5
      tstring-at.h5
  )

  FOREACH (ddl_file ${HDF5_REFERENCE_FILES})
    SET (ddldest "${PROJECT_BINARY_DIR}/${ddl_file}")
    #MESSAGE (STATUS " Translating ${ddl_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_SRC_DIR}/testfiles/${ddl_file} ${ddldest} -l3
    )
  ENDFOREACH (ddl_file ${HDF5_REFERENCE_FILES})

  FOREACH (xml_file ${HDF5_XML_REFERENCE_FILES})
    SET (xmldest "${PROJECT_BINARY_DIR}/${xml_file}")
    #MESSAGE (STATUS " Translating ${xml_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_SRC_DIR}/testfiles/${xml_file} ${xmldest} -l3
    )
  ENDFOREACH (xml_file ${HDF5_XML_REFERENCE_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

  FOREACH (h5_xml_file ${HDF5_XML_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_xml_file}")
    #MESSAGE (STATUS " Copying ${h5_xml_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_xml_file} ${dest}
    )
  ENDFOREACH (h5_xml_file ${HDF5_XML_REFERENCE_TEST_FILES})
  
  # --------------------------------------------------------------------
  # Special file handling
  # --------------------------------------------------------------------
  ADD_CUSTOM_COMMAND (
      TARGET     h5dump
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_SOURCE_DIR}/testfiles/tbin1.ddl ${PROJECT_BINARY_DIR}/tbin1LE.ddl
  )
  
  IF (WIN32 AND NOT CYGWIN)
    FILE (READ ${HDF5_TOOLS_SRC_DIR}/testfiles/tbinregR.exp TEST_STREAM)
    FILE (WRITE ${PROJECT_BINARY_DIR}/tbinregR.exp "${TEST_STREAM}")
  ELSE (WIN32 AND NOT CYGWIN)
    ADD_CUSTOM_COMMAND (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/tbinregR.exp ${PROJECT_BINARY_DIR}/tbinregR.exp
    )
  ENDIF (WIN32 AND NOT CYGWIN)
  
  # --------------------------------------------------------------------
  # Special option for Packed Bits
  # --------------------------------------------------------------------
  IF (HDF5_USE_H5DUMP_PACKED_BITS)
    #-- Copy all the HDF5 files from the test directory into the source directory
    SET (HDF5_REFERENCE_PBITS
        tnofilename-with-packed-bits.ddl
        tpbitsArray.ddl
        tpbitsCompound.ddl
        tpbitsIncomplete.ddl
        tpbitsLengthExceeded.ddl
        tpbitsCharLengthExceeded.ddl
        tpbitsIntLengthExceeded.ddl
        tpbitsLongLengthExceeded.ddl
        tpbitsLengthPositive.ddl
        tpbitsMax.ddl
        tpbitsMaxExceeded.ddl
        tpbitsOffsetExceeded.ddl
        tpbitsCharOffsetExceeded.ddl
        tpbitsIntOffsetExceeded.ddl
        tpbitsLongOffsetExceeded.ddl
        tpbitsOffsetNegative.ddl
        tpbitsOverlapped.ddl
        tpbitsSigned.ddl
        tpbitsUnsigned.ddl
        tpbitsSignedInt.ddl
        tpbitsUnsignedInt.ddl
        tpbitsSignedLong.ddl
        tpbitsUnsignedLong.ddl
        tpbitsSignedLongLong.ddl
        tpbitsUnsignedLongLong.ddl
        tpbitsSignedWhole.ddl
        tpbitsUnsignedWhole.ddl
        tpbitsSignedIntWhole.ddl
        tpbitsUnsignedIntWhole.ddl
        tpbitsSignedLongWhole.ddl
        tpbitsUnsignedLongWhole.ddl
        tpbitsSignedLongLongWhole.ddl
        tpbitsUnsignedLongLongWhole.ddl
        tpbitsSignedLongLongWhole1.ddl
        tpbitsUnsignedLongLongWhole1.ddl
        tpbitsSignedLongLongWhole63.ddl
        tpbitsUnsignedLongLongWhole63.ddl
        tpbitsSigned4.ddl
        tpbitsUnsigned4.ddl
        tpbitsSignedInt8.ddl
        tpbitsUnsignedInt8.ddl
        tpbitsSignedLong16.ddl
        tpbitsUnsignedLong16.ddl
        tpbitsSignedLongLong32.ddl
        tpbitsUnsignedLongLong32.ddl
        tpbitsSigned2.ddl
        tpbitsUnsigned2.ddl
        tpbitsSignedInt4.ddl
        tpbitsUnsignedInt4.ddl
        tpbitsSignedLong8.ddl
        tpbitsUnsignedLong8.ddl
        tpbitsSignedLongLong16.ddl
        tpbitsUnsignedLongLong16.ddl
    )

    FOREACH (ddl_pbits ${HDF5_REFERENCE_PBITS})
      SET (ddldest "${PROJECT_BINARY_DIR}/${ddl_pbits}")
      #MESSAGE (STATUS " Translating ${ddl_pbits}")
      ADD_CUSTOM_COMMAND (
          TARGET     h5dump
          POST_BUILD
          COMMAND    ${XLATE_UTILITY}
          ARGS       ${HDF5_TOOLS_SRC_DIR}/testfiles/${ddl_pbits} ${ddldest} -l3
      )
    ENDFOREACH (ddl_pbits ${HDF5_REFERENCE_PBITS})

  ENDIF (HDF5_USE_H5DUMP_PACKED_BITS)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5DUMP-${skipresultfile}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile} ${ARGN}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_SKIP_H5_TEST)

  MACRO (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
      IF (NOT ${resultcode} STREQUAL "0")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT ${resultcode} STREQUAL "0")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5DUMP-${resultfile}")
  ENDMACRO (ADD_H5_TEST file)

  MACRO (ADD_H5_EXPORT_TEST resultfile targetfile resultcode)
    ADD_TEST (
        NAME H5DUMP-output-${resultfile}
        COMMAND $<TARGET_FILE:h5dump> -d /Dataset1 -s 0 -R -y -o ${PROJECT_BINARY_DIR}/${resultfile}.txt ${PROJECT_BINARY_DIR}/${targetfile}
    )
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5DUMP-output-${resultfile} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5DUMP-output-${resultfile}")
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-output-cmp-${resultfile}
          COMMAND ${CMAKE_COMMAND}
                -E compare_files ${PROJECT_BINARY_DIR}/${resultfile}.txt ${PROJECT_BINARY_DIR}/${resultfile}.exp
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5DUMP-output-cmp-${resultfile} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      SET (last_test "H5DUMP-output-cmp-${resultfile}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_EXPORT_TEST file)

  MACRO (ADD_H5_MASK_TEST resultfile resultcode)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_MASK_ERROR=true"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      SET (last_test "H5DUMP-${resultfile}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_MASK_TEST file)

  MACRO (ADD_XML_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5DUMP-XML-${skipresultfile}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile}.xml --xml ${ARGN}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_XML_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_XML_SKIP_H5_TEST)

  MACRO (ADD_XML_H5_TEST resultfile resultcode)
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5DUMP-XML-${resultfile} COMMAND $<TARGET_FILE:h5dump> --xml ${ARGN})
      IF (NOT ${resultcode} STREQUAL "0")
        SET_TESTS_PROPERTIES (H5DUMP-XML-${resultfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT ${resultcode} STREQUAL "0")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5DUMP-XML-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=--xml;${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.xml"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5DUMP-XML-${resultfile} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5DUMP-XML-${resultfile}")
  ENDMACRO (ADD_XML_H5_TEST file)

  # --------------------------------------------------------------------
  # Determine which filters are available
  # --------------------------------------------------------------------
  SET (USE_FILTER_SZIP H5_HAVE_FILTER_SZIP)
  SET (USE_FILTER_DEFLATE H5_HAVE_FILTER_DEFLATE)
  SET (USE_FILTER_SHUFFLE H5_HAVE_FILTER_SHUFFLE)
  SET (USE_FILTER_FLETCHER32 H5_HAVE_FILTER_FLETCHER32)
  SET (USE_FILTER_NBIT H5_HAVE_FILTER_NBIT)
  SET (USE_FILTER_SCALEOFFSET H5_HAVE_FILTER_SCALEOFFSET)

  # --------------------------------------------------------------------
  # Determine if H5dump packed bits feature is included
  # --------------------------------------------------------------------
  SET (USE_PACKED_BITS HDF5_USE_H5DUMP_PACKED_BITS)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5DUMP-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          filter_fail.out
          filter_fail.out.err
          packedbits.out
          packedbits.out.err
          tall-1.out
          tall-1.out.err
          tall-2.out
          tall-2.out.err
          tall-2A.out
          tall-2A.out.err
          tall-2B.out
          tall-2B.out.err
          tall-3.out
          tall-3.out.err
          tall-4s.out
          tall-4s.out.err
          tall-5s.out
          tall-5s.out.err
          tall-6.out
          tall-6.out.err
          tallfilters.out
          tallfilters.out.err
          tarray1.out
          tarray1.out.err
          tarray1_big.out
          tarray1_big.out.err
          tarray2.out
          tarray2.out.err
          tarray3.out
          tarray3.out.err
          tarray4.out
          tarray4.out.err
          tarray5.out
          tarray5.out.err
          tarray6.out
          tarray6.out.err
          tarray7.out
          tarray7.out.err
          tarray8.out
          tarray8.out.err
          tattr-1.out
          tattr-1.out.err
          tattr-2.out
          tattr-2.out.err
          tattr-3.out
          tattr-3.out.err
          tattrreg.out
          tattrreg.out.err
          tattrregR.out
          tattrregR.out.err
          tbinregR.out
          tbinregR.out.err
          tbigdims.out
          tbigdims.out.err
          tboot1.out
          tboot1.out.err
          tboot2.out
          tboot2.out.err
          tchar1.out
          tchar1.out.err
          tchunked.out
          tchunked.out.err
          tcomp-1.out
          tcomp-1.out.err
          tcomp-2.out
          tcomp-2.out.err
          tcomp-3.out
          tcomp-3.out.err
          tcomp-4.out
          tcomp-4.out.err
          tcompact.out
          tcompact.out.err
          tcontents.out
          tcontents.out.err
          tcontiguos.out
          tcontiguos.out.err
          tdatareg.out
          tdatareg.out.err
          tdataregR.out
          tdataregR.out.err
          tdeflate.out
          tdeflate.out.err
          tdset-1.out
          tdset-1.out.err
          tdset-2.out
          tdset-2.out.err
          tdset-3s.out
          tdset-3s.out.err
          tempty.out
          tempty.out.err
          texternal.out
          texternal.out.err
          textlinksrc.out
          textlinksrc.out.err
          textlinkfar.out
          textlinkfar.out.err
          textlink.out
          textlink.out.err
          tfamily.out
          tfamily.out.err
          tfill.out
          tfill.out.err
          tfletcher32.out
          tfletcher32.out.err
          tfpformat.out
          tfpformat.out.err
          tgroup-1.out
          tgroup-1.out.err
          tgroup-2.out
          tgroup-2.out.err
          tgrp_comments.out
          tgrp_comments.out.err
          thlink-1.out
          thlink-1.out.err
          thlink-2.out
          thlink-2.out.err
          thlink-3.out
          thlink-3.out.err
          thlink-4.out
          thlink-4.out.err
          thlink-5.out
          thlink-5.out.err
          thyperslab.out
          thyperslab.out.err
          tindicesno.out
          tindicesno.out.err
          tindicessub1.out
          tindicessub1.out.err
          tindicessub2.out
          tindicessub2.out.err
          tindicessub3.out
          tindicessub3.out.err
          tindicessub4.out
          tindicessub4.out.err
          texceedsubstart.out
          texceedsubstart.out.err
          texceedsubcount.out
          texceedsubcount.out.err
          texceedsubstride.out
          texceedsubstride.out.err
          texceedsubblock.out
          texceedsubblock.out.err
          tindicesyes.out
          tindicesyes.out.err
          tlarge_objname.out
          tlarge_objname.out.err
          tldouble.out
          tldouble.out.err
          tlonglinks.out
          tlonglinks.out.err
          tloop-1.out
          tloop-1.out.err
          tmulti.out
          tmulti.out.err
          tnamed_dtype_attr.out
          tnamed_dtype_attr.out.err
          tnestcomp-1.out
          tnestcomp-1.out.err
          tnbit.out
          tnbit.out.err
          tnofilename.out
          tnofilename.out.err
          tnullspace.out
          tnullspace.out.err
          tordergr1.out
          tordergr1.out.err
          tordergr2.out
          tordergr2.out.err
          tordergr3.out
          tordergr3.out.err
          tordergr4.out
          tordergr4.out.err
          tordergr5.out
          tordergr5.out.err
          torderattr1.out
          torderattr1.out.err
          torderattr2.out
          torderattr2.out.err
          torderattr3.out
          torderattr3.out.err
          torderattr4.out
          torderattr4.out.err
          tperror.out
          tperror.out.err
          treference.out
          treference.out.err
          tsaf.out
          tsaf.out.err
          tscaleoffset.out
          tscaleoffset.out.err
          tshuffle.out
          tshuffle.out.err
          tslink-1.out
          tslink-1.out.err
          tslink-2.out
          tslink-2.out.err
          tsplit_file.out
          tsplit_file.out.err
          tstr-1.out
          tstr-1.out.err
          tstr-2.out
          tstr-2.out.err
          tstring.out
          tstring.out.err
          tstring2.out
          tstring2.out.err
          tstringe.out
          tstringe.out.err
          tszip.out
          tszip.out.err
          tudlink-1.out
          tudlink-1.out.err
          tudlink-2.out
          tudlink-2.out.err
          tuserfilter.out
          tuserfilter.out.err
          tvldtypes1.out
          tvldtypes1.out.err
          tvldtypes2.out
          tvldtypes2.out.err
          tvldtypes3.out
          tvldtypes3.out.err
          tvldtypes4.out
          tvldtypes4.out.err
          tvldtypes5.out
          tvldtypes5.out.err
          tvlstr.out
          tvlstr.out.err
          tvms.out
          tvms.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5DUMP-clearall-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5DUMP-clearall-objects")

  # test for signed/unsigned datasets
  ADD_H5_TEST (packedbits 0 packedbits.h5)
  # test for displaying groups
  ADD_H5_TEST (tgroup-1 0 tgroup.h5)
  # test for displaying the selected groups
  ADD_H5_TEST (tgroup-2 1 --group=/g2 --group / -g /y tgroup.h5)

  # test for displaying simple space datasets
  ADD_H5_TEST (tdset-1 0 tdset.h5)
  # test for displaying selected datasets
  ADD_H5_TEST (tdset-2 1 -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5)

  # test for displaying attributes
  ADD_H5_TEST (tattr-1 0 tattr.h5)
  # test for displaying the selected attributes of string type and scalar space
  ADD_H5_TEST (tattr-2 0 -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5)
  # test for header and error messages
  ADD_H5_TEST (tattr-3 1 --header -a /attr2 --attribute=/attr tattr.h5)
  # test for displaying attributes in shared datatype (also in group and dataset)
  ADD_H5_TEST (tnamed_dtype_attr 0 tnamed_dtype_attr.h5)

  # test for displaying soft links and user-defined links
  ADD_H5_TEST (tslink-1 0 tslink.h5)
  ADD_H5_TEST (tudlink-1 0 tudlink.h5)
  # test for displaying the selected link
  ADD_H5_TEST (tslink-2 0 -l slink2 tslink.h5)
  ADD_H5_TEST (tudlink-2 0 -l udlink2 tudlink.h5)

  # tests for hard links
  ADD_H5_TEST (thlink-1 0 thlink.h5)
  ADD_H5_TEST (thlink-2 0 -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5)
  ADD_H5_TEST (thlink-3 0 -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5)
  ADD_H5_TEST (thlink-4 0 -g /g1 thlink.h5)
  ADD_H5_TEST (thlink-5 0 -d /dset1 -g /g2 -d /g1/dset2 thlink.h5)

  # tests for compound data types
  ADD_H5_TEST (tcomp-1 0 tcompound.h5)
  # test for named data types
  ADD_H5_TEST (tcomp-2 0 -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5)
  # test for unamed type 
  #ADD_H5_TEST (tcomp-3 0 -t /#6632 -g /group2 tcompound.h5)
  # test complicated compound datatype
  ADD_H5_TEST (tcomp-4 0 tcompound_complex.h5)

  #test for the nested compound type
  ADD_H5_TEST (tnestcomp-1 0 tnestedcomp.h5)

  # test for options
  ADD_H5_TEST (tall-1 0 tall.h5)
  ADD_H5_TEST (tall-2 0 --header -g /g1/g1.1 -a attr2 tall.h5)
  ADD_H5_TEST (tall-3 0 -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5)

  # test for loop detection
  ADD_H5_TEST (tloop-1 0 tloop.h5)

  # test for string 
  ADD_H5_TEST (tstr-1 0 tstr.h5)
  ADD_H5_TEST (tstr-2 0 tstr2.h5)

  # test for file created by Lib SAF team
  ADD_H5_TEST (tsaf 0 tsaf.h5)

  # test for file with variable length data
  ADD_H5_TEST (tvldtypes1 0 tvldtypes1.h5)
  ADD_H5_TEST (tvldtypes2 0 tvldtypes2.h5)
  ADD_H5_TEST (tvldtypes3 0 tvldtypes3.h5)
  ADD_H5_TEST (tvldtypes4 0 tvldtypes4.h5)
  ADD_H5_TEST (tvldtypes5 0 tvldtypes5.h5)

  #test for file with variable length string data
  ADD_H5_TEST (tvlstr 0 tvlstr.h5)

  # test for files with array data
  ADD_H5_TEST (tarray1 0 tarray1.h5)
  # # added for bug# 2092 - tarray1_big.h5
  ADD_H5_TEST (tarray1_big 0 -R tarray1_big.h5)
  ADD_H5_TEST (tarray2 0 tarray2.h5)
  ADD_H5_TEST (tarray3 0 tarray3.h5)
  ADD_H5_TEST (tarray4 0 tarray4.h5)
  ADD_H5_TEST (tarray5 0 tarray5.h5)
  ADD_H5_TEST (tarray6 0 tarray6.h5)
  ADD_H5_TEST (tarray7 0 tarray7.h5)
  ADD_H5_TEST (tarray8 0 tarray8.h5)

  # test for files with empty data
  ADD_H5_TEST (tempty 0 tempty.h5)

  # test for files with groups that have comments
  ADD_H5_TEST (tgrp_comments 0 tgrp_comments.h5)

  # test the --filedriver flag
  ADD_H5_TEST (tsplit_file 0 --filedriver=split tsplit_file)
  ADD_H5_TEST (tfamily 0 --filedriver=family tfamily%05d.h5)
  ADD_H5_TEST (tmulti 0 --filedriver=multi tmulti)

  # test for files with group names which reach > 1024 bytes in size
  ADD_H5_TEST (tlarge_objname 0 -w157 tlarge_objname.h5)

  # test '-A' to suppress data but print attr's
  ADD_H5_TEST (tall-2A 0 -A tall.h5)

  # test '-r' to print attributes in ASCII instead of decimal
  ADD_H5_TEST (tall-2B 0 -A -r tall.h5)

  # test Subsetting
  ADD_H5_TEST (tall-4s 0 --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5)
  ADD_H5_TEST (tall-5s 0 -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5)
  ADD_H5_TEST (tdset-3s 0 -d "/dset1[1,1;;;]" tdset.h5)

  # test printing characters in ASCII instead of decimal
  ADD_H5_TEST (tchar1 0 -r tchar.h5)

  # test failure handling
  IF (NOT HDF5_USE_H5DUMP_PACKED_BITS)
    # Missing file name
    ADD_H5_TEST (tnofilename 1)
  ENDIF (NOT HDF5_USE_H5DUMP_PACKED_BITS)

  # rev. 2004

  # tests for super block
  ADD_H5_TEST (tboot1 0 -H -B -d dset tfcontents1.h5)
  ADD_H5_TEST (tboot2 0 -B tfcontents2.h5)

  # test -p with a non existing dataset
  ADD_H5_TEST (tperror 1 -p -d bogus tfcontents1.h5)

  # test for file contents
  ADD_H5_TEST (tcontents 0 -n tfcontents1.h5)

  # tests for storage layout
  # compact
  ADD_H5_TEST (tcompact 0 -H -p -d compact tfilters.h5)
  # contiguous
  ADD_H5_TEST (tcontiguos 0 -H -p -d contiguous tfilters.h5)
  # chunked
  ADD_H5_TEST (tchunked 0 -H -p -d chunked tfilters.h5)
  # external 
  ADD_H5_TEST (texternal 0 -H -p -d external tfilters.h5)

  # fill values
  ADD_H5_TEST (tfill 0 -p tfvalues.h5)

  # several datatype, with references , print path
  ADD_H5_TEST (treference 0  tattr2.h5)

  # escape/not escape non printable characters
  ADD_H5_TEST (tstringe 0 -e tstr3.h5)
  ADD_H5_TEST (tstring 0 tstr3.h5)
  # char data as ASCII with non escape
  ADD_H5_TEST (tstring2 0 -r -d str4 tstr3.h5)

  # array indices print/not print
  ADD_H5_TEST (tindicesyes 0 taindices.h5)
  ADD_H5_TEST (tindicesno 0 -y taindices.h5)

  ########## array indices with subsetting
  # 1D case
  ADD_H5_TEST (tindicessub1 0 -d 1d -s 1 -S 10 -c 2  -k 3 taindices.h5)

  # 2D case
  ADD_H5_TEST (tindicessub2 0 -d 2d -s 1,2  -S 3,3 -c 3,2 -k 2,2 taindices.h5)

  # 3D case
  ADD_H5_TEST (tindicessub3 0 -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2  -k 1,2,2  taindices.h5)

  # 4D case
  ADD_H5_TEST (tindicessub4 0 -d 4d -s 0,0,1,2  -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2  taindices.h5)

  # Exceed the dimensions for subsetting
  ADD_H5_TEST (texceedsubstart 1 -d 1d -s 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubcount 1 -d 1d -c 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubstride 1 -d 1d -S 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubblock 1 -d 1d -k 1,3 taindices.h5)

  # tests for filters
  # SZIP
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP)
  ADD_SKIP_H5_TEST (tszip 0 ${TESTTYPE} -H -p -d szip tfilters.h5)

  # deflate
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_SKIP_H5_TEST (tdeflate 0 ${TESTTYPE} -H -p -d deflate tfilters.h5)

  # shuffle
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SHUFFLE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SHUFFLE)
  ADD_SKIP_H5_TEST (tshuffle 0 ${TESTTYPE} -H -p -d shuffle tfilters.h5)

  # fletcher32
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_FLETCHER32)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_FLETCHER32)
  ADD_SKIP_H5_TEST (tfletcher32 0 ${TESTTYPE} -H -p -d fletcher32  tfilters.h5)

  # nbit
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_NBIT)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_NBIT)
  ADD_SKIP_H5_TEST (tnbit 0 ${TESTTYPE} -H -p -d nbit  tfilters.h5)

  # scaleoffset
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SCALEOFFSET)
  ADD_SKIP_H5_TEST (tscaleoffset 0 ${TESTTYPE} -H -p -d scaleoffset  tfilters.h5)

  # all
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
  ADD_SKIP_H5_TEST (tallfilters 0 ${TESTTYPE} -H -p -d all  tfilters.h5)

  # user defined
  ADD_H5_TEST (tuserfilter 0 -H  -p -d myfilter  tfilters.h5)

  # test for displaying objects with very long names
  ADD_H5_TEST (tlonglinks 0 tlonglinks.h5)

  # dimensions over 4GB, print boundary 
  ADD_H5_TEST (tbigdims 0 -d dset4gb -s 4294967284 -c 22 tbigdims.h5)

  # hyperslab read
  ADD_H5_TEST (thyperslab 0 thyperslab.h5)
    
  # test for displaying dataset and attribute of null space
  ADD_H5_TEST (tnullspace 0 tnullspace.h5)

  # test for long double (some systems do not have long double)
  #ADD_H5_TEST (tldouble 0 tldouble.h5)

  # test for vms
  ADD_H5_TEST (tvms 0 tvms.h5)

  # test for binary output
  ADD_H5_TEST (tbin1LE 0 -d integer -o out1LE.bin -b LE tbinary.h5)

  # NATIVE default. the NATIVE test can be validated with h5import/h5diff
  ADD_H5_TEST (tbin1 0 -d integer -o out1.bin -b tbinary.h5)
  ADD_TEST (NAME H5DUMP-clear-out1 COMMAND ${CMAKE_COMMAND} -E remove out1.h5)
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5DUMP-clear-out1 PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  ADD_TEST (NAME H5DUMP-h5import-out1 COMMAND h5import out1.bin -c out3.h5import -o out1.h5)
  SET_TESTS_PROPERTIES (H5DUMP-h5import-out1 PROPERTIES DEPENDS H5DUMP-clear-out1)
  ADD_TEST (NAME H5DUMP-h5diff-out1 COMMAND h5diff tbinary.h5 out1.h5 /integer /integer)
  SET_TESTS_PROPERTIES (H5DUMP-h5diff-out1 PROPERTIES DEPENDS H5DUMP-h5import-out1)
  SET (last_test "H5DUMP-h5diff-out1")

  ADD_H5_TEST (tbin2 0 -b BE -d float -o out2.bin tbinary.h5)

  # the NATIVE test can be validated with h5import/h5diff
  ADD_H5_TEST (tbin3 0 -d integer -o out3.bin -b NATIVE tbinary.h5)
  ADD_TEST (NAME H5DUMP-clear-out3 COMMAND ${CMAKE_COMMAND} -E remove out3.h5)
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5DUMP-clear-out3 PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  ADD_TEST (NAME H5DUMP-h5import-out3 COMMAND h5import out3.bin -c out3.h5import -o out3.h5)
  SET_TESTS_PROPERTIES (H5DUMP-h5import-out3 PROPERTIES DEPENDS H5DUMP-clear-out3)
  ADD_TEST (NAME H5DUMP-h5diff-out3 COMMAND h5diff tbinary.h5 out3.h5 /integer /integer -q)
  SET_TESTS_PROPERTIES (H5DUMP-h5diff-out3 PROPERTIES DEPENDS H5DUMP-h5import-out3)
  SET (last_test "H5DUMP-h5diff-out3")

  ADD_H5_TEST (tbin4 0 -d double -b FILE -o out4.bin tbinary.h5)
   
  # Clean up binary output files
  IF (NOT HDF5_NOCLEANUP)
    ADD_TEST (
        NAME H5DUMP-clear-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            out1.bin
            out1LE.bin
            out2.bin
            out3.bin
            out4.bin
            out1.h5
            out3.h5
    )
  ENDIF (NOT HDF5_NOCLEANUP)
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5DUMP-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5DUMP-clear-objects")

  # test for dataset region references 
  ADD_H5_TEST (tdatareg 0 tdatareg.h5)
  ADD_H5_TEST (tdataregR 0 -R tdatareg.h5)
  ADD_H5_TEST (tattrreg 0 tattrreg.h5)
  ADD_H5_TEST (tattrregR 0 -R tattrreg.h5)
  ADD_H5_EXPORT_TEST (tbinregR tdatareg.h5 0 -d /Dataset1 -s 0 -R -y -o)

  # tests for group creation order
  # "1" tracked, "2" name, root tracked
  ADD_H5_TEST (tordergr1 0 --group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5)
  ADD_H5_TEST (tordergr2 0 --group=1 --sort_by=creation_order --sort_order=descending tordergr.h5)
  ADD_H5_TEST (tordergr3 0 -g 2 -q name -z ascending tordergr.h5)
  ADD_H5_TEST (tordergr4 0 -g 2 -q name -z descending tordergr.h5)
  ADD_H5_TEST (tordergr5 0 -q creation_order tordergr.h5)

  # tests for attribute order
  ADD_H5_TEST (torderattr1 0 -H --sort_by=name --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr2 0 -H --sort_by=name --sort_order=descending torderattr.h5)
  ADD_H5_TEST (torderattr3 0 -H --sort_by=creation_order --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr4 0 -H --sort_by=creation_order --sort_order=descending torderattr.h5)

  # tests for floating point user defined printf format
  ADD_H5_TEST (tfpformat 0 -m %.7f tfpformat.h5)

  # tests for traversal of external links
  ADD_H5_TEST (textlinksrc 0 textlinksrc.h5)
  ADD_H5_TEST (textlinkfar 0 textlinkfar.h5)

  # test for dangling external links
  ADD_H5_TEST (textlink 0 textlink.h5)

  # test for error stack display (BZ2048)
  ADD_H5_MASK_TEST (filter_fail 1 --enable-error-stack filter_fail.h5)

  # test for -o -y for dataset with attributes
  ADD_H5_TEST (tall-6 0 -y -o data -d /g1/g1.1/dset1.1.1 tall.h5)

  ####### test for dataset packed bits ######
  IF (HDF5_USE_H5DUMP_PACKED_BITS)
    # Remove any output file left over from previous test run
    ADD_TEST (
        NAME H5DUMP_PACKED_BITS-clearall-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            tnofilename-with-packed-bits.out
            tnofilename-with-packed-bits.out.err
            tpbitsArray.out
            tpbitsArray.out.err
            tpbitsCompound.out
            tpbitsCompound.out.err
            tpbitsIncomplete.out
            tpbitsIncomplete.out.err
            tpbitsLengthExceeded.out
            tpbitsLengthExceeded.out.err
            tpbitsCharLengthExceeded.out
            tpbitsCharLengthExceeded.out.err
            tpbitsIntLengthExceeded.out
            tpbitsIntLengthExceeded.out.err
            tpbitsLongLengthExceeded.out
            tpbitsLongLengthExceeded.out.err
            tpbitsLengthPositive.out
            tpbitsLengthPositive.out.err
            tpbitsMax.out
            tpbitsMax.out.err
            tpbitsMaxExceeded.out
            tpbitsMaxExceeded.out.err
            tpbitsOffsetExceeded.out
            tpbitsOffsetExceeded.out.err
            tpbitsCharOffsetExceeded.out
            tpbitsCharOffsetExceeded.out.err
            tpbitsIntOffsetExceeded.out
            tpbitsIntOffsetExceeded.out.err
            tpbitsLongOffsetExceeded.out
            tpbitsLongOffsetExceeded.out.err
            tpbitsOffsetNegative.out
            tpbitsOffsetNegative.out.err
            tpbitsOverlapped.out
            tpbitsOverlapped.out.err
            tpbitsSigned.out
            tpbitsSigned.out.err
            tpbitsUnsigned.out
            tpbitsUnsigned.out.err
            tpbitsSignedInt.out
            tpbitsSignedInt.out.err
            tpbitsUnsignedInt.out
            tpbitsUnsignedInt.out.err
            tpbitsSignedLong.out
            tpbitsSignedLong.out.err
            tpbitsUnsignedLong.out
            tpbitsUnsignedLong.out.err
            tpbitsSignedLongLong.out
            tpbitsSignedLongLong.out.err
            tpbitsUnsignedLongLong.out
            tpbitsUnsignedLongLong.out.err
            tpbitsSignedWhole.out
            tpbitsSignedWhole.out.err
            tpbitsUnsignedWhole.out
            tpbitsUnsignedWhole.out.err
            tpbitsSignedIntWhole.out
            tpbitsSignedIntWhole.out.err
            tpbitsUnsignedIntWhole.out
            tpbitsUnsignedIntWhole.out.err
            tpbitsSignedLongWhole.out
            tpbitsSignedLongWhole.out.err
            tpbitsUnsignedLongWhole.out
            tpbitsUnsignedLongWhole.out.err
            tpbitsSignedLongLongWhole.out
            tpbitsSignedLongLongWhole.out.err
            tpbitsUnsignedLongLongWhole.out
            tpbitsUnsignedLongLongWhole.out.err
            tpbitsSignedLongLongWhole1.out
            tpbitsSignedLongLongWhole1.out.err
            tpbitsUnsignedLongLongWhole1.out
            tpbitsUnsignedLongLongWhole1.out.err
            tpbitsSignedLongLongWhole63.out
            tpbitsSignedLongLongWhole63.out.err
            tpbitsUnsignedLongLongWhole63.out
            tpbitsUnsignedLongLongWhole63.out.err
            tpbitsSigned4.out
            tpbitsSigned4.out.err
            tpbitsUnsigned4.out
            tpbitsUnsigned4.out.err
            tpbitsSignedInt8.out
            tpbitsSignedInt8.out.err
            tpbitsUnsignedInt8.out
            tpbitsUnsignedInt8.out.err
            tpbitsSignedLong16.out
            tpbitsSignedLong16.out.err
            tpbitsUnsignedLong16.out
            tpbitsUnsignedLong16.out.err
            tpbitsSignedLongLong32.out
            tpbitsSignedLongLong32.out.err
            tpbitsUnsignedLongLong32.out
            tpbitsUnsignedLongLong32.out.err
            tpbitsSigned2.out
            tpbitsSigned2.out.err
            tpbitsUnsigned2.out
            tpbitsUnsigned2.out.err
            tpbitsSignedInt4.out
            tpbitsSignedInt4.out.err
            tpbitsUnsignedInt4.out
            tpbitsUnsignedInt4.out.err
            tpbitsSignedLong8.out
            tpbitsSignedLong8.out.err
            tpbitsUnsignedLong8.out
            tpbitsUnsignedLong8.out.err
            tpbitsSignedLongLong16.out
            tpbitsSignedLongLong16.out.err
            tpbitsUnsignedLongLong16.out
            tpbitsUnsignedLongLong16.out.err
    )
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5DUMP_PACKED_BITS-clearall-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5DUMP_PACKED_BITS-clearall-objects")

    # test failure handling
    # Missing file name
    ADD_H5_TEST (tnofilename-with-packed-bits 1)
    # Limits:
    # Maximum number of packed bits is 8 (for now).
    # Maximum integer size is 8*sizeof(long long).
    # Maximun Offset is Maximum size - 1.
    # Maximum Offset+Length is Maximum size.
    # Tests:
    # Normal operation on both signed and unsigned int datasets.
    # Sanity check
    # Their rawdata output should be the same.
    ADD_H5_TEST (tpbitsSignedWhole 0 -d /DS08BITS -M 0,8 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedWhole 0 -d /DU08BITS -M 0,8 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedIntWhole 0 -d /DS16BITS -M 0,16 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedIntWhole 0 -d /DU16BITS -M 0,16 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLongWhole 0 -d /DS32BITS -M 0,32 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLongWhole 0 -d /DU32BITS -M 0,32 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLongLongWhole 0 -d /DS64BITS -M 0,64 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLongLongWhole 0 -d /DU64BITS -M 0,64 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLongLongWhole63 0 -d /DS64BITS -M 0,63 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLongLongWhole63 0 -d /DU64BITS -M 0,63 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLongLongWhole1 0 -d /DS64BITS -M 1,63 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLongLongWhole1 0 -d /DU64BITS -M 1,63 packedbits.h5)
    # Half sections
    ADD_H5_TEST (tpbitsSigned4 0 -d /DS08BITS -M 0,4,4,4 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsigned4 0 -d /DU08BITS -M 0,4,4,4 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedInt8 0 -d /DS16BITS -M 0,8,8,8 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedInt8 0 -d /DU16BITS -M 0,8,8,8 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLong16 0 -d /DS32BITS -M 0,16,16,16 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLong16 0 -d /DU32BITS -M 0,16,16,16 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLongLong32 0 -d /DS64BITS -M 0,32,32,32 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLongLong32 0 -d /DU64BITS -M 0,32,32,32 packedbits.h5)
    # Quarter sections
    ADD_H5_TEST (tpbitsSigned2 0 -d /DS08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsigned2 0 -d /DU08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedInt4 0 -d /DS16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedInt4 0 -d /DU16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLong8 0 -d /DS32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLong8 0 -d /DU32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLongLong16 0 -d /DS64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLongLong16 0 -d /DU64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5)
    # Begin and End
    ADD_H5_TEST (tpbitsSigned 0 -d /DS08BITS -M 0,2,2,6 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsigned 0 -d /DU08BITS -M 0,2,2,6 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedInt 0 -d /DS16BITS -M 0,2,10,6 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedInt 0 -d /DU16BITS -M 0,2,10,6 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLong 0 -d /DS32BITS -M 0,2,26,6 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLong 0 -d /DU32BITS -M 0,2,26,6 packedbits.h5)
    ADD_H5_TEST (tpbitsSignedLongLong 0 -d /DS64BITS -M 0,2,58,6 packedbits.h5)
    ADD_H5_TEST (tpbitsUnsignedLongLong 0 -d /DU64BITS -M 0,2,58,6 packedbits.h5)
    # Overlapped packed bits.
    ADD_H5_TEST (tpbitsOverlapped 0 -d /DS08BITS -M 0,1,1,1,2,1,0,3 packedbits.h5)
    # Maximum number of packed bits.
    ADD_H5_TEST (tpbitsMax 0 -d /DS08BITS -M 0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5)
    # Compound type.
    ADD_H5_TEST (tpbitsCompound 0 -d /dset1 -M 0,1,1,1 tcompound.h5)
    # Array type.
    ADD_H5_TEST (tpbitsArray 0 -d /Dataset1 -M 0,1,1,1 tarray1.h5)
    # Test Error handling.
    # Too many packed bits requested. Max is 8 for now.
    ADD_H5_TEST (tpbitsMaxExceeded 1 -d /DS08BITS -M 0,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5)
    # Offset too large. Max is 8*sizeof(long long.
    ADD_H5_TEST (tpbitsOffsetExceeded 1 -d /DS08BITS -M 64,1 packedbits.h5)
    ADD_H5_TEST (tpbitsCharOffsetExceeded 0 -d /DS08BITS -M 8,1 packedbits.h5)
    ADD_H5_TEST (tpbitsIntOffsetExceeded 0 -d /DS16BITS -M 16,1 packedbits.h5)
    ADD_H5_TEST (tpbitsLongOffsetExceeded 0 -d /DS32BITS -M 32,1 packedbits.h5)
    # Bad offset, must not be negative.
    ADD_H5_TEST (tpbitsOffsetNegative 1 -d /DS08BITS -M -1,1 packedbits.h5)
    # Bad length, must not be positive.
    ADD_H5_TEST (tpbitsLengthPositive 1 -d /DS08BITS -M 4,0 packedbits.h5)
    # Offset+Length is too large. Max is 8*sizeof(long long).
    ADD_H5_TEST (tpbitsLengthExceeded 1 -d /DS08BITS -M 37,28 packedbits.h5)
    ADD_H5_TEST (tpbitsCharLengthExceeded 0 -d /DS08BITS -M 2,7 packedbits.h5)
    ADD_H5_TEST (tpbitsIntLengthExceeded 0 -d /DS16BITS -M 10,7 packedbits.h5)
    ADD_H5_TEST (tpbitsLongLengthExceeded 0 -d /DS32BITS -M 26,7 packedbits.h5)
    # Incomplete pair of packed bits request.
    ADD_H5_TEST (tpbitsIncomplete 1 -d /DS08BITS -M 0,2,2,1,0,2,2, packedbits.h5)
  ENDIF (HDF5_USE_H5DUMP_PACKED_BITS)
   
  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5DUMP-XML-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          tall.h5.out
          tall.h5.out.err
          tall-2A.h5.out
          tall-2A.h5.out.err
          tarray1.h5.out
          tarray1.h5.out.err
          tarray2.h5.out
          tarray2.h5.out.err
          tarray3.h5.out
          tarray3.h5.out.err
          tarray6.h5.out
          tarray6.h5.out.err
          tarray7.h5.out
          tarray7.h5.out.err
          tattr.h5.out
          tattr.h5.out.err
          tbitfields.h5.out
          tbitfields.h5.out.err
          tcompound.h5.out
          tcompound.h5.out.err
          tcompound2.h5.out
          tcompound2.h5.out.err
          tcompound_complex.h5.out
          tcompound_complex.h5.out.err
          tdatareg.h5.out
          tdatareg.h5.out.err
          tdset.h5.out
          tdset.h5.out.err
          tdset2.h5.out
          tdset2.h5.out.err
          tempty-dtd-2.h5.out
          tempty-dtd-2.h5.out.err
          tempty-dtd-uri.h5.out
          tempty-dtd-uri.h5.out.err
          tempty-dtd.h5.out
          tempty-dtd.h5.out.err
          tempty-nons-2.h5.out
          tempty-nons-2.h5.out.err
          tempty-nons-uri.h5.out
          tempty-nons-uri.h5.out.err
          tempty-nons.h5.out
          tempty-nons.h5.out.err
          tempty-ns-2.h5.out
          tempty-ns-2.h5.out.err
          tempty-ns.h5.out
          tempty-ns.h5.out.err
          tempty.h5.out
          tempty.h5.out.err
          tenum.h5.out
          tenum.h5.out.err
          textlink.h5.out
          textlink.h5.out.err
          tfpformat.h5.out
          tfpformat.h5.out.err
          tgroup.h5.out
          tgroup.h5.out.err
          thlink.h5.out
          thlink.h5.out.err
          tloop.h5.out
          tloop.h5.out.err
          tloop2.h5.out
          tloop2.h5.out.err
          tmany.h5.out
          tmany.h5.out.err
          tname-amp.h5.out
          tname-amp.h5.out.err
          tname-apos.h5.out
          tname-apos.h5.out.err
          tname-gt.h5.out
          tname-gt.h5.out.err
          tname-lt.h5.out
          tname-lt.h5.out.err
          tname-quot.h5.out
          tname-quot.h5.out.err
          tname-sp.h5.out
          tname-sp.h5.out.err
          tnamed_dtype_attr.h5.out
          tnamed_dtype_attr.h5.out.err
          tnestedcomp.h5.out
          tnestedcomp.h5.out.err
          tnodata.h5.out
          tnodata.h5.out.err
          tnoname.h5.out
          tnoname.h5.out.err
          tobjref.h5.out
          tobjref.h5.out.err
          topaque.h5.out
          topaque.h5.out.err
          torderattr1.h5.out
          torderattr1.h5.out.err
          torderattr2.h5.out
          torderattr2.h5.out.err
          torderattr3.h5.out
          torderattr3.h5.out.err
          torderattr4.h5.out
          torderattr4.h5.out.err
          tref-escapes-at.h5.out
          tref-escapes-at.h5.out.err
          tref-escapes.h5.out
          tref-escapes.h5.out.err
          tref.h5.out
          tref.h5.out.err
          tsaf.h5.out
          tsaf.h5.out.err
          tslink.h5.out
          tslink.h5.out.err
          tstr.h5.out
          tstr.h5.out.err
          tstr2.h5.out
          tstr2.h5.out.err
          tstring.h5.out
          tstring.h5.out.err
          tstring-at.h5.out
          tstring-at.h5.out.err
          tudlink.h5.out
          tudlink.h5.out.err
          tvldtypes1.h5.out
          tvldtypes1.h5.out.err
          tvldtypes2.h5.out
          tvldtypes2.h5.out.err
          tvldtypes3.h5.out
          tvldtypes3.h5.out.err
          tvldtypes4.h5.out
          tvldtypes4.h5.out.err
          tvldtypes5.h5.out
          tvldtypes5.h5.out.err
          tvlstr.h5.out
          tvlstr.h5.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5DUMP-XML-clearall-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5DUMP-XML-clearall-objects")

  ########## test XML
  ADD_XML_H5_TEST (tall.h5 0 tall.h5)
  ADD_XML_H5_TEST (tattr.h5 0 tattr.h5)
  ADD_XML_H5_TEST (tbitfields.h5 0 tbitfields.h5)
  ADD_XML_H5_TEST (tcompound.h5 0 tcompound.h5)
  ADD_XML_H5_TEST (tcompound2.h5 0 tcompound2.h5)
  ADD_XML_H5_TEST (tdatareg.h5 0 tdatareg.h5)
  ADD_XML_H5_TEST (tdset.h5 0 tdset.h5)
  ADD_XML_H5_TEST (tdset2.h5 0 tdset2.h5)
  ADD_XML_H5_TEST (tenum.h5 0 tenum.h5)
  ADD_XML_H5_TEST (tgroup.h5 0 tgroup.h5)
  ADD_XML_H5_TEST (thlink.h5 0 thlink.h5)
  ADD_XML_H5_TEST (tloop.h5 0 tloop.h5)
  ADD_XML_H5_TEST (tloop2.h5 0 tloop2.h5)
  ADD_XML_H5_TEST (tmany.h5 0 tmany.h5)
  ADD_XML_H5_TEST (tnestedcomp.h5 0 tnestedcomp.h5)
  ADD_XML_H5_TEST (tcompound_complex.h5 0 tcompound_complex.h5)
  ADD_XML_H5_TEST (tobjref.h5 0 tobjref.h5)
  ADD_XML_H5_TEST (topaque.h5 0 topaque.h5)
  ADD_XML_H5_TEST (tslink.h5 0 tslink.h5)
  ADD_XML_H5_TEST (tudlink.h5 0 tudlink.h5)
  ADD_XML_H5_TEST (textlink.h5 0 textlink.h5)
  ADD_XML_H5_TEST (tstr.h5 0 tstr.h5)
  ADD_XML_H5_TEST (tstr2.h5 0 tstr2.h5)
  ADD_XML_H5_TEST (tref.h5 0 tref.h5)
  ADD_XML_H5_TEST (tname-amp.h5 0 tname-amp.h5)
  ADD_XML_H5_TEST (tname-apos.h5 0 tname-apos.h5)
  ADD_XML_H5_TEST (tname-gt.h5 0 tname-gt.h5)
  ADD_XML_H5_TEST (tname-lt.h5 0 tname-lt.h5)
  ADD_XML_H5_TEST (tname-quot.h5 0 tname-quot.h5)
  ADD_XML_H5_TEST (tname-sp.h5 0 tname-sp.h5)
  ADD_XML_H5_TEST (tstring.h5 0 tstring.h5)
  ADD_XML_H5_TEST (tstring-at.h5 0 tstring-at.h5)
  ADD_XML_H5_TEST (tref-escapes.h5 0 tref-escapes.h5)
  ADD_XML_H5_TEST (tref-escapes-at.h5 0 tref-escapes-at.h5)
  ADD_XML_H5_TEST (tnodata.h5 0 tnodata.h5)
  ADD_XML_H5_TEST (tarray1.h5 0 tarray1.h5)
  ADD_XML_H5_TEST (tarray2.h5 0 tarray2.h5)
  ADD_XML_H5_TEST (tarray3.h5 0 tarray3.h5)
  ADD_XML_H5_TEST (tarray6.h5 0 tarray6.h5)
  ADD_XML_H5_TEST (tarray7.h5 0 tarray7.h5)
  ADD_XML_H5_TEST (tvldtypes1.h5 0 tvldtypes1.h5)
  ADD_XML_H5_TEST (tvldtypes2.h5 0 tvldtypes2.h5)
  ADD_XML_H5_TEST (tvldtypes3.h5 0 tvldtypes3.h5)
  ADD_XML_H5_TEST (tvldtypes4.h5 0 tvldtypes4.h5)
  ADD_XML_H5_TEST (tvldtypes5.h5 0 tvldtypes5.h5)
  ADD_XML_H5_TEST (tvlstr.h5 0 tvlstr.h5)
  ADD_XML_H5_TEST (tsaf.h5 0 tsaf.h5)
  ADD_XML_H5_TEST (tempty.h5 0 tempty.h5)
  ADD_XML_H5_TEST (tnamed_dtype_attr.h5 0 tnamed_dtype_attr.h5)
  ##Test dataset and attribute of null space.  Commented out:
  ## wait until the XML schema is updated for null space. 
  ##  ADD_XML_H5_TEST (tnullspace.h5 0 tnulspace.h5)

  # other options for xml

  ADD_XML_H5_TEST (tempty-dtd.h5 0 --use-dtd tempty.h5)
  ADD_XML_H5_TEST (tempty-dtd-2.h5 0 -u tempty.h5)

  # The lone colon here confuses some systems (Cray X1).  Skip
  # it if configure detects that this is a problem.
  SET (TESTTYPE "TEST")
  IF (NOT "H5_LONE_COLON")
    SET (TESTTYPE "SKIP")
  ENDIF (NOT "H5_LONE_COLON")
  ADD_XML_SKIP_H5_TEST (tempty-nons.h5 0 ${TESTTYPE} -X : tempty.h5)

  ADD_XML_H5_TEST (tempty-nons-2.h5 0 --xml-ns=: tempty.h5)

  ## Some of these combinations are syntactically correct but
  ##  the URLs are dummies 
  ADD_XML_H5_TEST (tempty-ns.h5 0 -X thing: tempty.h5)
  ADD_XML_H5_TEST (tempty-ns-2.h5 0 --xml-ns=thing: tempty.h5)
  ADD_XML_H5_TEST (tempty-nons-uri.h5 0 --xml-ns=: --xml-dtd=http://somewhere.net tempty.h5)
  ADD_XML_H5_TEST (tempty-dtd-uri.h5 0 --use-dtd --xml-dtd=http://somewhere.net tempty.h5)

  ADD_XML_H5_TEST (tall-2A.h5 0 -A tall.h5)


  # tests for attribute order
  ADD_XML_H5_TEST (torderattr1.h5 0 -H --sort_by=name --sort_order=ascending torderattr.h5)
  ADD_XML_H5_TEST (torderattr2.h5 0 -H --sort_by=name --sort_order=descending torderattr.h5)
  ADD_XML_H5_TEST (torderattr3.h5 0 -H --sort_by=creation_order --sort_order=ascending torderattr.h5)
  ADD_XML_H5_TEST (torderattr4.h5 0 -H --sort_by=creation_order --sort_order=descending torderattr.h5)

  # tests for floating point user defined printf format
  ADD_XML_H5_TEST (tfpformat.h5 0 -u -m %.7f tfpformat.h5)
   
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
        h5dump
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
