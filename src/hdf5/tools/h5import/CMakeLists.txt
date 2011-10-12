cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5IMPORT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5import ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5import.c)
TARGET_NAMING (h5import ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5import  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5import)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5import executables
  # --------------------------------------------------------------------
  ADD_EXECUTABLE (h5importtest ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5importtest.c)
  TARGET_NAMING (h5importtest ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (h5importtest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

  SET (HDF5_REFERENCE_CONF_FILES
      binfp32.conf
      binfp64.conf
      binin8.conf
      binin16.conf
      binin32.conf
      binuin16.conf
      binuin32.conf
      textfp32.conf
      textfp64.conf
      textin8.conf
      textin16.conf
      textin32.conf
      textuin16.conf
      textuin32.conf
      textpfe.conf
      textstr.conf
  )
  SET (HDF5_REFERENCE_TXT_FILES
      fp1.txt
      fp2.txt
      in1.txt
      in16.txt
      in32.txt
      in64.txt
      str.txt
  )
  SET (HDF5_REFERENCE_TEST_FILES
      test1.h5
      test2.h5
      test3.h5
      test4.h5
      test5.h5
      test6.h5
      test7.h5
      test8.h5
      test9.h5
      test10.h5
      test11.h5
      test12.h5
      test13.h5
      test14.h5
      test15.h5
  )

  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  FOREACH (conf_file ${HDF5_REFERENCE_CONF_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${conf_file}")
    #MESSAGE (STATUS " Copying ${conf_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5import
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/testfiles/${conf_file} ${dest}
    )
  ENDFOREACH (conf_file ${HDF5_REFERENCE_CONF_FILES})

  FOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${txt_file}")
    #MESSAGE (STATUS " Copying ${txt_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5import
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/testfiles/${txt_file} ${dest}
    )
  ENDFOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5import
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################
  MACRO (ADD_H5_TEST testname importfile conffile testfile)
    ADD_TEST (
        NAME H5IMPORT-${testname}-clear-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testfile}
            ${testfile}.new
            ${testfile}.new.err
            ${testfile}.out
            ${testfile}.out.err
    )
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5IMPORT-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    ADD_TEST (NAME H5IMPORT-${testname} COMMAND $<TARGET_FILE:h5import> ${importfile} -c testfiles/${conffile} -o ${testfile})
    SET_TESTS_PROPERTIES (H5IMPORT-${testname} PROPERTIES DEPENDS H5IMPORT-${testname}-clear-objects)
    SET (last_test "H5IMPORT-${testname}")

    # If using memchecker add tests without using scripts
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5IMPORT-H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testfile}.new"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5IMPORT-H5DUMP-${testname} PROPERTIES DEPENDS H5IMPORT-${testname})
      ADD_TEST (
          NAME H5IMPORT-H5DUMP_CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=testfiles/${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testfile}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_REFERENCE=${testfile}.new"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5IMPORT-H5DUMP_CMP-${testname} PROPERTIES DEPENDS H5IMPORT-H5DUMP-${testname})
      SET (last_test "H5IMPORT-H5DUMP_CMP-${testname}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST testname resultcode testfile vparam srcname dstname)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5IMPORT-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          binfp64.bin
          binin8.bin
          binin8w.bin
          binin16.bin
          binin32.bin
          binuin16.bin
          binuin32.bin
          txtin16.txt
          txtin32.txt
          ${HDF5_REFERENCE_TEST_FILES}
  )

  ADD_TEST (NAME H5IMPORT-h5importtest COMMAND $<TARGET_FILE:h5importtest>)
  SET_TESTS_PROPERTIES (H5IMPORT-h5importtest PROPERTIES DEPENDS H5IMPORT-clear-objects)
  SET (last_test "H5IMPORT-h5importtest")

  # ----- TESTING "ASCII I32 rank 3 - Output BE "
  ADD_H5_TEST (ASCII_I32 in32.txt textin32.conf test1.h5)

  # ----- TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
  ADD_H5_TEST (ASCII_I16 in16.txt textin16.conf test2.h5)

  # ----- TESTING "ASCII I8 - rank 3 - Output I8 LE-Chunked+Extended+Compressed " 
  ADD_H5_TEST (ASCII_I8 in16.txt textin8.conf test3.h5)

  # ----- TESTING "ASCII UI32 - rank 3 - Output BE" 
  ADD_H5_TEST (ASCII_UI32 in1.txt textuin32.conf test4.h5)

  # ----- TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
  ADD_H5_TEST (ASCII_UI16 in1.txt textuin16.conf test5.h5)

  # ----- TESTING "ASCII F32 - rank 3 - Output LE " 
  ADD_H5_TEST (ASCII_F32 fp1.txt textfp32.conf test6.h5)

  # ----- TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
  ADD_H5_TEST (ASCII_F64 fp2.txt textfp64.conf test7.h5)

  # ----- TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
  ADD_H5_TEST (BINARY_F64 binfp64.bin binfp64.conf test8.h5)

  # ----- TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
  ADD_H5_TEST (BINARY_I16 binin16.bin binin16.conf test9.h5)

  # ----- TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
  ADD_H5_TEST (BINARY_I8 binin8.bin binin8.conf test10.h5)

  # ----- TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
  ADD_H5_TEST (BINARY_I32 binin32.bin binin32.conf test11.h5)

  # ----- TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
  ADD_H5_TEST (BINARY_UI16 binuin16.bin binuin16.conf test12.h5)

  # ----- TESTING "BINARY UI32 - rank 3 - Output LE + CHUNKED " 
  ADD_H5_TEST (BINARY_UI32 binuin32.bin binuin32.conf test13.h5)

  # ----- TESTING "STR" 
  ADD_H5_TEST (STR str.txt textstr.conf test14.h5)

  # ----- TESTING "ASCII F64 - rank 1 - INPUT-CLASS TEXTFPE " 
  ADD_H5_TEST (ASCII_F64_R1 in64.txt textpfe.conf test15.h5)

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
        h5import
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
