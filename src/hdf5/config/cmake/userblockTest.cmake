# runTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.

# arguments checking
IF (NOT TEST_PROGRAM)
  MESSAGE (FATAL_ERROR "Require TEST_PROGRAM tellub to be defined")
ENDIF (NOT TEST_PROGRAM)
IF (NOT TEST_GET_PROGRAM)
  MESSAGE (FATAL_ERROR "Require TEST_GET_PROGRAM getub to be defined")
ENDIF (NOT TEST_GET_PROGRAM)
IF (NOT TEST_FOLDER)
  MESSAGE ( FATAL_ERROR "Require TEST_FOLDER to be defined")
ENDIF (NOT TEST_FOLDER)
IF (NOT TEST_HFILE)
  MESSAGE (FATAL_ERROR "Require TEST_HFILE the hdf file to be defined")
ENDIF (NOT TEST_HFILE)
IF (NOT TEST_UFILE)
  MESSAGE (FATAL_ERROR "Require TEST_UFILE the ub file to be defined")
ENDIF (NOT TEST_UFILE)
IF (NOT TEST_CHECKUB)
  MESSAGE (STATUS "Require TEST_CHECKUB - YES or NO - to be defined")
ENDIF (NOT TEST_CHECKUB)
#IF (NOT TEST_EXPECT)
#  MESSAGE (STATUS "Require TEST_EXPECT to be defined")
#ENDIF (NOT TEST_EXPECT)
#IF (NOT TEST_OFILE)
#  MESSAGE (FATAL_ERROR "Require TEST_OFILE the original hdf file to be defined")
#ENDIF (NOT TEST_OFILE)

SET (TEST_U_STRING_LEN 0)
SET (TEST_O_STRING_LEN 0)
SET (TEST_H_STRING_LEN 0)
SET (TEST_STRING_SIZE 0)

IF (TEST_CHECKUB STREQUAL "YES")
  # find the length of the user block to check
  #s1=`cat $ufile | wc -c | sed -e 's/ //g'`
  FILE (STRINGS ${TEST_FOLDER}/${TEST_UFILE} TEST_U_STRING)
  STRING (LENGTH ${TEST_U_STRING} TEST_U_STRING_LEN)

  # Get the size of the original user block, if any.
  IF (TEST_OFILE)
    # 'tellub' calls H5Fget_user_block to get the size
    #  of the user block
    #s2=`$JAM_BIN/tellub $origfile`
    EXECUTE_PROCESS (
        COMMAND ${TEST_PROGRAM} ${TEST_OFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE ${TEST_HFILE}.len.txt
        OUTPUT_VARIABLE TEST_ERROR
        ERROR_VARIABLE TEST_ERROR
    )
    IF (NOT ${TEST_RESULT} STREQUAL "0")
      MESSAGE (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} ${TEST_OFILE} is: ${TEST_ERROR}")
    ENDIF (NOT ${TEST_RESULT} STREQUAL "0")
    FILE (READ ${TEST_HFILE}.len.txt TEST_O_STRING_LEN)
  ENDIF (TEST_OFILE)
   
  MATH( EXPR TEST_STRING_SIZE "${TEST_U_STRING_LEN} + ${TEST_O_STRING_LEN}" )
 
  IF (NOT TEST_O_STRING_LEN STREQUAL "0")
    #$JAM_BIN/getub -c $s2 $origfile > $cmpfile
    EXECUTE_PROCESS (
        COMMAND ${TEST_GET_PROGRAM} -c ${TEST_O_STRING_LEN} ${TEST_OFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE ${TEST_UFILE}.cmp
        OUTPUT_VARIABLE TEST_ERROR
        ERROR_VARIABLE TEST_ERROR
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    #cat $ufile >> $cmpfile
    FILE (STRINGS ${TEST_UFILE} TEST_STREAM NEWLINE_CONSUME)
    FILE (APPEND ${TEST_UFILE}.cmp "${TEST_STREAM}") 
  ELSE (NOT TEST_O_STRING_LEN STREQUAL "0")
    FILE (STRINGS ${TEST_UFILE} TEST_STREAM NEWLINE_CONSUME)
    FILE (WRITE ${TEST_UFILE}.cmp ${TEST_STREAM})
  ENDIF (NOT TEST_O_STRING_LEN STREQUAL "0")

  #$JAM_BIN/getub -c $size $hfile > $tfile
  EXECUTE_PROCESS (
      COMMAND ${TEST_GET_PROGRAM} -c ${TEST_STRING_SIZE} ${TEST_HFILE}
      WORKING_DIRECTORY ${TEST_FOLDER}
      RESULT_VARIABLE TEST_RESULT
      OUTPUT_FILE ${TEST_HFILE}.cmp
      OUTPUT_VARIABLE TEST_ERROR
      ERROR_VARIABLE TEST_ERROR
      OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  # now compare the outputs
  EXECUTE_PROCESS (
      COMMAND ${CMAKE_COMMAND} -E compare_files ${TEST_UFILE}.cmp ${TEST_HFILE}.cmp
      RESULT_VARIABLE TEST_RESULT
  )

  MESSAGE (STATUS "COMPARE Result: ${TEST_RESULT}: ${TEST_STRING_SIZE}=${TEST_U_STRING_LEN}+${TEST_O_STRING_LEN}")
  # if the return value is !=${TEST_EXPECT} bail out
  IF (NOT ${TEST_RESULT} STREQUAL ${TEST_EXPECT})
    MESSAGE (FATAL_ERROR "Failed: The output of ${TEST_UFILE} did not match ${TEST_HFILE}.\n${TEST_ERROR}")
  ENDIF (NOT ${TEST_RESULT} STREQUAL ${TEST_EXPECT})
ELSE (TEST_CHECKUB STREQUAL "YES")
    # call 'ubsize' to get the size of the user block
    #ubsize=`$JAM_BIN/tellub $hfile`
    EXECUTE_PROCESS (
        COMMAND ${TEST_PROGRAM} ${TEST_HFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_H_STRING_LEN
        OUTPUT_VARIABLE TEST_ERROR
        ERROR_VARIABLE TEST_ERROR
    )
  IF (NOT TEST_H_STRING_LEN STREQUAL "0")
    MESSAGE (FATAL_ERROR "Failed: The output of ${TEST_HFILE} was NOT empty")
  ENDIF (NOT TEST_H_STRING_LEN STREQUAL "0")
ENDIF (TEST_CHECKUB STREQUAL "YES")

# everything went fine...
MESSAGE ("Passed: The output of CHECK matched expectation")

