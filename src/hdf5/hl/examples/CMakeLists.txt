cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_EXAMPLES )

SET (HDF5_TEST_FILES
)

FOREACH (h5_file ${HDF5_TEST_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
  #MESSAGE (STATUS " Copying ${h5_file}")
  ADD_CUSTOM_COMMAND (
      TARGET     h5dump
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${h5_file} ${dest}
  )
ENDFOREACH (h5_file ${HDF5_TEST_FILES})

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
    ex_lite1
    ex_lite2
    ex_lite3
    ptExampleFL
    ptExampleVL
    ex_image1
    ex_image2
    ex_table_01
    ex_table_02
    ex_table_03
    ex_table_04
    ex_table_05
    ex_table_06
    ex_table_07
    ex_table_08
    ex_table_09
    ex_table_10
    ex_table_11
    ex_table_12
    ex_ds1
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (hl_ex_${example} ${HDF5_HL_EXAMPLES_SOURCE_DIR}/${example}.c)
  TARGET_NAMING (hl_ex_${example} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (hl_ex_${example} ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})

  IF (BUILD_TESTING)
    ADD_TEST (NAME hl_ex_${example} COMMAND $<TARGET_FILE:hl_ex_${example}>)
  ENDIF (BUILD_TESTING)
ENDFOREACH (example ${examples})

IF (BUILD_TESTING)
  SET (HDF5_TEST_FILES
      image24pixel.txt
      image8.txt
  )

  FOREACH (h5_file ${HDF5_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     hl_ex_ex_ds1
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_TEST_FILES})
ENDIF (BUILD_TESTING)
