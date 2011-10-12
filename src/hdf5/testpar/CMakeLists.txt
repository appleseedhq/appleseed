cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TEST_PAR)

INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib )
#-----------------------------------------------------------------------------
# Define Tests
#-----------------------------------------------------------------------------

SET (testphdf5_SRCS
    ${HDF5_TEST_PAR_SOURCE_DIR}/testphdf5.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_dset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_file.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_mdset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_ph5basic.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_coll_chunk.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_span_tree.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_chunk_alloc.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_filter_read.c
)

#-- Adding test for testhdf5
ADD_EXECUTABLE (testphdf5 ${testphdf5_SRCS})
TARGET_NAMING (testphdf5 ${LIB_TYPE})
TARGET_LINK_LIBRARIES (testphdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_TEST (NAME testphdf5 COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:testphdf5>)

MACRO (ADD_H5P_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_PAR_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

  ADD_TEST (NAME ${file} COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:${file}> -p)
ENDMACRO (ADD_H5P_TEST file)

SET (H5P_TESTS
    t_mpi
    t_posix_compliant
    t_cache
    t_pflush1
    t_pflush2
    t_shapesame
)

FOREACH (testp ${H5P_TESTS})
    ADD_H5P_TEST(${testp})
ENDFOREACH (testp ${H5P_TESTS})

SET_TESTS_PROPERTIES(t_pflush2 PROPERTIES DEPENDS t_pflush1)

IF (HDF5_TEST_VFD)

  SET (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )

  SET (H5P_VFD_TESTS
      t_pflush1
      t_pflush2
  )
  
  IF (DIRECT_VFD)
    SET (VFD_LIST ${VFD_LIST} direct)
  ENDIF (DIRECT_VFD)

  MACRO (ADD_VFD_TEST vfdname resultcode)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      FOREACH (test ${H5P_VFD_TESTS})
        ADD_TEST (
          NAME VFD-${vfdname}-${test} 
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${test}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${test}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
        )
      ENDFOREACH (test ${H5P_VFD_TESTS})
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_VFD_TEST)
  
  # Run test with different Virtual File Driver
  FOREACH (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  ENDFOREACH (vfd ${VFD_LIST})

ENDIF (HDF5_TEST_VFD)

