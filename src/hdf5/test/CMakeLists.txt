cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TEST)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (TEST_LIB_SRCS
    ${HDF5_TEST_SOURCE_DIR}/h5test.c
    ${HDF5_TEST_SOURCE_DIR}/testframe.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
)

SET (TEST_LIB_HEADERS
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
SET (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
CONFIGURE_FILE (${HDF5_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (${HDF5_TEST_LIB_TARGET} ${LIB_TYPE} ${TEST_LIB_SRCS} ${TEST_LIB_HEADERS})
IF (MSVC)
  TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} "ws2_32.lib")
ENDIF (MSVC)
IF (MINGW)
  TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} "wsock32.lib")
ENDIF (MINGW)
TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_TEST_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} ${LIB_TYPE})

# --------------------------------------------------------------------
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
SET (HDF5_TEST_FILES
  tnullspace.h5
)

FOREACH (h5_tfile ${HDF5_TEST_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${h5_tfile}")
  #MESSAGE (STATUS " Copying ${h5_tfile}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_tfile} ${dest}
  )
ENDFOREACH (h5_tfile ${HDF5_TEST_FILES})

# --------------------------------------------------------------------
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
SET (HDF5_REFERENCE_FILES
    err_compat_1
    err_compat_2
    error_test_1
    error_test_2
    links_env.out
)

FOREACH (ref_file ${HDF5_REFERENCE_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${ref_file}")
  #MESSAGE (STATUS " Copying ${h5_file}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${XLATE_UTILITY}
      ARGS       ${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file} ${dest} -l3
  )
ENDFOREACH (ref_file ${HDF5_REFERENCE_FILES})

# --------------------------------------------------------------------
#-- Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
SET (HDF5_REFERENCE_TEST_FILES
    be_data.h5
    be_extlink1.h5
    be_extlink2.h5
    corrupt_stab_msg.h5
    deflate.h5
    family_v16_00000.h5
    family_v16_00001.h5
    family_v16_00002.h5
    family_v16_00003.h5
    fill_old.h5
    fixed_idx.h5
    group_old.h5
    le_data.h5
    le_extlink1.h5
    le_extlink2.h5
    mergemsg.h5
    noencoder.h5
    specmetaread.h5
    tarrold.h5
    tbad_msg_count.h5
    tbogus.h5
    test_filters_be.hdf5
    test_filters_le.hdf5
    th5s.h5
    tlayouto.h5
    tmtimen.h5
    tmtimeo.h5
    vms_data.h5
)

FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  SET (dest "${HDF5_TEST_BINARY_DIR}/${h5_file}")
  #MESSAGE (STATUS " Copying ${h5_file} to ${dest}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TEST_SOURCE_DIR}/${h5_file} ${dest}
  )
ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

SET (testhdf5_SRCS
    ${HDF5_TEST_SOURCE_DIR}/testhdf5.c
    ${HDF5_TEST_SOURCE_DIR}/tarray.c
    ${HDF5_TEST_SOURCE_DIR}/tattr.c
    ${HDF5_TEST_SOURCE_DIR}/tchecksum.c
    ${HDF5_TEST_SOURCE_DIR}/tconfig.c
    ${HDF5_TEST_SOURCE_DIR}/tcoords.c
    ${HDF5_TEST_SOURCE_DIR}/tfile.c
    ${HDF5_TEST_SOURCE_DIR}/tgenprop.c
    ${HDF5_TEST_SOURCE_DIR}/th5o.c
    ${HDF5_TEST_SOURCE_DIR}/th5s.c
    ${HDF5_TEST_SOURCE_DIR}/theap.c
    ${HDF5_TEST_SOURCE_DIR}/tid.c
    ${HDF5_TEST_SOURCE_DIR}/titerate.c
    ${HDF5_TEST_SOURCE_DIR}/tmeta.c
    ${HDF5_TEST_SOURCE_DIR}/tmisc.c
    ${HDF5_TEST_SOURCE_DIR}/trefer.c
    ${HDF5_TEST_SOURCE_DIR}/trefstr.c
    ${HDF5_TEST_SOURCE_DIR}/tselect.c
    ${HDF5_TEST_SOURCE_DIR}/tskiplist.c
    ${HDF5_TEST_SOURCE_DIR}/tsohm.c
    ${HDF5_TEST_SOURCE_DIR}/ttime.c
    ${HDF5_TEST_SOURCE_DIR}/ttst.c
    ${HDF5_TEST_SOURCE_DIR}/tunicode.c
    ${HDF5_TEST_SOURCE_DIR}/tvltypes.c
    ${HDF5_TEST_SOURCE_DIR}/tvlstr.c
)

#-- Adding test for testhdf5
ADD_EXECUTABLE (testhdf5 ${testhdf5_SRCS})
TARGET_NAMING (testhdf5 ${LIB_TYPE})
TARGET_LINK_LIBRARIES (testhdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_TEST (NAME testhdf5 COMMAND $<TARGET_FILE:testhdf5>)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

MACRO (ADD_H5_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

  ADD_TEST (NAME ${file} COMMAND $<TARGET_FILE:${file}>)
ENDMACRO (ADD_H5_TEST file)

# Remove any output file left over from previous test run
ADD_TEST (
    NAME h5test-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        cache_test.h5
        coord.h5
        dt_arith1.h5
        dt_arith2.h5
        dtransform.h5
        dtypes4.h5
        dtypes5.h5
        efc0.h5
        efc1.h5
        efc2.h5
        efc3.h5
        efc4.h5
        efc5.h5
        extlinks16A00000.h5
        extlinks16A00001.h5
        extlinks16A00002.h5
        extlinks16B-b.h5
        extlinks16B-g.h5
        extlinks16B-l.h5
        extlinks16B-r.h5
        extlinks16B-s.h5
        extlinks19B00000.h5
        extlinks19B00001.h5
        extlinks19B00002.h5
        extlinks19B00003.h5
        extlinks19B00004.h5
        extlinks19B00005.h5
        extlinks19B00006.h5
        extlinks19B00007.h5
        extlinks19B00008.h5
        extlinks19B00009.h5
        extlinks19B00010.h5
        extlinks19B00011.h5
        extlinks19B00012.h5
        extlinks19B00013.h5
        extlinks19B00014.h5
        extlinks19B00015.h5
        extlinks19B00016.h5
        extlinks19B00017.h5
        extlinks19B00018.h5
        extlinks19B00019.h5
        extlinks19B00020.h5
        extlinks19B00021.h5
        extlinks19B00022.h5
        extlinks19B00023.h5
        extlinks19B00024.h5
        extlinks19B00025.h5
        extlinks19B00026.h5
        extlinks19B00027.h5
        extlinks19B00028.h5
        fheap.h5
        objcopy_ext.h5
        sys_file1
        tattr.h5
        testmeta.h5
        tfile1.h5
        tfile2.h5
        tfile3.h5
        tfile4.h5
        tfile5.h5
        th5o_file
        th5s1.h5
        th5s2.h5
        th5s3.h5
        tnullspace.h5
        tselect.h5
        tsohm.h5
        tsohm_dst.h5
        tsohm_src.h5
        tstint1.h5
        tstint2.h5
)

SET (H5_TESTS
    accum
    lheap
    ohdr
    stab
    gheap
    #cache
    #cache_api
    pool
    hyperslab
    istore
    bittests
    dt_arith
    dtypes
    dsets
    cmpd_dset
    filter_fail
    extend
    external
    efc
    objcopy
    links
    unlink
    big
    mtime
    fillval
    mount
    flush1
    flush2
    app_ref
    enum
    set_extent
    #ttsafe
    getname
    vfd
    ntypes
    dangle
    dtransform
    reserved
    cross_read
    freespace
    mf
    btree2
    fheap
    #error_test
    #err_compat
    tcheck_version
    testmeta
    #links_env
)

FOREACH (test ${H5_TESTS})
  ADD_H5_TEST(${test})
  SET_TESTS_PROPERTIES(${test} PROPERTIES DEPENDS h5test-clear-objects)
ENDFOREACH (test ${H5_TESTS})

SET_TESTS_PROPERTIES(flush2 PROPERTIES DEPENDS flush1)

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
ADD_EXECUTABLE (cache ${HDF5_TEST_SOURCE_DIR}/cache.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache ${LIB_TYPE})
TARGET_LINK_LIBRARIES (cache ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME cache COMMAND $<TARGET_FILE:cache>)
SET_TESTS_PROPERTIES(cache PROPERTIES DEPENDS h5test-clear-objects)

#-- Adding test for cache_api
ADD_EXECUTABLE (cache_api ${HDF5_TEST_SOURCE_DIR}/cache_api.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache_api ${LIB_TYPE})
TARGET_LINK_LIBRARIES (cache_api ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME cache_api COMMAND $<TARGET_FILE:cache_api>)

#-- Adding test for ttsafe
ADD_EXECUTABLE (ttsafe
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
)
TARGET_NAMING (ttsafe ${LIB_TYPE})
TARGET_LINK_LIBRARIES (ttsafe ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME ttsafe COMMAND $<TARGET_FILE:ttsafe>)

#-- Adding test for err_compat
IF (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  ADD_EXECUTABLE (err_compat ${HDF5_TEST_SOURCE_DIR}/err_compat.c)
  TARGET_NAMING (err_compat ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (err_compat ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

  ADD_TEST (NAME err_compat COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:err_compat>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "TEST_OUTPUT=err_compat.txt"
      -D "TEST_REFERENCE=err_compat_1"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
  )
ENDIF (HDF5_ENABLE_DEPRECATED_SYMBOLS)

#-- Adding test for error_test
ADD_EXECUTABLE (error_test ${HDF5_TEST_SOURCE_DIR}/error_test.c)
TARGET_NAMING (error_test ${LIB_TYPE})
TARGET_LINK_LIBRARIES (error_test ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME error_test COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:error_test>"
    -D "TEST_ARGS:STRING="
    -D "TEST_EXPECT=0"
    -D "TEST_MASK_ERROR=true"
    -D "TEST_OUTPUT=error_test.txt"
    -D "TEST_REFERENCE=error_test_1"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
    -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
)

#-- Adding test for links_env
ADD_EXECUTABLE (links_env ${HDF5_TEST_SOURCE_DIR}/links_env.c)
TARGET_NAMING (links_env ${LIB_TYPE})
TARGET_LINK_LIBRARIES (links_env ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME links_env COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:links_env>"
    -D "TEST_ARGS:STRING="
    -D "TEST_ENV_VAR:STRING=HDF5_EXT_PREFIX"
    -D "TEST_ENV_VALUE:STRING=.:tmp"
    -D "TEST_EXPECT=0"
    -D "TEST_OUTPUT=links_env.txt"
    -D "TEST_REFERENCE=links_env.out"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
    -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
)

IF (HDF5_TEST_VFD)

  SET (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )

  SET (H5_VFD_TESTS
      testhdf5
      accum
      lheap
      ohdr
      stab
      gheap
      cache
      cache_api
      pool
      hyperslab
      istore
      bittests
      dt_arith
      dtypes
      dsets
      cmpd_dset
      filter_fail
      extend
      external
      efc
      objcopy
      links
      unlink
      big
      mtime
      fillval
      mount
      flush1
      flush2
      app_ref
      enum
      set_extent
      ttsafe
      getname
      vfd
      ntypes
      dangle
      dtransform
      reserved
      cross_read
      freespace
      mf
      btree2
      #fheap
      error_test
      err_compat
      tcheck_version
      testmeta
      links_env
)
  
  IF (DIRECT_VFD)
    SET (VFD_LIST ${VFD_LIST} direct)
  ENDIF (DIRECT_VFD)

  MACRO (ADD_VFD_TEST vfdname resultcode)
    FOREACH (test ${H5_VFD_TESTS})
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
    ENDFOREACH (test ${H5_VFD_TESTS})
    IF (HDF5_TEST_FHEAP_VFD)
      ADD_TEST (
        NAME VFD-${vfdname}-fheap 
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:fheap>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=fheap"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
      )
    ENDIF (HDF5_TEST_FHEAP_VFD)
  ENDMACRO (ADD_VFD_TEST)
  
  # Run test with different Virtual File Driver
  FOREACH (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  ENDFOREACH (vfd ${VFD_LIST})

ENDIF (HDF5_TEST_VFD)

##############################################################################
##############################################################################
###           T H E   G E N E R A T O R S                                  ###
##############################################################################
##############################################################################

IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  MACRO (ADD_H5_GENERATOR genfile)
    ADD_EXECUTABLE (${genfile} ${HDF5_TEST_SOURCE_DIR}/${genfile}.c)
    TARGET_NAMING (${genfile} ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (${genfile} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  ENDMACRO (ADD_H5_GENERATOR genfile)

  # generator executables
  SET (H5_GENERATORS
      gen_bad_ohdr
      gen_bogus
      gen_cross
      gen_deflate
      gen_filters
      gen_idx
      gen_new_array
      gen_new_fill
      gen_new_group
      gen_new_mtime
      gen_new_super
      gen_noencoder
      gen_nullspace
      gen_udlinks
      space_overflow
      gen_sizes_lheap
  )

  FOREACH (gen ${H5_GENERATORS})
    ADD_EXECUTABLE (${gen} ${HDF5_TEST_SOURCE_DIR}/${gen}.c)
    TARGET_NAMING (${gen} ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (${gen} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  ENDFOREACH (gen ${H5_GENERATORS})

ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

