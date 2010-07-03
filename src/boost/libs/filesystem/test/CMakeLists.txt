boost_additional_test_dependencies(filesystem BOOST_DEPENDS test bind)


ADD_DEFINITIONS(-DBOOST_ALL_NO_LIB) # -DBOOST_FILESYSTEM_STATIC_LINK)

SET(TESTS path_test operations_test 
  fstream_test convenience_test 
  large_file_support_test wide_test)

FOREACH(test ${TESTS})
  boost_test_run(${test}_static
    ${test}.cpp
    DEPENDS boost_filesystem STATIC
    COMPILE_FLAGS -DBOOST_FILESYSTEM_STATIC_LINK)
  boost_test_run(${test}_dynamic
    ${test}.cpp
    DEPENDS boost_filesystem SHARED
    COMPILE_FLAGS -DBOOST_FILESYSTEM_DYN_LINK)
ENDFOREACH(test ${TESTS})

boost_test_compile(mbcopy ../example/mbcopy.cpp)
boost_test_compile(mbpath ../example/mbpath.cpp)
boost_test_compile(simple_ls ../example/simple_ls.cpp)

