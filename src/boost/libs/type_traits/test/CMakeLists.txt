file(GLOB TEST_FILES "*_test.cpp")
foreach(TEST_FILE ${TEST_FILES})
  get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
  boost_test_run(${TEST_NAME})
endforeach(TEST_FILE ${TEST_FILES})

boost_test_run(udt_specialisations)
