boost_additional_test_dependencies(gil BOOST_DEPENDS test)


boost_test_run(rational_example rational_example.cpp)
boost_test_run(rational_test 
  rational_test.cpp
  DEPENDS boost_unit_test_framework
  )
