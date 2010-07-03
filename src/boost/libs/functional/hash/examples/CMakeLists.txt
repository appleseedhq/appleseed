#-------------------------------------------------------------------------
#-- Needed include directories for the tests
boost_additional_test_dependencies(config BOOST_DEPENDS test)
#-------------------------------------------------------------------------
boost_test_run(books)
boost_test_run(point)
boost_test_run(portable)


