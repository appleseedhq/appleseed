boost_additional_test_dependencies(random BOOST_DEPENDS test)


boost_test_run(random_test ../random_test.cpp)
boost_test_run(random_demo ../random_demo.cpp)
# TODO: These tests were commented out in the Jamfile.v2:
#       [ run libs/random/nondet_random_speed.cpp ]
#       [ run libs/random/random_device.cpp ]
#       [ run libs/random/random_speed.cpp ]
#       [ run libs/random/statistic_tests.cpp ]
