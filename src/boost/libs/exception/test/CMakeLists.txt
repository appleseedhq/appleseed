boost_additional_test_dependencies(exception BOOST_DEPENDS test)

boost_test_run(is_output_streamable_test)
boost_test_run(has_to_string_test)
boost_test_run(to_string_test)
boost_test_run(to_string_stub_test)
boost_test_compile_fail(to_string_fail)

#exception

boost_test_run(cloning_test)
boost_test_run(copy_exception_test)
boost_test_run(unknown_exception_test)
boost_test_run(exception_test)
#boost_test_run(boost_error_info_test)
boost_test_run(enable_error_info_test enable_error_info_test.cpp helper1.cpp)
boost_test_run(throw_exception_test throw_exception_test.cpp helper2.cpp)
boost_test_run(errno_test)
boost_test_run(error_info_test)
boost_test_run(diagnostic_information_test)
boost_test_compile_fail(exception_fail)
boost_test_compile_fail(throw_exception_fail)

# Compile headers tests
set (compile_tests
  diagnostic_information_hpp_test
  error_info_hpp_test
  exception_ptr_hpp_test
  exception_hpp_test
  get_error_info_hpp_test
  info_hpp_test
  info_tuple_hpp_test
  to_string_hpp_test
  to_string_stub_hpp_test
  )

#-- Create a Compile test for each source
foreach(test ${compile_tests})
    boost_test_compile(${test} "${test}.cpp") 
endforeach(test ${compile_tests})

