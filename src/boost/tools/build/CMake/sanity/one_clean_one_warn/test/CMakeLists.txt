boost_test_run(times_out) 
boost_test_compile(this_passes times_out.cpp) 
boost_test_compile_fail(this_unexpectedly_passes times_out.cpp) 

