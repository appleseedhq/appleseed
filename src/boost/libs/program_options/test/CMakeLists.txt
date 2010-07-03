boost_additional_test_dependencies(program_options BOOST_DEPENDS test)

set(PROGRAM_OPTIONS_LIBRARIES
  boost_program_options
  boost_test_exec_monitor)

macro(program_options_test_run TESTNAME)
  boost_test_run(${TESTNAME} 
    DEPENDS boost_program_options boost_test_exec_monitor STATIC)
  boost_test_run("${TESTNAME}_dll"
    "${TESTNAME}.cpp"
    DEPENDS boost_program_options boost_test_exec_monitor-static SHARED)
endmacro(program_options_test_run)

program_options_test_run(options_description_test) 
program_options_test_run(parsers_test)
program_options_test_run(variable_map_test)
program_options_test_run(cmdline_test)
program_options_test_run(positional_options_test)
program_options_test_run(unicode_test) 
program_options_test_run(winmain) 
