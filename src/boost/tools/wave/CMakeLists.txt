boost_add_executable(wave cpp.cpp 
  DEPENDS boost_wave boost_program_options boost_filesystem boost_serialization
  MULTI_THREADED
  )
