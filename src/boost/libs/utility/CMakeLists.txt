#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    assert.hpp
    call_traits.hpp
    checked_delete.hpp
    compressed_pair.hpp
    current_function.hpp
    operators.hpp
    throw_exception.hpp
    utility.hpp
    utility
)

# Add a library target to the build system
boost_library_project(
  utility
  #  SRCDIRS 
  TESTDIRS  test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "Various small utilities for C++ programming."
  MODULARIZED 
  AUTHORS  "David Abrahams <dave -at- boostpro.com>"
           "Brad King"
           "Douglas Gregor <doug.gregor -at- gmail.com>"
  #  MAINTAINERS 
)


