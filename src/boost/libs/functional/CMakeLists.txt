#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    functional.hpp
    functional
)

# Add a library target to the build system
boost_library_project(
  functional
  #  SRCDIRS 
  TESTDIRS  test hash/test hash/examples
  HEADERS ${lib_headers}
  DOCDIRS  hash/doc
  #  DESCRIPTION 
  MODULARIZED 
  #  AUTHORS 
  #  MAINTAINERS 
)


