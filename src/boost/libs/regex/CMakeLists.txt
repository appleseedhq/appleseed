#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    regex.h
    regex.hpp
    regex_fwd.hpp
    regex
    cregex.hpp
)

# Add a library target to the build system
boost_library_project(
  regex
  SRCDIRS  src
  TESTDIRS  test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "A regular expression library"
  MODULARIZED
  AUTHORS  "John Maddock <john -at- johnmaddock.co.uk>"
  #  MAINTAINERS 
)


