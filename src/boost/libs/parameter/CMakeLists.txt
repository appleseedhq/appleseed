#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    parameter.hpp
    parameter
)

# Add a library target to the build system
boost_library_project(
  parameter
  #  SRCDIRS 
  TESTDIRS  test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "Write functions that accept arguments by name."
  MODULARIZED
  AUTHORS  "David Abrahams <dave -at- boostpro.com>"
           "Daniel Wallin <dalwan01 -at- student.umu.se>"
  #  MAINTAINERS 
)


