#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    algorithm
)

# Add a library target to the build system
boost_library_project(
  algorithm
  #  SRCDIRS 
  TESTDIRS  minmax/test string/test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "A library of various algorithms."
  MODULARIZED
  AUTHORS  "Pavol Droba <droba -at- topmail.sk>"
           "Herve Bronnimann <hbr -at- poly.edu>"
  #  MAINTAINERS 
)
