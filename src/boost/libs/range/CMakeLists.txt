#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    range.hpp
    range
)

# Add a library target to the build system
boost_library_project(
  range
  #  SRCDIRS 
  TESTDIRS  test
  HEADERS ${lib_headers}
  DOCDIRS  doc
  DESCRIPTION  "A new infrastructure for generic algorithms that builds on top of the new iterator concepts."
  MODULARIZED 
  AUTHORS  "Thorsten Ottosen <nesotto -at- cs.auc.dk>"
  #  MAINTAINERS 
)


