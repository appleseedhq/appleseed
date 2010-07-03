#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    filesystem.hpp
    filesystem
)

# Add a library target to the build system
boost_library_project(
  filesystem
  SRCDIRS  src 
  TESTDIRS  test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "Provides portable facilities to query and manipulate paths, files, and directories."
  MODULARIZED
  AUTHORS  "Beman Dawes <bdawes -at- acm.org>"
  #  MAINTAINERS 
)


