#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    mpi.hpp
    mpi
)

# Add a library target to the build system
boost_library_project(
  mpi
  SRCDIRS  src
  TESTDIRS  test
  HEADERS ${lib_headers}
  DOCDIRS  doc
  #  DESCRIPTION 
  MODULARIZED
  #  AUTHORS 
  #  MAINTAINERS 
)


