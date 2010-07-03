#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    bimap.hpp
    bimap
)

# Add a library target to the build system
boost_library_project(
  bimap
  #  SRCDIRS 
  TESTDIRS  test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "Bidirectional map containers"
  MODULARIZED
  AUTHORS  "Matias Capeletto <matias.capeletto -at- gmail.com>"
  #  MAINTAINERS 
)


