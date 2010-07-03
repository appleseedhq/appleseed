#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    tuple
)

# Add a library target to the build system
boost_library_project(
  tuple
  #  SRCDIRS 
  TESTDIRS  test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "Ease definition of functions returning multiple values, and more."
  MODULARIZED 
  AUTHORS  "Jaakko Jarvi <jarvi -at- cs.tamu.edu>"
  #  MAINTAINERS 
)


