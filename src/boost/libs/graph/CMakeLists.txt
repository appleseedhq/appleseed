#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    graph
)

# Add a library target to the build system
boost_library_project(
  graph
  SRCDIRS  src 
  TESTDIRS  test
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "The BGL graph interface and graph components are generic, in the same sense as the the Standard Template Library (STL)."
  MODULARIZED
  AUTHORS  "Jeremy Siek <jeremy.siek -at- gmail.com>"
           "Lie-Quan Lee"
           "Andrew Lumsdaine"
           "Douglas Gregor <doug.gregor -at- gmail.com>"
  #  MAINTAINERS 
)


