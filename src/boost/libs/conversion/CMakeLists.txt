#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    
)

# Add a library target to the build system
boost_library_project(
  conversion
  #  SRCDIRS 
  TESTDIRS  test
  # HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "Polymorphic and lexical casts"
  # MODULARIZED 
  AUTHORS  "David Abrahams <dave -at- boost-consulting.com>"
           "Kevlin Henney"
  #  MAINTAINERS 
)


