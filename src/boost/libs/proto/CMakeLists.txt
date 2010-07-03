#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    proto
)

# Add a library target to the build system
boost_library_project(
  proto
  #  SRCDIRS 
  TESTDIRS test
  HEADERS ${lib_headers}
  DOCDIRS doc
  # DESCRIPTION  "Helps Boost library developers adapt to compiler idiosyncrasies; not intended for library users."
  MODULARIZED 
  # AUTHORS  "John Maddock <john -at- johnmaddock.co.uk>"
  #  MAINTAINERS 
)


