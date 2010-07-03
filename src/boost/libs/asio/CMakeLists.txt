#----------------------------------------------------------------------------
# This file was automatically generated from the original CMakeLists.txt file
# Add a variable to hold the headers for the library
set (lib_headers
    asio.hpp
    asio
)

# Add a library target to the build system
boost_library_project(
  asio
  #  SRCDIRS 
  #  TESTDIRS 
  HEADERS ${lib_headers}
  #  DOCDIRS 
  DESCRIPTION  "Portable networking, including sockets, timers, hostname resolution and socket iostreams."
  MODULARIZED
  AUTHORS  "Chris Kohlhoff <chris -at- kohlhoff.com>"
  #  MAINTAINERS 
)


