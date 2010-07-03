##########################################################################
# Boost Configuration Support                                            #
##########################################################################
# Copyright (C) 2007 Douglas Gregor <doug.gregor@gmail.com>              #
# Copyright (C) 2007 Troy Straszheim                                     #
#                                                                        #
# Distributed under the Boost Software License, Version 1.0.             #
# See accompanying file LICENSE_1_0.txt or copy at                       #
#   http://www.boost.org/LICENSE_1_0.txt                                 #
##########################################################################
# This module defines several variables that provide information about   #
# the target compiler and platform.                                      #
#                                                                        #
# Variables defined:                                                     #
#                                                                        #
#   BOOST_TOOLSET:                                                       #
#     The Boost toolset name, used by the library version mechanism to   #
#     encode the compiler and version into the name of the               #
#     library. This toolset name will correspond with Boost.Build        #
#     version 2's toolset name, including version number.                #
#                                                                        #
#   MULTI_THREADED_COMPILE_FLAGS:                                        #
#     Compilation flags when building multi-threaded programs.           #
#                                                                        #
#   MULTI_THREADED_LINK_FLAGS:                                           #
#     Linker flags when building multi-threaded programs.                #
##########################################################################
include(CheckCXXSourceCompiles)

#
#  Python interpreter
#
include(FindPythonInterp)
message(STATUS "found python executable  ${PYTHON_EXECUTABLE}")
include(FindPythonLibs)
message(STATUS "found python includes    ${PYTHON_INCLUDE_PATH}")
message(STATUS "found python libs        ${PYTHON_LIBRARIES}")

# Toolset detection.
if (NOT BOOST_TOOLSET)
  set(BOOST_TOOLSET "unknown")
  if (MSVC60)
    set(BOOST_TOOLSET "vc6")
  elseif(MSVC70)
    set(BOOST_TOOLSET "vc7")
  elseif(MSVC71)
    set(BOOST_TOOLSET "vc71")
  elseif(MSVC80)
    set(BOOST_TOOLSET "vc80")
  elseif(MSVC90)
    set(BOOST_TOOLSET "vc90")
  elseif(MSVC)
    set(BOOST_TOOLSET "vc")
  elseif(BORLAND)
    set(BOOST_TOOLSET "bcb")
  elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
    # Execute GCC with the -dumpversion option, to give us a version string
    execute_process(
      COMMAND ${CMAKE_CXX_COMPILER} "-dumpversion" 
      OUTPUT_VARIABLE GCC_VERSION_STRING)
    
    # Match only the major and minor versions of the version string
    string(REGEX MATCH "[0-9]+.[0-9]+" GCC_MAJOR_MINOR_VERSION_STRING
      "${GCC_VERSION_STRING}")
    
    # Strip out the period between the major and minor versions
    string(REGEX REPLACE "\\." "" BOOST_VERSIONING_GCC_VERSION
      "${GCC_MAJOR_MINOR_VERSION_STRING}")
    
    # Set the GCC versioning toolset
    set(BOOST_TOOLSET "gcc${BOOST_VERSIONING_GCC_VERSION}")
  endif(MSVC60)
endif (NOT BOOST_TOOLSET)

# Multi-threading support
if(CMAKE_SYSTEM_NAME STREQUAL "SunOS")
  set(MULTI_THREADED_COMPILE_FLAGS "-pthreads")
  set(MULTI_THREADED_LINK_LIBS rt)
elseif(CMAKE_SYSTEM_NAME STREQUAL "BeOS")
  # No threading options necessary for BeOS
elseif(CMAKE_SYSTEM_NAME MATCHES ".*BSD")
  set(MULTI_THREADED_COMPILE_FLAGS "-pthread")
  set(MULTI_THREADED_LINK_LIBS pthread)
elseif(CMAKE_SYSTEM_NAME STREQUAL "DragonFly")
  # DragonFly is  FreeBSD bariant
  set(MULTI_THREADED_COMPILE_FLAGS "-pthread")
elseif(CMAKE_SYSTEM_NAME STREQUAL "IRIX")
  # TODO: GCC on Irix doesn't support multi-threading?
elseif(CMAKE_SYSTEM_NAME STREQUAL "HP-UX")
  # TODO: gcc on HP-UX does not support multi-threading?
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  # No threading options necessary for Mac OS X
elseif(UNIX)
  # Assume -pthread and -lrt on all other variants
  set(MULTI_THREADED_COMPILE_FLAGS "-pthread -D_REENTRANT")
  set(MULTI_THREADED_LINK_FLAGS "")  
  set(MULTI_THREADED_LINK_LIBS pthread rt)
endif(CMAKE_SYSTEM_NAME STREQUAL "SunOS")

# Setup DEBUG_COMPILE_FLAGS, RELEASE_COMPILE_FLAGS, DEBUG_LINK_FLAGS and
# and RELEASE_LINK_FLAGS based on the CMake equivalents
if(CMAKE_CXX_FLAGS_DEBUG)
  if(MSVC)
    # Eliminate the /MDd flag; we'll add it back when we need it
    string(REPLACE "/MDd" "" CMAKE_CXX_FLAGS_DEBUG 
           "${CMAKE_CXX_FLAGS_DEBUG}") 
  endif(MSVC)
  set(DEBUG_COMPILE_FLAGS "${CMAKE_CXX_FLAGS_DEBUG}" CACHE STRING "Compilation flags for debug libraries")
endif(CMAKE_CXX_FLAGS_DEBUG)
if(CMAKE_CXX_FLAGS_RELEASE)
  if(MSVC)
    # Eliminate the /MD flag; we'll add it back when we need it
    string(REPLACE "/MD" "" CMAKE_CXX_FLAGS_RELEASE
           "${CMAKE_CXX_FLAGS_RELEASE}") 
  endif(MSVC)
  set(RELEASE_COMPILE_FLAGS "${CMAKE_CXX_FLAGS_RELEASE}" CACHE STRING "Compilation flags for release libraries")
endif(CMAKE_CXX_FLAGS_RELEASE)
if(CMAKE_SHARED_LINKER_FLAGS_DEBUG)
  set(DEBUG_LINK_FLAGS "${CMAKE_SHARED_LINKER_FLAGS_DEBUG}" CACHE STRING "Linker flags for debug libraries")
endif(CMAKE_SHARED_LINKER_FLAGS_DEBUG)
if(CMAKE_SHARED_LINKER_FLAGS_RELEASE)
  set(RELEASE_LINK_FLAGS "${CMAKE_SHARED_LINKER_FLAGS_RELEASE}" CACHE STRING "Link flags for release libraries")
endif(CMAKE_SHARED_LINKER_FLAGS_RELEASE)

# Set DEBUG_EXE_LINK_FLAGS, RELEASE_EXE_LINK_FLAGS
if (CMAKE_EXE_LINKER_FLAGS_DEBUG)
  set(DEBUG_EXE_LINK_FLAGS "${CMAKE_EXE_LINKER_FLAGS_DEBUG}")
endif (CMAKE_EXE_LINKER_FLAGS_DEBUG)
if (CMAKE_EXE_LINKER_FLAGS_RELEASE)
  set(RELEASE_EXE_LINK_FLAGS "${CMAKE_EXE_LINKER_FLAGS_RELEASE}")
endif (CMAKE_EXE_LINKER_FLAGS_RELEASE)

# Tweak the configuration and build types appropriately.
if(CMAKE_CONFIGURATION_TYPES)
  # Limit CMAKE_CONFIGURATION_TYPES to Debug and Release
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Semicolon-separate list of supported configuration types" FORCE)
else(CMAKE_CONFIGURATION_TYPES)
  # Build in release mode by default
  if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are Release or Debug" FORCE)
  endif (NOT CMAKE_BUILD_TYPE)
endif(CMAKE_CONFIGURATION_TYPES)

# Clear out the built-in C++ compiler and link flags for each of the 
# configurations.
set(CMAKE_CXX_FLAGS_DEBUG "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_SHARED_LINKER_FLAGS_DEBUG "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_MODULE_LINKER_FLAGS_DEBUG "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_EXE_LINKER_FLAGS_DEBUG "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_CXX_FLAGS_RELEASE "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_MODULE_LINKER_FLAGS_RELEASE "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_EXE_LINKER_FLAGS_RELEASE "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_CXX_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_SHARED_LINKER_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_MODULE_LINKER_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_EXE_LINKER_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_MODULE_LINKER_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused by Boost")
set(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused by Boost")
