##########################################################################
# Boost Build Slave Support                                              #
##########################################################################
# Copyright (C) 2008 Troy D. Straszheim                                  #
#                                                                        #
# Distributed under the Boost Software License, Version 1.0.             #
# See accompanying file LICENSE_1_0.txt or copy at                       #
#   http://www.boost.org/LICENSE_1_0.txt                                 #
##########################################################################
option(BOOST_BUILD_SLAVE "Be a build slave, report build/testing" OFF)

file(TO_NATIVE_PATH "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" BOOST_BUILD_SLAVE_PYTHONPATH)

if(BOOST_BUILD_SLAVE)
  set(BOOST_BUILD_SLAVE_SUBMIT_URL "http://boost:boost@boost.resophonic.com/trac/login/xmlrpc" 
    CACHE STRING "URL to post regression testing results to.")

  file(TO_NATIVE_PATH "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" BOOST_BUILD_SLAVE_PYTHONPATH)

  set(BOOST_BUILD_SLAVE_TIMEOUT 300
    CACHE STRING "Seconds until build slave times out any individual build step")    

  set(BOOST_BUILD_SLAVE_DETAILS_FILE "slave-description.txt"
    CACHE FILEPATH "Path to file, absolute or relative to build directory, containing descriptive text about the build (configuration peculiarities, etc) to be reported to the server")

  set(BOOST_BUILD_SLAVE_CONTACT_INFO "buildmeister@example.com"
    CACHE STRING "Contact information regarding this build")

  set(BOOST_BUILD_SLAVE_HOSTNAME "" 
    CACHE STRING "If set, don't report what python determines to be the FQDN of this host, report this string instead.")

  set(BOOST_BUILD_SLAVE_SLEEP_DURATION "60" 
    CACHE STRING "Number of seconds to sleep between checks for updates from the repository.")
  
endif(BOOST_BUILD_SLAVE)

message(STATUS "Configuring test/compile drivers")
  
if(CMAKE_VERBOSE_MAKEFILE)
  set(BOOST_DRIVER_VERBOSE True)
else(CMAKE_VERBOSE_MAKEFILE)
  set(BOOST_DRIVER_VERBOSE False)
endif(CMAKE_VERBOSE_MAKEFILE)

#
# the programs that do the dirty work.
#
foreach(PYFILE boost_build_slave passthru marshal start finish info post classify)
  configure_file(tools/build/CMake/${PYFILE}.py.in 
    ${BOOST_BUILD_SLAVE_PYTHONPATH}/${PYFILE}.py 
    @ONLY
    )
endforeach()

if(WIN32)
  configure_file(tools/build/CMake/windows_kill.py.in
    ${BOOST_BUILD_SLAVE_PYTHONPATH}/kill_subprocess.py
    COPYONLY
    )
else(WIN32)
  configure_file(tools/build/CMake/unix_kill.py.in
    ${BOOST_BUILD_SLAVE_PYTHONPATH}/kill_subprocess.py
    COPYONLY
    )
endif(WIN32)


#
# the test driver is either marshal or passthru depending on whether
# you're in build slave mode or not.  The compilation/link rules
# aren't modified if you're not in slave mode, BUUUT the tests still 
# need a driver script that knows whether to expect failure or not
# and 'flips' the return status accordingly: thus passthru.py.
#
if(BOOST_BUILD_SLAVE)
  file(TO_NATIVE_PATH ${BOOST_BUILD_SLAVE_PYTHONPATH}/marshal.py BOOST_TEST_DRIVER)

  configure_file(tools/build/CMake/run_continuous_slave.py.in
    ${CMAKE_BINARY_DIR}/run_continuous_slave.py
    @ONLY
    )

else(BOOST_BUILD_SLAVE)
  file(TO_NATIVE_PATH ${BOOST_BUILD_SLAVE_PYTHONPATH}/passthru.py BOOST_TEST_DRIVER)
endif(BOOST_BUILD_SLAVE)

if(BOOST_BUILD_SLAVE)
  #
  #  Redirect various build steps
  # 
  
  set(CMAKE_CXX_COMPILE_OBJECT 
    "\"${PYTHON_EXECUTABLE}\" \"${BOOST_TEST_DRIVER}\" <CMAKE_CURRENT_BINARY_DIR> cxx_compile_object <OBJECT> ${CMAKE_CXX_COMPILE_OBJECT}" )

  set(CMAKE_CXX_CREATE_SHARED_LIBRARY  
    "\"${PYTHON_EXECUTABLE}\" \"${BOOST_TEST_DRIVER}\" <CMAKE_CURRENT_BINARY_DIR> create_shared_library <TARGET> ${CMAKE_CXX_CREATE_SHARED_LIBRARY}") 
    
  set(CMAKE_CXX_CREATE_STATIC_LIBRARY  
    "\"${PYTHON_EXECUTABLE}\" \"${BOOST_TEST_DRIVER}\" <CMAKE_CURRENT_BINARY_DIR> create_static_library <TARGET> ${CMAKE_CXX_CREATE_STATIC_LIBRARY}") 

  set(CMAKE_CXX_LINK_EXECUTABLE  
    "\"${PYTHON_EXECUTABLE}\" \"${BOOST_TEST_DRIVER}\" <CMAKE_CURRENT_BINARY_DIR> link_executable <TARGET> ${CMAKE_CXX_LINK_EXECUTABLE}") 

  #
  #  Custom targets for talking to the server via xmlrpc
  #


  #
  #  Get us a new build id from the server
  #
  file(TO_NATIVE_PATH ${BOOST_BUILD_SLAVE_PYTHONPATH}/start.py 
    BOOST_BUILD_SLAVE_START_PY)
  add_custom_target(slave-start
    COMMAND ${PYTHON_EXECUTABLE} ${BOOST_BUILD_SLAVE_START_PY}
    COMMENT "Slave starting build"
    )

  #
  #  Tell server we're done... it'll update finish time in the db.
  #
  file(TO_NATIVE_PATH ${BOOST_BUILD_SLAVE_PYTHONPATH}/finish.py 
    BOOST_BUILD_SLAVE_FINISH_PY)
  add_custom_target(slave-finish
    COMMAND ${PYTHON_EXECUTABLE} ${BOOST_BUILD_SLAVE_FINISH_PY}
    COMMENT "Slave finishing build"
    )
  #

  #  Local only:  show what we report to server (our platform description, toolset, etc)
  #
  file(TO_NATIVE_PATH ${BOOST_BUILD_SLAVE_PYTHONPATH}/info.py 
    BOOST_BUILD_SLAVE_INFO_PY)
  add_custom_target(slave-info
    COMMAND ${PYTHON_EXECUTABLE} ${BOOST_BUILD_SLAVE_INFO_PY}
    COMMENT "Print slave info"
    )
endif(BOOST_BUILD_SLAVE)

#
#  Used over in BoostTesting and BoostCore to attach xmlrpc submission rules
#  to various intermediate build targets (libraries, test suites) 
#

file(TO_NATIVE_PATH ${BOOST_BUILD_SLAVE_PYTHONPATH}/post.py 
  BOOST_BUILD_SLAVE_POST_PY)
macro(boost_post_results PROJECT_NAME_ PARENT_TARGET BUILD_OR_TEST LOGDIR)
  if(BOOST_BUILD_SLAVE)
    add_custom_command(TARGET ${PARENT_TARGET}
      POST_BUILD
      COMMAND ${PYTHON_EXECUTABLE} ${BOOST_BUILD_SLAVE_POST_PY} ${PROJECT_NAME_} ${PARENT_TARGET} ${BUILD_OR_TEST} ${LOGDIR}
      COMMENT "Submitting results for '${BUILD_OR_TEST}' of ${PARENT_TARGET} in ${PROJECT_NAME_}"
      )
    set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${LOGDIR}/Log.marshal)
    add_dependencies(test ${PARENT_TARGET})
  endif(BOOST_BUILD_SLAVE)
endmacro(boost_post_results PARENT_TARGET)
