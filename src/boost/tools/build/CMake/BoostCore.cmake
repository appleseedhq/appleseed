##########################################################################
# Core Functionality for Boost                                           #
##########################################################################
# Copyright (C) 2007-2008 Douglas Gregor <doug.gregor@gmail.com>         #
# Copyright (C) 2007 Troy Straszheim                                     #
#                                                                        #
# Distributed under the Boost Software License, Version 1.0.             #
# See accompanying file LICENSE_1_0.txt or copy at                       #
#   http://www.boost.org/LICENSE_1_0.txt                                 #
##########################################################################
# Important developer macros in this file:                               #
#                                                                        #
#   boost_library_project: Defines a Boost library project (e.g.,        #
#   Boost.Python).                                                       #
#                                                                        #
#   boost_add_library: Builds library binaries for Boost libraries       #
#   with compiled sources (e.g., boost_filesystem).                      #
#                                                                        #
#   boost_add_executable: Builds executables.                            #
##########################################################################

add_custom_target(modularize)

# Defines a Boost library project (e.g., for Boost.Python). Use as:
#
#   boost_library_project(libname
#                         [SRCDIRS srcdir1 srcdir2 ...] 
#                         [TESTDIRS testdir1 testdir2 ...]
#                         [DEPENDS lib1 lib2 ...]
#                         [DESCRIPTION description]
#                         [AUTHORS author1 author2 ...]
#                         [MAINTAINERS maint1 maint2 ...]
#                         [MODULARIZED])
#
# where libname is the name of the library (e.g., Python, or
# Filesystem), srcdir1, srcdir2, etc, are subdirectories containing
# library sources (for Boost libraries that build actual library
# binaries), and testdir1, testdir2, etc, are subdirectories
# containing regression tests. DEPENDS lists the names of the other
# Boost libraries that this library depends on. If the dependencies
# are not satisfied (e.g., because the library isn't present or its
# build is turned off), this library won't be built. 
#
# DESCRIPTION provides a brief description of the library, which can
# be used to summarize the behavior of the library for a user. AUTHORS
# lists the authors of the library, while MAINTAINERS lists the active
# maintainers. If MAINTAINERS is left empty, it is assumed that the 
# authors are still maintaining the library. Both authors and maintainers
# should have their name followed by their current e-mail address in
# angle brackets, with -at- instead of the at sign, e.g.,
#   Douglas Gregor <doug.gregor -at- gmail.com>
#
# For libraries that build actual library binaries, this macro adds a
# option BUILD_BOOST_LIBNAME (which defaults to ON). When the option
# is ON, this macro will include the source subdirectories, and
# therefore, will build and install the library binary.
#
# For libraries that have regression tests, and when testing is
# enabled globally by the BUILD_TESTING option, this macro also
# defines the TEST_BOOST_LIBNAME option (defaults to ON). When ON, the
# generated makefiles/project files will contain regression tests for
# this library.
#
# Example: 
#   boost_library_project(
#     Thread
#     SRCDIRS src 
#     TESTDIRS test
#     )
macro(boost_library_project LIBNAME)
  parse_arguments(THIS_PROJECT
    "SRCDIRS;TESTDIRS;HEADERS;DOCDIRS;DESCRIPTION;AUTHORS;MAINTAINERS"
    "MODULARIZED"
    ${ARGN}
    )

  # Set THIS_PROJECT_DEPENDS_ALL to the set of all of its
  # dependencies, its dependencies' dependencies, etc., transitively.
  string(TOUPPER "BOOST_${LIBNAME}_DEPENDS" THIS_PROJECT_DEPENDS)
  set(THIS_PROJECT_DEPENDS_ALL ${${THIS_PROJECT_DEPENDS}})
  set(ADDED_DEPS TRUE)
  while (ADDED_DEPS)
    set(ADDED_DEPS FALSE)
    foreach(DEP ${THIS_PROJECT_DEPENDS_ALL})
      string(TOUPPER "BOOST_${DEP}_DEPENDS" DEP_DEPENDS)
      foreach(DEPDEP ${${DEP_DEPENDS}})
        list(FIND THIS_PROJECT_DEPENDS_ALL ${DEPDEP} DEPDEP_INDEX)
        if (DEPDEP_INDEX EQUAL -1)
          list(APPEND THIS_PROJECT_DEPENDS_ALL ${DEPDEP})
          set(ADDED_DEPS TRUE)
        endif()
      endforeach()
    endforeach()
  endwhile()

  set(THIS_PROJECT_OKAY ON)

  if(FALSE)
    # This really isn't the check we want to do, especially when we
    # hit circular dependencies. For now, just enable all libraries to
    # be built all the time, until we can implement proper subsetting
    # behavior at the CMake level.
    set(THIS_PROJECT_FAILED_DEPS "")
    foreach(DEP ${THIS_PROJECT_DEPENDS_ALL})
      string(TOUPPER "BUILD_BOOST_${DEP}" BOOST_LIB_DEP)
      if (NOT ${BOOST_LIB_DEP})
        set(THIS_PROJECT_OKAY OFF)
        set(THIS_PROJECT_FAILED_DEPS "${THIS_PROJECT_FAILED_DEPS}  ${DEP}\n")
      endif (NOT ${BOOST_LIB_DEP})
    endforeach(DEP)
  endif(FALSE)

  if (THIS_PROJECT_SRCDIRS)
    # This Boost library has source directories, so provide an option
    # BUILD_BOOST_LIBNAME that allows one to turn on/off building of
    # the library.
    if (NOT THIS_PROJECT_OKAY)
      if (${BOOST_BUILD_LIB_OPTION})
        # The user explicitly turned on this library in a prior
        # iteration, but it can no longer be built because one of the
        # dependencies was turned off. Force this option off and
        # complain about it.
        set(${BOOST_BUILD_LIB_OPTION} OFF
          CACHE BOOL "Build Boost.${LIBNAME} (prefer make targets, not this, to build individual libs)" FORCE)
        message(SEND_ERROR 
      "Cannot build Boost.${LIBNAME} due to missing library dependencies:\n${THIS_PROJECT_FAILED_DEPS}")
      endif (${BOOST_BUILD_LIB_OPTION})
    endif (NOT THIS_PROJECT_OKAY)
  endif (THIS_PROJECT_SRCDIRS)

  if(THIS_PROJECT_OKAY)
    string(TOLOWER "${LIBNAME}" libname)
    string(TOUPPER "${LIBNAME}" ULIBNAME)
    project(${LIBNAME})
    
    if (THIS_PROJECT_MODULARIZED OR THIS_PROJECT_SRCDIRS)
      # We only build a component group for modularized libraries or libraries
      # that have compiled parts.
      if (COMMAND cpack_add_component_group)
        # Compute a reasonable description for this library.
        if (THIS_PROJECT_DESCRIPTION)
          set(THIS_PROJECT_DESCRIPTION "Boost.${LIBNAME}\n\n${THIS_PROJECT_DESCRIPTION}")
         
          if (THIS_PROJECT_AUTHORS)
            list(LENGTH THIS_PROJECT_AUTHORS THIS_PROJECT_NUM_AUTHORS)
            if (THIS_PROJECT_NUM_AUTHORS EQUAL 1)
              set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}\n\nAuthor: ")
            else()
              set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}\n\nAuthors: ")
            endif()
            set(THIS_PROJECT_FIRST_AUTHOR TRUE)
            foreach(AUTHOR ${THIS_PROJECT_AUTHORS})
              string(REGEX REPLACE " *-at- *" "@" AUTHOR ${AUTHOR})
              if (THIS_PROJECT_FIRST_AUTHOR)
                set(THIS_PROJECT_FIRST_AUTHOR FALSE)
              else()
                set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}\n         ")
              endif()
              set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}${AUTHOR}")
            endforeach(AUTHOR)
          endif (THIS_PROJECT_AUTHORS)

          if (THIS_PROJECT_MAINTAINERS)
            list(LENGTH THIS_PROJECT_MAINTAINERS THIS_PROJECT_NUM_MAINTAINERS)
            if (THIS_PROJECT_NUM_MAINTAINERS EQUAL 1)
              set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}\nMaintainer: ")
            else()
              set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}\nMaintainers: ")
            endif()
            set(THIS_PROJECT_FIRST_MAINTAINER TRUE)
            foreach(MAINTAINER ${THIS_PROJECT_MAINTAINERS})
              string(REGEX REPLACE " *-at- *" "@" MAINTAINER ${MAINTAINER})
              if (THIS_PROJECT_FIRST_MAINTAINER)
                set(THIS_PROJECT_FIRST_MAINTAINER FALSE)
              else()
                set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}\n             ")
              endif()
              set(THIS_PROJECT_DESCRIPTION "${THIS_PROJECT_DESCRIPTION}${MAINTAINER}")
            endforeach(MAINTAINER)
          endif (THIS_PROJECT_MAINTAINERS)
        endif (THIS_PROJECT_DESCRIPTION)
      
        # Create a component group for this library
        cpack_add_component_group(${libname}
          DISPLAY_NAME "${LIBNAME}"
          DESCRIPTION ${THIS_PROJECT_DESCRIPTION})
      endif ()
    endif ()
        
    if (THIS_PROJECT_MODULARIZED)
      # Add this module's include directory
      include_directories("${Boost_SOURCE_DIR}/libs/${libname}/include")
     
      # Install this module's headers
      install(DIRECTORY include/boost 
        DESTINATION ${BOOST_HEADER_DIR}
        COMPONENT ${libname}_headers
        PATTERN "CVS" EXCLUDE
        PATTERN ".svn" EXCLUDE)
        
      if (COMMAND cpack_add_component)        
        # Determine the header dependencies
        set(THIS_PROJECT_HEADER_DEPENDS)
        foreach(DEP ${${THIS_PROJECT_DEPENDS}})
          string(TOLOWER ${DEP} dep)
          if (${dep} STREQUAL "serialization")
            # TODO: Ugly, ugly hack until the serialization library is modularized
          elseif (${dep} STREQUAL "thread")
          else()
            list(APPEND THIS_PROJECT_HEADER_DEPENDS ${dep}_headers)
          endif()
        endforeach(DEP)

        # Tell CPack about the headers component
        cpack_add_component(${libname}_headers
          DISPLAY_NAME "Header files"
          GROUP      ${libname}
          DEPENDS    ${THIS_PROJECT_HEADER_DEPENDS})
      endif ()
    endif ()

#-- This is here to debug the modularize code
    set(modularize_debug FALSE)
    if (modularize_debug)
      set(modularize_output ${Boost_BINARY_DIR})
      set(modularize_libs_dir "modularize")
    else (modularize_debug)
      set(modularize_output ${Boost_SOURCE_DIR})
      set(modularize_libs_dir "libs")
    endif(modularize_debug)
      # Modularization code
    if(THIS_PROJECT_HEADERS)
      set(${LIBNAME}-modularize-commands)
      foreach(item ${THIS_PROJECT_HEADERS})
        if(EXISTS "${Boost_SOURCE_DIR}/boost/${item}")
          if(IS_DIRECTORY "${Boost_SOURCE_DIR}/boost/${item}")
            list(APPEND ${LIBNAME}-modularize-commands
              COMMAND "${CMAKE_COMMAND}" -E copy_directory
              "${Boost_SOURCE_DIR}/boost/${item}"
              "${modularize_output}/${modularize_libs_dir}/${libname}/include/boost/${item}"
              )
            if (NOT modularize_debug)
                list(APPEND ${LIBNAME}-modularize-commands
                     COMMAND "${CMAKE_COMMAND}" -E remove_directory "${Boost_SOURCE_DIR}/boost/${item}" 
                    )
            endif (NOT modularize_debug)
          else(IS_DIRECTORY "${Boost_SOURCE_DIR}/boost/${item}")
            list(APPEND ${LIBNAME}-modularize-commands
              COMMAND "${CMAKE_COMMAND}" -E copy
              "${Boost_SOURCE_DIR}/boost/${item}"
              "${modularize_output}/${modularize_libs_dir}/${libname}/include/boost/${item}"
              )
            if (NOT modularize_debug)
                list(APPEND ${LIBNAME}-modularize-commands
                     COMMAND "${CMAKE_COMMAND}" -E remove "${Boost_SOURCE_DIR}/boost/${item}" 
                    )
            endif (NOT modularize_debug)
            
          endif(IS_DIRECTORY "${Boost_SOURCE_DIR}/boost/${item}")
        elseif(EXISTS "${Boost_SOURCE_DIR}/${modularize_libs_dir}/${libname}/include/boost/${item}")
          # Okay; already modularized
        else()
          message(SEND_ERROR 
            "Header or directory boost/${item} does not exist. The HEADERS argument in ${Boost_SOURCE_DIR}/${modularize_libs_dir}/${libname}/CMakeLists.txt should be updated.")
        endif()
      endforeach(item)

      if (${LIBNAME}-modularize-commands)
        set(${LIBNAME}-modularize-commands
         # COMMAND "${CMAKE_COMMAND}" -E remove_directory "${modularize_output}/libs/${libname}/include"
          COMMAND "${CMAKE_COMMAND}" -E make_directory
          "${modularize_output}/${modularize_libs_dir}/${libname}/include/boost"
          ${${LIBNAME}-modularize-commands}
          )
        if (NOT modularize_debug)
          set(${LIBNAME}-modularize-commands
            COMMAND "${CMAKE_COMMAND}" -E remove_directory "${modularize_output}/${modularize_libs_dir}/${libname}/include"
            ${${LIBNAME}-modularize-commands}
          )
        endif (NOT modularize_debug)
        add_custom_target(${LIBNAME}-modularize
          ${${LIBNAME}-modularize-commands}
          COMMENT "Modularizing ${LIBNAME} headers to project-local dir from monolithic boost dir"
      )

        if(THIS_PROJECT_MODULARIZED)
          add_dependencies(modularize ${LIBNAME}-modularize)
        endif(THIS_PROJECT_MODULARIZED)
      endif()
    endif(THIS_PROJECT_HEADERS)
    
    # For each of the modular libraries on which this project depends,
    # add the include path for that library.
    set(THIS_PROJECT_HAS_HEADER_DEPENDS FALSE)
    foreach(DEP ${THIS_PROJECT_DEPENDS_ALL})
      include_directories("${modularize_output}/${modularize_libs_dir}/${DEP}/include")
    endforeach(DEP)

    # TODO: is this still necessary?
    if(NOT EXISTS ${CMAKE_BINARY_DIR}/bin/tests)
      file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/bin/tests)
    endif(NOT EXISTS ${CMAKE_BINARY_DIR}/bin/tests)
    if(NOT EXISTS ${CMAKE_BINARY_DIR}/bin/tests/${PROJECT_NAME})
      file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/bin/tests/${PROJECT_NAME})
    endif(NOT EXISTS ${CMAKE_BINARY_DIR}/bin/tests/${PROJECT_NAME})

    # Include each of the source directories
    if(THIS_PROJECT_SRCDIRS)
      foreach(SUBDIR ${THIS_PROJECT_SRCDIRS})
        add_subdirectory(${SUBDIR})
      endforeach(SUBDIR ${THIS_PROJECT_SRCDIRS})
    endif()

    if(BUILD_TESTING AND THIS_PROJECT_TESTDIRS)
      # Testing is enabled globally and this project has some
      # tests. So, include the tests
      add_custom_target(${PROJECT_NAME}-test)

      add_dependencies(test ${PROJECT_NAME}-test)

      # the last argument here, the binary directory that the 
      # logs are in, has to match the binary directory
      # passed to 'add_subdirectory', in the foreach() just below
      boost_post_results(${PROJECT_NAME} ${PROJECT_NAME}-test
                         test
                         ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-test
                         )

      foreach(SUBDIR ${THIS_PROJECT_TESTDIRS})
        add_subdirectory(${SUBDIR} ${PROJECT_NAME}-test)
      endforeach()
    endif(BUILD_TESTING AND THIS_PROJECT_TESTDIRS)

    if (BUILD_DOCUMENTATION AND THIS_PROJECT_DOCDIRS)
      foreach(SUBDIR ${THIS_PROJECT_DOCDIRS})
        add_subdirectory(${SUBDIR})
      endforeach(SUBDIR)
    endif ()
  endif()
endmacro(boost_library_project)

macro(boost_tool_project TOOLNAME)
  parse_arguments(THIS_PROJECT
    "DESCRIPTION;AUTHORS;MAINTAINERS"
    ""
    ${ARGN}
    )

  set(THIS_PROJECT_IS_TOOL TRUE)

  string(TOUPPER ${TOOLNAME} UTOOLNAME)
  set(THIS_PROJECT_OKAY ON)
  set(THIS_PROJECT_FAILED_DEPS "")
  foreach(DEP ${BOOST_${UTOOLNAME}_DEPENDS})
    string(TOUPPER "BUILD_BOOST_${DEP}" BOOST_LIB_DEP)
    if (NOT ${BOOST_LIB_DEP})
      set(THIS_PROJECT_OKAY OFF)
      set(THIS_PROJECT_FAILED_DEPS "${THIS_PROJECT_FAILED_DEPS}  ${DEP}\n")
    endif (NOT ${BOOST_LIB_DEP})
  endforeach(DEP)

  option(BUILD_${UTOOLNAME} "Build ${TOOLNAME}" ON)

  if (NOT THIS_PROJECT_OKAY)
    if (BUILD_${UTOOLNAME})
      # The user explicitly turned on this tool in a prior
      # iteration, but it can no longer be built because one of the
      # dependencies was turned off. Force this option off and
      # complain about it.
      set(BUILD_${UTOOLNAME} OFF CACHE BOOL "Build ${TOOLNAME}" FORCE)
      message(SEND_ERROR "Cannot build ${TOOLNAME} due to missing library dependencies:\n${THIS_PROJECT_FAILED_DEPS}")
    endif ()
  endif (NOT THIS_PROJECT_OKAY)

  if(BUILD_${UTOOLNAME} AND THIS_PROJECT_OKAY)
    string(TOLOWER "${TOOLNAME}" toolname)
    project(${TOOLNAME})
    
    # Add this module's include directory
    include_directories("${Boost_SOURCE_DIR}/libs/${toolname}/include")

    # For each of the modular libraries on which this project depends,
    # add the include path for that library.
    foreach(DEP ${BOOST_${UTOOLNAME}_DEPENDS})
      string(TOUPPER ${DEP} UDEP)
      include_directories("${Boost_SOURCE_DIR}/libs/${DEP}/include")
    endforeach(DEP)
  endif()
endmacro(boost_tool_project)

#TODO: Finish this documentation
# Defines dependencies of a boost project and testing targets. Use as:
#
#   boost_module(libname
#                DEPENDS srcdir1 srcdir2 ...
#                TEST_DEPENDS testdir1 testdir2 ...
#
# Example: 
#   boost_library_project(
#     Thread
#     SRCDIRS src 
#     TESTDIRS test
#     )
macro(boost_module LIBNAME)
  parse_arguments(THIS_MODULE
    "DEPENDS"
    ""
    ${ARGN}
    )

  # Export BOOST_${LIBNAME}_DEPENDS
  string(TOUPPER "BOOST_${LIBNAME}_DEPENDS" THIS_MODULE_LIBNAME_DEPENDS)
  set(${THIS_MODULE_LIBNAME_DEPENDS} ${THIS_MODULE_DEPENDS})
  #
  #message(STATUS "----------------------------------------------------------------")
  #message(STATUS "LIBNAME: ${LIBNAME}")
  #message(STATUS "THIS_MODULE_DEPENDS: ${THIS_MODULE_DEPENDS}")
  #message(STATUS "THIS_MODULE_LIBNAME_DEPENDS: ${THIS_MODULE_LIBNAME_DEPENDS}")
  #message(STATUS "${THIS_MODULE_LIBNAME_DEPENDS}: ${${THIS_MODULE_LIBNAME_DEPENDS}}")
  #message(STATUS "THIS_MODULE_TEST_DEPENDS: ${THIS_MODULE_TEST_DEPENDS}")
  #message(STATUS "THIS_MODULE_LIBNAME_TEST_DEPENDS: ${THIS_MODULE_LIBNAME_TEST_DEPENDS}")
  #message(STATUS "${THIS_MODULE_LIBNAME_TEST_DEPENDS}: ${${THIS_MODULE_LIBNAME_TEST_DEPENDS}}")
endmacro(boost_module)

# This macro is an internal utility macro that builds the name of a
# particular variant of a library
#
#   boost_library_variant_target_name(feature1 feature2 ...)
#
# where feature1, feature2, etc. are the names of features to be
# included in this variant, e.g., MULTI_THREADED, DEBUG. 
#
# This macro sets three macros:
#   
#   VARIANT_TARGET_NAME: The suffix that should be appended to the
#   name of the library target to name this variant of the
#   library. For example, this might be "-mt-static" for a static,
#   multi-threaded variant. It should be used to name the CMake
#   library target, e.g., boost_signals-mt-static.
#
#   VARIANT_VERSIONED_NAME: The suffix that will be added to the name
#   of the generated library, containing information about the
#   particular version of the library and the toolset used to build
#   this library. For example, this might be "-gcc41-mt-1_34" for the
#   multi-threaded, release variant of the library in Boost 1.34.0 as
#   compiled with GCC 4.1.  If option BUILD_VERSIONED is OFF, this 
#   variable is set to the empty string.
#
#   VARIANT_DISPLAY_NAME: The display name that describes this
#   variant, e.g., "Debug, static, multi-threaded".
#
macro(boost_library_variant_target_name)
  set(VARIANT_TARGET_NAME "")

  # The versioned name starts with the full Boost toolset
  if(BUILD_VERSIONED)
    set(VARIANT_VERSIONED_NAME "-${BOOST_TOOLSET}")
  else(BUILD_VERSIONED)
    set(VARIANT_VERSIONED_NAME "")
  endif(BUILD_VERSIONED)

  # Add -mt for multi-threaded libraries
  list_contains(VARIANT_IS_MT MULTI_THREADED ${ARGN})
  if (VARIANT_IS_MT)
    set(VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}-mt")

    # If we're creating versioned names, tack on "-mt"
    set(VARIANT_VERSIONED_NAME "${VARIANT_VERSIONED_NAME}-mt")
  endif (VARIANT_IS_MT)

  # Add -static for static libraries, -shared for shared libraries
  list_contains(VARIANT_IS_STATIC STATIC ${ARGN})
  if (VARIANT_IS_STATIC)
    set(VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}-static")
    set(VARIANT_DISPLAY_NAME "Static")
  else (VARIANT_IS_STATIC)
    set(VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}-shared")
    set(VARIANT_DISPLAY_NAME "Shared")
  endif (VARIANT_IS_STATIC)

  # Add "multi-threaded" to the display name for multithreaded libraries.
  if (VARIANT_IS_MT)
    set(VARIANT_DISPLAY_NAME "${VARIANT_DISPLAY_NAME}, multi-threaded")
  endif ()

  # Compute the ABI tag, which depends on various kinds of options
  set(VARIANT_ABI_TAG "")

  # Linking statically to the runtime library
  list_contains(VARIANT_IS_STATIC_RUNTIME STATIC_RUNTIME ${ARGN})
  if (VARIANT_IS_STATIC_RUNTIME)  
    set(VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}-staticrt")
    set(VARIANT_ABI_TAG "${VARIANT_ABI_TAG}s")
    set(VARIANT_DISPLAY_NAME "${VARIANT_DISPLAY_NAME}, static runtime")
  endif (VARIANT_IS_STATIC_RUNTIME)
  
  # Using the debug version of the runtime library.
  # With Visual C++, this comes automatically with debug
  if (MSVC)
    list_contains(VARIANT_IS_DEBUG DEBUG ${ARGN})
    if (VARIANT_IS_DEBUG)
      set(VARIANT_ABI_TAG "${VARIANT_ABI_TAG}g")
    endif (VARIANT_IS_DEBUG)
  endif (MSVC)

  # Add -pydebug for debug builds of Python
  list_contains(VARIANT_IS_PYDEBUG PYTHON_DEBUG ${ARGN})
  if (VARIANT_IS_PYDEBUG)
    set(VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}-pydebug")
    set(VARIANT_ABI_TAG "${VARIANT_ABI_TAG}y")
    set(VARIANT_DISPLAY_NAME "${VARIANT_DISPLAY_NAME}, Python debugging")
  endif (VARIANT_IS_PYDEBUG)

  # TODO: STLport rather than default library
  # TODO: STLport's deprecated iostreams

  # Add -debug for debug libraries
  list_contains(VARIANT_IS_DEBUG DEBUG ${ARGN})
  if (VARIANT_IS_DEBUG)
    # Only add the actual "-debug" if we're also building release libraries
    if (BUILD_RELEASE)
      set(VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}-debug")
    endif (BUILD_RELEASE)
    set(VARIANT_ABI_TAG "${VARIANT_ABI_TAG}d")

    set(VARIANT_DISPLAY_NAME "${VARIANT_DISPLAY_NAME}, debug")
  else()
    set(VARIANT_DISPLAY_NAME "${VARIANT_DISPLAY_NAME}, release")
  endif()

  # If there is an ABI tag, append it to the versioned name
  if (VARIANT_ABI_TAG)
    set(VARIANT_VERSIONED_NAME "${VARIANT_VERSIONED_NAME}-${VARIANT_ABI_TAG}")
  endif (VARIANT_ABI_TAG)

  if(BUILD_VERSIONED)
    # Append the Boost version number to the versioned name
    if(BOOST_VERSION_SUBMINOR GREATER 0)
      set(VARIANT_VERSIONED_NAME
    "${VARIANT_VERSIONED_NAME}-${BOOST_VERSION_MAJOR}_${BOOST_VERSION_MINOR}_${BOOST_VERSION_SUBMINOR}")
    else(BOOST_VERSION_SUBMINOR GREATER 0)
      set(VARIANT_VERSIONED_NAME 
    "${VARIANT_VERSIONED_NAME}-${BOOST_VERSION_MAJOR}_${BOOST_VERSION_MINOR}")
    endif(BOOST_VERSION_SUBMINOR GREATER 0)
  endif(BUILD_VERSIONED)
endmacro(boost_library_variant_target_name)

# This macro is an internal utility macro that updates compilation and
# linking flags based on interactions among the features in a variant.
#
#   boost_feature_interactions(prefix
#                              feature1 feature2 ...)
#
# where "prefix" is the prefix of the compilation and linking flags
# that will be updated (e.g., ${prefix}_COMPILE_FLAGS). feature1,
# feature2, etc. are the names of the features used in this particular
# variant. If the features in this variant conflict, set
# ${prefix}_OKAY to FALSE.
macro(boost_feature_interactions PREFIX)
  # Don't build or link against a shared library and a static run-time
  list_contains(IS_SHARED SHARED ${ARGN})
  list_contains(IS_STATIC_RUNTIME STATIC_RUNTIME ${ARGN})
  if (IS_SHARED AND IS_STATIC_RUNTIME)
    set(${PREFIX}_OKAY FALSE)
  endif (IS_SHARED AND IS_STATIC_RUNTIME)
  
  # With Visual C++, the dynamic runtime is multi-threaded only
  if (MSVC)
    list_contains(IS_DYNAMIC_RUNTIME DYNAMIC_RUNTIME ${ARGN})
    list_contains(IS_SINGLE_THREADED SINGLE_THREADED ${ARGN})
    if (IS_DYNAMIC_RUNTIME AND IS_SINGLE_THREADED)
      set(${PREFIX}_OKAY FALSE)
    endif (IS_DYNAMIC_RUNTIME AND IS_SINGLE_THREADED) 
  endif (MSVC)

  # Visual C++-specific runtime library flags
  if(MSVC)
    list_contains(IS_STATIC_RUNTIME STATIC_RUNTIME ${ARGN})
    list_contains(IS_DEBUG DEBUG ${ARGN})
    if(IS_DEBUG)
      if(IS_STATIC_RUNTIME)
        set(${PREFIX}_COMPILE_FLAGS "/MTd ${${PREFIX}_COMPILE_FLAGS}")
      else(IS_STATIC_RUNTIME)
        set(${PREFIX}_COMPILE_FLAGS "/MDd ${${PREFIX}_COMPILE_FLAGS}")
      endif(IS_STATIC_RUNTIME)       
    else(IS_DEBUG)
      if(IS_STATIC_RUNTIME)
        set(${PREFIX}_COMPILE_FLAGS "/MT ${${PREFIX}_COMPILE_FLAGS}")
      else(IS_STATIC_RUNTIME)
        set(${PREFIX}_COMPILE_FLAGS "/MD ${${PREFIX}_COMPILE_FLAGS}")
      endif(IS_STATIC_RUNTIME)       
    endif(IS_DEBUG)
  endif(MSVC)  
endmacro(boost_feature_interactions)

# This macro is an internal utility macro that builds a particular
# variant of a boost library.
#
#   boost_library_variant(libname 
#                         feature1 feature2 ...)
#
# where libname is the name of the Boost library (e.g.,
# "boost_filesystem") and feature1, feature2, ... are the features
# that will be used in this variant. 
#
# This macro will define a new library target based on libname and the
# specific variant name (see boost_library_variant_target_name), which
# depends on the utility target libname. The compilation and linking
# flags for this library are defined by THIS_LIB_COMPILE_FLAGS,
# THIS_LIB_LINK_FLAGS, THIS_LIB_LINK_LIBS, and all of the compile and
# linking flags implied by the features provided.
#
# If any of the features listed conflict with this library, no new
# targets will be built. For example, if the library provides the
# option NOT_MULTI_THREADED, and one of the features provided is
# MULTI_THREADED, this macro will essentially be a no-op.
macro(boost_library_variant LIBNAME)
  set(THIS_VARIANT_COMPILE_FLAGS "${THIS_LIB_COMPILE_FLAGS}")
  set(THIS_VARIANT_LINK_FLAGS "${THIS_LIB_LINK_FLAGS}")
  set(THIS_VARIANT_LINK_LIBS ${THIS_LIB_LINK_LIBS})
  
  # Determine if it is okay to build this variant
  set(THIS_VARIANT_OKAY TRUE)
  foreach(ARG ${ARGN})
    # If the library itself stated that we cannot build this variant,
    # don't. For example, we're trying to build a shared library
    # variant, but the user specified NO_SHARED in the requirements of
    # the library.
    if (THIS_LIB_NO_${ARG})
      set(THIS_VARIANT_OKAY FALSE)
    endif (THIS_LIB_NO_${ARG})

    # If the user specified that we should not build any variants of
    # this kind, don't. For example, if the BUILD_SHARED option is
    # off, don't build shared libraries.
    if(NOT BUILD_${ARG})
      set(THIS_VARIANT_OKAY FALSE)
    endif(NOT BUILD_${ARG})

    # Accumulate compile and link flags
    set(THIS_VARIANT_COMPILE_FLAGS "${THIS_VARIANT_COMPILE_FLAGS} ${THIS_LIB_${ARG}_COMPILE_FLAGS} ${${ARG}_COMPILE_FLAGS}")
    set(THIS_VARIANT_LINK_FLAGS "${THIS_VARIANT_LINK_FLAGS} ${THIS_LIB_${ARG}_LINK_FLAGS} ${${ARG}_LINK_FLAGS}")
    set(THIS_VARIANT_LINK_LIBS ${THIS_VARIANT_LINK_LIBS} ${THIS_LIB_${ARG}_LINK_LIBS} ${${ARG}_LINK_LIBS})
  endforeach(ARG ${ARGN})

  # Handle feature interactions
  boost_feature_interactions("THIS_VARIANT" ${ARGN})

  if (THIS_VARIANT_OKAY)
 
    # Determine the suffix for this library target
    boost_library_variant_target_name(${ARGN})
    set(VARIANT_LIBNAME "${LIBNAME}${VARIANT_TARGET_NAME}")

    # We handle static vs. dynamic libraries differently
    list_contains(THIS_LIB_IS_STATIC "STATIC" ${ARGN})
    if (THIS_LIB_IS_STATIC)
      # If the STATIC_TAG flag was set, we append "-s" to the name of
      # the library. This is an unfortunate hack, needed only for the
      # test library.
      if (THIS_LIB_STATIC_TAG)
        set(THIS_LIB_STATIC_TAG "-s")
      else(THIS_LIB_STATIC_TAG)
        set(THIS_LIB_STATIC_TAG "")
      endif(THIS_LIB_STATIC_TAG)
      
      # On Windows, we need static and shared libraries to have
      # different names, so we follow the Boost.Build version 2 style
      # and prepend "lib" to the name.
      if(WIN32 AND NOT CYGWIN)
        set(LIBPREFIX "lib")
      else(WIN32 AND NOT CYGWIN)
        set(LIBPREFIX "")
      endif(WIN32 AND NOT CYGWIN)
      
      # Add the library itself
      add_library(${VARIANT_LIBNAME} STATIC ${THIS_LIB_SOURCES})

      # Set properties on this library
      set_target_properties(${VARIANT_LIBNAME}
        PROPERTIES
        OUTPUT_NAME "${LIBPREFIX}${LIBNAME}${VARIANT_VERSIONED_NAME}${THIS_LIB_STATIC_TAG}"
        CLEAN_DIRECT_OUTPUT 1
        COMPILE_FLAGS "${THIS_VARIANT_COMPILE_FLAGS}"
        LINK_FLAGS "${THIS_VARIANT_LINK_FLAGS}"
        )
    elseif (THIS_LIB_MODULE)
      # Add a module
      add_library(${VARIANT_LIBNAME} MODULE ${THIS_LIB_SOURCES})

      # Set properties on this library
      set_target_properties(${VARIANT_LIBNAME}
        PROPERTIES
        OUTPUT_NAME "${LIBNAME}${VARIANT_VERSIONED_NAME}"
        CLEAN_DIRECT_OUTPUT 1
        COMPILE_FLAGS "${THIS_VARIANT_COMPILE_FLAGS}"
        LINK_FLAGS "${THIS_VARIANT_LINK_FLAGS}"
       # SOVERSION "${BOOST_VERSION}"
        )
    else (THIS_LIB_IS_STATIC)
      #TODO: Check the SOVERSION behavior on Linux and Windows
      # Add a module
      add_library(${VARIANT_LIBNAME} SHARED ${THIS_LIB_SOURCES})
      # Set properties on this library
      set_target_properties(${VARIANT_LIBNAME}
        PROPERTIES
        OUTPUT_NAME "${LIBNAME}${VARIANT_VERSIONED_NAME}"
        CLEAN_DIRECT_OUTPUT 1
        COMPILE_FLAGS "${THIS_VARIANT_COMPILE_FLAGS}"
        LINK_FLAGS "${THIS_VARIANT_LINK_FLAGS}"
        # SOVERSION "${BOOST_VERSION}"
        )
    endif (THIS_LIB_IS_STATIC)
      
    # The basic LIBNAME target depends on each of the variants
    add_dependencies(${LIBNAME} ${VARIANT_LIBNAME})
    
    boost_post_results(${PROJECT_NAME} ${VARIANT_LIBNAME} build ${CMAKE_CURRENT_BINARY_DIR})

    # Link against whatever libraries this library depends on
    target_link_libraries(${VARIANT_LIBNAME} ${THIS_VARIANT_LINK_LIBS})
    foreach(dependency ${THIS_LIB_DEPENDS})
      target_link_libraries(${VARIANT_LIBNAME} "${dependency}${VARIANT_TARGET_NAME}")
    endforeach(dependency)

    if(NOT THIS_LIB_NO_INSTALL)
      # Setup installation properties
      string(TOLOWER "${PROJECT_NAME}${VARIANT_TARGET_NAME}" LIB_COMPONENT)
      string(REPLACE "-" "_" LIB_COMPONENT ${LIB_COMPONENT})
      
      # Installation of this library variant
      string(TOLOWER ${PROJECT_NAME} libname)
      install(TARGETS ${VARIANT_LIBNAME} DESTINATION lib COMPONENT ${LIB_COMPONENT})
      set_property( 
            TARGET ${VARIANT_LIBNAME}
            PROPERTY BOOST_CPACK_COMPONENT
            ${LIB_COMPONENT})
      
      # Make the library installation component dependent on the library
      # installation components of dependent libraries.
      set(THIS_LIB_COMPONENT_DEPENDS)
      foreach(DEP ${THIS_LIB_DEPENDS})
        # We ask the library variant that this library depends on to tell us
        # what it's associated installation component is. We depend on that 
        # installation component.
        get_property(DEP_COMPONENT 
          TARGET "${DEP}${VARIANT_TARGET_NAME}"
          PROPERTY BOOST_CPACK_COMPONENT)
        
    if (DEP_COMPONENT)
          if (DEP_COMPONENT STREQUAL LIB_COMPONENT)
            # Do nothing: we have library dependencies within one 
            # Boost library
          else()
            list(APPEND THIS_LIB_COMPONENT_DEPENDS ${DEP_COMPONENT})
          endif()
    endif()
      endforeach(DEP)
      
      if (COMMAND cpack_add_component)
        cpack_add_component(${LIB_COMPONENT}
          DISPLAY_NAME "${VARIANT_DISPLAY_NAME}"
          GROUP ${libname}
          DEPENDS ${THIS_LIB_COMPONENT_DEPENDS})
      endif ()
    endif(NOT THIS_LIB_NO_INSTALL)
  endif (THIS_VARIANT_OKAY)
endmacro(boost_library_variant)

# Updates the set of default build variants to account for variations
# in the given feature.
#
#   boost_add_default_variant(feature-val1 feature-val2 ...)
#
# Each new feature creates a new set of build variants using that
# feature. For example, writing:
# 
#    boost_add_default_variant(SINGLE_THREADED MULTI_THREADED)
#
# will create single- and multi-threaded variants of every default
# library variant already defined, doubling the number of variants
# that will be built. See the top-level CMakeLists.txt for the set of
# default variants.
#
# Variables affected:
#
#   BOOST_DEFAULT_VARIANTS:
#     This variable describes all of the variants that will be built
#     by default, and will be updated with each invocation of
#     boost_add_default_variant. The variable itself is a list, where
#     each element in the list contains a colon-separated string
#     naming a specific set of features for that variant, e.g.,
#     STATIC:DEBUG:SINGLE_THREADED.
#
#   BOOST_FEATURES:
#     This variable describes all of the feature sets that we know about,
#     and will be extended each time ither boost_add_default_variant or 
#     boost_add_extra_variant is invoked. This macro will contain a list
#     of feature sets, each containing the values for a given feature
#     separated by colons, e.g., "DEBUG:RELEASE".
#
#   BOOST_ADD_ARG_NAMES:
#     This variable describes all of the feature-specific arguments
#     that can be used for the boost_add_library macro, separated by
#     semicolons. For example, given the use of
#     boost_add_default_variant above, this variable will contain (at
#     least)
#
#        SINGLE_THREADED_COMPILE_FLAGS;SINGLE_THREADED_LINK_FLAGS;
#        MULTI_THREADED_COMPILE_FLAGS;MULTI_THREADED_LINK_FLAGS
#
#     When this variable is used in boost_add_library, it turns these
#     names into feature-specific options. For example,
#     MULTI_THREADED_COMPILE_FLAGS provides extra compile flags to be
#     used only for multi-threaded variants of the library.
#
#   BOOST_ADDLIB_OPTION_NAMES:
#     Like BOOST_ADD_ARG_NAMES, this variable describes
#     feature-specific options to boost_library that can be used to
#     turn off building of the library when the variant would require
#     certain features. For example, the NO_SINGLE_THREADED option
#     turns off building of single-threaded variants for a library.
#
#   BOOST_ADDEXE_OPTION_NAMES:
#     Like BOOST_ADDLIB_OPTION_NAMES, execept that that variable 
#     describes options to boost_add_executable that can be used to
#     describe which features are needed to build the executable.
#     For example, the MULTI_THREADED option requires that the 
#     executable be built against multi-threaded libraries and with
#     multi-threaded options.
macro(boost_add_default_variant)
  # Update BOOST_DEFAULT_VARIANTS
  if (BOOST_DEFAULT_VARIANTS)
    set(BOOST_DEFAULT_VARIANTS_ORIG ${BOOST_DEFAULT_VARIANTS})
    set(BOOST_DEFAULT_VARIANTS)
    foreach(VARIANT ${BOOST_DEFAULT_VARIANTS_ORIG})
      foreach(FEATURE ${ARGN})
        list(APPEND BOOST_DEFAULT_VARIANTS "${VARIANT}:${FEATURE}")
      endforeach(FEATURE ${ARGN})
    endforeach(VARIANT ${BOOST_DEFAULT_VARIANTS_ORIG})
    set(BOOST_DEFAULT_VARIANTS_ORIG)
  else (BOOST_DEFAULT_VARIANTS)
    set(BOOST_DEFAULT_VARIANTS ${ARGN})
  endif (BOOST_DEFAULT_VARIANTS)

  # Set Feature flag options used by the boost_library macro and the
  # BOOST_FEATURES variable
  set(BOOST_DEFVAR_FEATURES)
  foreach(FEATURE ${ARGN})
    set(BOOST_ADD_ARG_NAMES 
      "${BOOST_ADD_ARG_NAMES};${FEATURE}_COMPILE_FLAGS;${FEATURE}_LINK_FLAGS;${FEATURE}_LINK_LIBS")
    set(BOOST_ADDLIB_OPTION_NAMES "${BOOST_ADDLIB_OPTION_NAMES};NO_${FEATURE}")
    set(BOOST_ADDEXE_OPTION_NAMES "${BOOST_ADDEXE_OPTION_NAMES};${FEATURE}")
    if (BOOST_DEFVAR_FEATURES)
      set(BOOST_DEFVAR_FEATURES "${BOOST_DEFVAR_FEATURES}:${FEATURE}")
    else (BOOST_DEFVAR_FEATURES)
      set(BOOST_DEFVAR_FEATURES "${FEATURE}")
    endif (BOOST_DEFVAR_FEATURES)
  endforeach(FEATURE ${ARGN})
  list(APPEND BOOST_FEATURES ${BOOST_DEFVAR_FEATURES})
endmacro(boost_add_default_variant)

# Updates the set of "extra" build variants, which may be used to
# generate extra, library-specific variants of libraries.
#
#   boost_add_extra_variant(feature-val1 feature-val2 ...)
#
# Each extra viarant makes it possible for libraries to define extra
# variants.  For example, writing:
# 
#    boost_add_extra_variant(PYTHON_NODEBUG PYTHON_DEBUG)
#
# creates a PYTHON_NODEBUG/PYTHON_DEBUG feature pair as an extra
# variant, used by the Boost.Python library, which generates separate
# variants of the Boost.Python library: one variant uses the Python
# debug libraries, the other does not.
#
# The difference between boost_add_default_variant and
# boost_add_extra_variant is that adding a new default variant
# introduces additional variants to *all* Boost libraries, unless
# those variants are explicitly excluded by the library. Adding a new
# extra variant, on the other hand, allows libraries to specifically
# request extra variants using that feature.
#
# Variables affected:
#
#   BOOST_FEATURES:
#     See boost_add_default_variant.
#
#   BOOST_ADD_ARG_NAMES: 
#     See boost_add_default_variant.
#
#   BOOST_ADDLIB_OPTION_NAMES:
#     See boost_add_default_variant.
#
#   BOOST_ADDEXE_OPTION_NAMES:
#     See boost_add_default_variant.
macro(boost_add_extra_variant)
  set(BOOST_EXTVAR_FEATURES)
  foreach(FEATURE ${ARGN})
    set(BOOST_ADD_ARG_NAMES 
      "${BOOST_ADD_ARG_NAMES};${FEATURE}_COMPILE_FLAGS;${FEATURE}_LINK_FLAGS;${FEATURE}_LINK_LIBS")
    set(BOOST_ADDLIB_OPTION_NAMES "${BOOST_ADDLIB_OPTION_NAMES};NO_${FEATURE}")
    set(BOOST_ADDEXE_OPTION_NAMES "${BOOST_ADDEXE_OPTION_NAMES};${FEATURE}")
    if (BOOST_EXTVAR_FEATURES)
      set(BOOST_EXTVAR_FEATURES "${BOOST_EXTVAR_FEATURES}:${FEATURE}")
    else (BOOST_EXTVAR_FEATURES)
      set(BOOST_EXTVAR_FEATURES "${FEATURE}")
    endif (BOOST_EXTVAR_FEATURES)
  endforeach(FEATURE ${ARGN})  
  list(APPEND BOOST_FEATURES ${BOOST_EXTVAR_FEATURES})
endmacro(boost_add_extra_variant)

# Creates a new Boost library target that generates a compiled library
# (.a, .lib, .dll, .so, etc) from source files. This routine will
# actually build several different variants of the same library, with
# different compilation options, as determined by the set of "default"
# library variants.
#
#   boost_add_library(libname
#                     source1 source2 ...
#                     [COMPILE_FLAGS compileflags]
#                     [feature_COMPILE_FLAGS compileflags]
#                     [LINK_FLAGS linkflags]
#                     [feature_LINK_FLAGS linkflags]
#                     [LINK_LIBS linklibs]
#                     [feature_LINK_LIBS linklibs]
#                     [DEPENDS libdepend1 libdepend2 ...]
#                     [STATIC_TAG]
#                     [MODULE]
#                     [NOT_feature]
#                     [EXTRA_VARIANTS variant1 variant2 ...]
#                     [FORCE_VARIANTS variant1])
#
# where libname is the name of Boost library binary (e.g.,
# "boost_regex") and source1, source2, etc. are the source files used
# to build the library, e.g., cregex.cpp.
#
# This macro has a variety of options that affect its behavior. In
# several cases, we use the placeholder "feature" in the option name
# to indicate that there are actually several different kinds of
# options, each referring to a different build feature, e.g., shared
# libraries, multi-threaded, debug build, etc. For a complete listing
# of these features, please refer to the CMakeLists.txt file in the
# root of the Boost distribution, which defines the set of features
# that will be used to build Boost libraries by default.
#
# The options that affect this macro's behavior are:
#
#   COMPILE_FLAGS: Provides additional compilation flags that will be
#   used when building all variants of the library. For example, one
#   might want to add "-DBOOST_SIGNALS_NO_LIB=1" through this option
#   (which turns off auto-linking for the Signals library while
#   building it).
#
#   feature_COMPILE_FLAGS: Provides additional compilation flags that
#   will be used only when building variants of the library that
#   include the given feature. For example,
#   MULTI_THREADED_COMPILE_FLAGS are additional flags that will be
#   used when building a multi-threaded variant, while
#   SHARED_COMPILE_FLAGS will be used when building a shared library
#   (as opposed to a static library).
#
#   LINK_FLAGS: Provides additional flags that will be passed to the
#   linker when linking each variant of the library. This option
#   should not be used to link in additional libraries; see LINK_LIBS
#   and DEPENDS.
#
#   feature_LINK_FLAGS: Provides additional flags that will be passed
#   to the linker when building variants of the library that contain a
#   specific feature, e.g., MULTI_THREADED_LINK_FLAGS. This option
#   should not be used to link in additional libraries; see
#   feature_LINK_LIBS.
#
#   LINK_LIBS: Provides additional libraries against which each of the
#   library variants will be linked. For example, one might provide
#   "expat" as options to LINK_LIBS, to state that each of the library
#   variants will link against the expat library binary. Use LINK_LIBS
#   for libraries external to Boost; for Boost libraries, use DEPENDS.
#
#   feature_LINK_LIBS: Provides additional libraries for specific
#   variants of the library to link against. For example,
#   MULTI_THREADED_LINK_LIBS provides extra libraries to link into
#   multi-threaded variants of the library.
#
#   DEPENDS: States that this Boost library depends on and links
#   against another Boost library. The arguments to DEPENDS should be
#   the unversioned name of the Boost library, such as
#   "boost_filesystem". Like LINK_LIBS, this option states that all
#   variants of the library being built will link against the stated
#   libraries. Unlike LINK_LIBS, however, DEPENDS takes particular
#   library variants into account, always linking the variant of one
#   Boost library against the same variant of the other Boost
#   library. For example, if the boost_mpi_python library DEPENDS on
#   boost_python, multi-threaded variants of boost_mpi_python will
#   link against multi-threaded variants of boost_python.
#
#   STATIC_TAG: States that the name of static library variants on
#   Unix need to be named differently from shared library
#   variants. This particular option should only be used in rare cases
#   where the static and shared library variants are incompatible,
#   such that linking against the shared library rather than the
#   static library will cause features. When this option is provided,
#   static libraries on Unix variants will have "-s" appended to their
#   names. Note: we hope that this is a temporary solution. At
#   present, it is only used by the Test library.
#
#   MODULE: This option states that, when building a shared library,
#   the shared library should be built as a module rather than a
#   normal shared library. Modules have special meaning an behavior on
#   some platforms, such as Mac OS X.
#
#   NOT_feature: States that library variants containing a particular
#   feature should not be built. For example, passing
#   NOT_SINGLE_THREADED suppresses generation of single-threaded
#   variants of this library.
#
#   EXTRA_VARIANTS: Specifies that extra variants of this library
#   should be built, based on the features listed. Each "variant" is a 
#   colon-separated list of features. For example, passing
#     EXTRA_VARIANTS "PYTHON_NODEBUG:PYTHON_DEBUG"
#   will result in the creation of an extra set of library variants,
#   some with the PYTHON_NODEBUG feature and some with the
#   PYTHON_DEBUG feature. 
#
#   FORCE_VARIANTS: This will force the build system to ALWAYS build this 
#   variant of the library not matter what variants are set.
#
# Example:
#   boost_add_library(
#     boost_thread
#     barrier.cpp condition.cpp exceptions.cpp mutex.cpp once.cpp 
#     recursive_mutex.cpp thread.cpp tss_hooks.cpp tss_dll.cpp tss_pe.cpp 
#     tss.cpp xtime.cpp
#     SHARED_COMPILE_FLAGS "-DBOOST_THREAD_BUILD_DLL=1"
#     STATIC_COMPILE_FLAGS "-DBOOST_THREAD_BUILD_LIB=1"
#     NOT_SINGLE_THREADED
#   )
macro(boost_add_library LIBNAME)
  parse_arguments(THIS_LIB
    "DEPENDS;COMPILE_FLAGS;LINK_FLAGS;LINK_LIBS;EXTRA_VARIANTS;FORCE_VARIANTS;${BOOST_ADD_ARG_NAMES}"
    "STATIC_TAG;MODULE;NO_INSTALL;${BOOST_ADDLIB_OPTION_NAMES}"
    ${ARGN}
    )
  set(THIS_LIB_SOURCES ${THIS_LIB_DEFAULT_ARGS})

  string(TOUPPER "${LIBNAME}_COMPILED_LIB" compiled_lib) 
  set (${compiled_lib} TRUE CACHE INTERNAL "")

  if (NOT TEST_INSTALLED_TREE)
    # A top-level target that refers to all of the variants of the
    # library, collectively.
    add_custom_target(${LIBNAME})

    if (THIS_LIB_EXTRA_VARIANTS)
      # Build the set of variants that we will generate for this library
      set(THIS_LIB_VARIANTS)
      foreach(VARIANT ${BOOST_DEFAULT_VARIANTS})
        foreach(EXTRA_VARIANT ${THIS_LIB_EXTRA_VARIANTS})
          string(REPLACE ":" ";" FEATURES "${EXTRA_VARIANT}")
          separate_arguments(FEATURES)
          foreach(FEATURE ${FEATURES})
            list(APPEND THIS_LIB_VARIANTS "${VARIANT}:${FEATURE}")
          endforeach(FEATURE ${FEATURES})
        endforeach(EXTRA_VARIANT ${THIS_LIB_EXTRA_VARIANTS})
      endforeach(VARIANT ${BOOST_DEFAULT_VARIANTS})
    else (THIS_LIB_EXTRA_VARIANTS)
      set(THIS_LIB_VARIANTS ${BOOST_DEFAULT_VARIANTS})
    endif (THIS_LIB_EXTRA_VARIANTS)
    
    if (THIS_LIB_FORCE_VARIANTS)
    #  string(TOUPPER "${LIBNAME}_FORCE_VARIANTS" force_variants)
    #  set(${force_variants} ${THIS_LIB_FORCE_VARIANTS} CACHE INTERNAL "")
      set(BUILD_${THIS_LIB_FORCE_VARIANTS}_PREV ${BUILD_${THIS_LIB_FORCE_VARIANTS}} )
      set(BUILD_${THIS_LIB_FORCE_VARIANTS} TRUE)
    endif (THIS_LIB_FORCE_VARIANTS)
    
    
    # Build each of the library variants
    foreach(VARIANT_STR ${THIS_LIB_VARIANTS})
      string(REPLACE ":" ";" VARIANT ${VARIANT_STR})
      separate_arguments(VARIANT)
      boost_library_variant(${LIBNAME} ${VARIANT})
    endforeach(VARIANT_STR ${THIS_LIB_VARIANTS})
  endif (NOT TEST_INSTALLED_TREE)
  
  if (THIS_LIB_FORCE_VARIANTS)
    set(BUILD_${THIS_LIB_FORCE_VARIANTS} ${BUILD_${THIS_LIB_FORCE_VARIANTS}_PREV} )
   # message(STATUS "* ^^ BUILD_${THIS_LIB_FORCE_VARIANTS}  ${BUILD_${THIS_LIB_FORCE_VARIANTS}}")
  endif (THIS_LIB_FORCE_VARIANTS)
  
endmacro(boost_add_library)

# Creates a new executable from source files.
#
#   boost_add_executable(exename
#                        source1 source2 ...
#                        [COMPILE_FLAGS compileflags]
#                        [feature_COMPILE_FLAGS compileflags]
#                        [LINK_FLAGS linkflags]
#                        [feature_LINK_FLAGS linkflags]
#                        [LINK_LIBS linklibs]
#                        [feature_LINK_LIBS linklibs]
#                        [DEPENDS libdepend1 libdepend2 ...]
#                        [feature]
#                        [NO_INSTALL])
#
# where exename is the name of the executable (e.g., "wave").  source1,
# source2, etc. are the source files used to build the executable, e.g.,
# cpp.cpp. If no source files are provided, "exename.cpp" will be
# used.
#
# This macro has a variety of options that affect its behavior. In
# several cases, we use the placeholder "feature" in the option name
# to indicate that there are actually several different kinds of
# options, each referring to a different build feature, e.g., shared
# libraries, multi-threaded, debug build, etc. For a complete listing
# of these features, please refer to the CMakeLists.txt file in the
# root of the Boost distribution, which defines the set of features
# that will be used to build Boost libraries by default.
#
# The options that affect this macro's behavior are:
#
#   COMPILE_FLAGS: Provides additional compilation flags that will be
#   used when building the executable.
#
#   feature_COMPILE_FLAGS: Provides additional compilation flags that
#   will be used only when building the executable with the given
#   feature (e.g., SHARED_COMPILE_FLAGS when we're linking against
#   shared libraries). Note that the set of features used to build the
#   executable depends both on the arguments given to
#   boost_add_executable (see the "feature" argument description,
#   below) and on the user's choice of variants to build.
#
#   LINK_FLAGS: Provides additional flags that will be passed to the
#   linker when linking the executable. This option should not be used
#   to link in additional libraries; see LINK_LIBS and DEPENDS.
#
#   feature_LINK_FLAGS: Provides additional flags that will be passed
#   to the linker when linking the executable with the given feature
#   (e.g., MULTI_THREADED_LINK_FLAGS when we're linking a
#   multi-threaded executable).
#
#   LINK_LIBS: Provides additional libraries against which the
#   executable will be linked. For example, one might provide "expat"
#   as options to LINK_LIBS, to state that the executable will link
#   against the expat library binary. Use LINK_LIBS for libraries
#   external to Boost; for Boost libraries, use DEPENDS.
#
#   feature_LINK_LIBS: Provides additional libraries to link against
#   when linking an executable built with the given feature. 
#
#   DEPENDS: States that this executable depends on and links against
#   a Boostlibrary. The arguments to DEPENDS should be the unversioned
#   name of the Boost library, such as "boost_filesystem". Like
#   LINK_LIBS, this option states that the executable will link
#   against the stated libraries. Unlike LINK_LIBS, however, DEPENDS
#   takes particular library variants into account, always linking to
#   the appropriate variant of a Boost library. For example, if the
#   MULTI_THREADED feature was requested in the call to
#   boost_add_executable, DEPENDS will ensure that we only link
#   against multi-threaded libraries.
#
#   feature: States that the executable should always be built using a
#   given feature, e.g., SHARED linking (against its libraries) or
#   MULTI_THREADED (for multi-threaded builds). If that feature has
#   been turned off by the user, the executable will not build.
#
#   NO_INSTALL: Don't install this executable with the rest of Boost.
#
#   OUTPUT_NAME: If you want the executable to be generated somewhere
#   other than the binary directory, pass the path (including
#   directory and file name) via the OUTPUT_NAME parameter.
#
# Example:
#   boost_add_executable(wave cpp.cpp 
#     DEPENDS boost_wave boost_program_options boost_filesystem 
#             boost_serialization
#     )
macro(boost_add_executable EXENAME)
  # Note: ARGS is here to support the use of boost_add_executable in
  # the testing code.
  parse_arguments(THIS_EXE
    "DEPENDS;COMPILE_FLAGS;LINK_FLAGS;LINK_LIBS;OUTPUT_NAME;ARGS;${BOOST_ADD_ARG_NAMES}"
    "NO_INSTALL;${BOOST_ADDEXE_OPTION_NAMES}"
    ${ARGN}
    )

  # Determine the list of sources
  if (THIS_EXE_DEFAULT_ARGS)
    set(THIS_EXE_SOURCES ${THIS_EXE_DEFAULT_ARGS})
  else (THIS_EXE_DEFAULT_ARGS)
    set(THIS_EXE_SOURCES ${EXENAME}.cpp)
  endif (THIS_EXE_DEFAULT_ARGS)

  # Whether we can build both debug and release versions of this
  # executable within an IDE (based on the selected configuration
  # type).
  set(THIS_EXE_DEBUG_AND_RELEASE FALSE)
  
  # Compute the variant that will be used to build this executable,
  # taking into account both the requested features passed to
  # boost_add_executable and what options the user has set.
  set(THIS_EXE_OKAY TRUE)
  set(THIS_EXE_VARIANT)

  foreach(FEATURESET_STR ${BOOST_FEATURES})
    string(REPLACE ":" ";" FEATURESET ${FEATURESET_STR})
    separate_arguments(FEATURESET)
    set(THIS_EXE_REQUESTED_FROM_SET FALSE)
    foreach (FEATURE ${FEATURESET})
      if (THIS_EXE_${FEATURE})
        # Make this feature part of the variant
        list(APPEND THIS_EXE_VARIANT ${FEATURE})
        set(THIS_EXE_REQUESTED_FROM_SET TRUE)

        # The caller has requested this particular feature be used
        # when building the executable. If we can't satisfy that
        # request (because the user has turned off the build variants
        # with that feature), then we won't build this executable.
        if (NOT BUILD_${FEATURE})
          set(THIS_EXE_OKAY FALSE)
          message(STATUS "* ${EXENAME} is NOT being built because BUILD_${FEATURE} is FALSE")
        endif (NOT BUILD_${FEATURE})
      endif (THIS_EXE_${FEATURE})
    endforeach (FEATURE ${FEATURESET})

    if (NOT THIS_EXE_REQUESTED_FROM_SET)
      # The caller did not specify which feature value to use from
      # this set, so find the first feature value that actually works.
      set(THIS_EXE_FOUND_FEATURE FALSE)

      # If this feature set decides between Release and Debug, we
      # either query CMAKE_BUILD_TYPE to determine which to use (for
      # makefile targets) or handle both variants separately (for IDE
      # targets).
      if (FEATURESET_STR STREQUAL "RELEASE:DEBUG")
        if (CMAKE_CONFIGURATION_TYPES)
          # IDE target: can we build both debug and release?
          if (BUILD_DEBUG AND BUILD_RELEASE)
            # Remember that we're capable of building both configurations
            set(THIS_EXE_DEBUG_AND_RELEASE TRUE)

            # Don't add RELEASE or DEBUG to the variant (yet)
            set(THIS_EXE_FOUND_FEATURE TRUE)
          endif (BUILD_DEBUG AND BUILD_RELEASE)
        else (CMAKE_CONFIGURATION_TYPES)
          # Makefile target: CMAKE_BUILD_TYPE tells us which variant to build
          if (CMAKE_BUILD_TYPE STREQUAL "Release")
            # Okay, build the release variant
            list(APPEND THIS_EXE_VARIANT RELEASE)
            set(THIS_EXE_FOUND_FEATURE TRUE)
          elseif (CMAKE_BUILD_TYPE STREQUAL "Debug")
            # Okay, build the debug variant
            list(APPEND THIS_EXE_VARIANT DEBUG)
            set(THIS_EXE_FOUND_FEATURE TRUE)
          endif (CMAKE_BUILD_TYPE STREQUAL "Release")
        endif (CMAKE_CONFIGURATION_TYPES)
      endif (FEATURESET_STR STREQUAL "RELEASE:DEBUG")

      # Search through all of the features in the set to find one that works
      foreach (FEATURE ${FEATURESET})
        # We only care about the first feature value we find...
        if (NOT THIS_EXE_FOUND_FEATURE)
          # Are we allowed to build this feature?
          if (BUILD_${FEATURE})
            # Found it: we're done
            list(APPEND THIS_EXE_VARIANT ${FEATURE})
            set(THIS_EXE_FOUND_FEATURE TRUE)
          endif (BUILD_${FEATURE})
        endif (NOT THIS_EXE_FOUND_FEATURE)
      endforeach (FEATURE ${FEATURESET})

      if (NOT THIS_EXE_FOUND_FEATURE)
        # All of the features in this set were turned off. 
        # Just don't build anything.
        set(THIS_EXE_OKAY FALSE)
      endif (NOT THIS_EXE_FOUND_FEATURE)
    endif (NOT THIS_EXE_REQUESTED_FROM_SET)
  endforeach(FEATURESET_STR ${BOOST_FEATURES})
  
  # Propagate flags from each of the features
  if (THIS_EXE_OKAY)
    foreach (FEATURE ${THIS_EXE_VARIANT})
      # Add all of the flags for this feature
      set(THIS_EXE_COMPILE_FLAGS 
          "${THIS_EXE_COMPILE_FLAGS} ${THIS_EXE_${FEATURE}_COMPILE_FLAGS} ${${FEATURE}_COMPILE_FLAGS}")
      set(THIS_EXE_LINK_FLAGS 
          "${THIS_EXE_LINK_FLAGS} ${THIS_EXE_${FEATURE}_LINK_FLAGS} ${${FEATURE}_LINK_FLAGS} ${${FEATURE}_EXE_LINK_FLAGS}")
      set(THIS_EXE_LINK_LIBS 
          ${THIS_EXE_LINK_LIBS} ${THIS_EXE_${FEATURE}_LINK_LIBS} ${${FEATURE}_LINK_LIBS})
    endforeach (FEATURE ${THIS_EXE_VARIANT})

    # Handle feature interactions
    boost_feature_interactions("THIS_EXE" ${THIS_EXE_VARIANT})
  endif (THIS_EXE_OKAY)

  if (THIS_EXE_OKAY)
    # Compute the name of the variant targets that we'll be linking
    # against. We'll use this to link against the appropriate
    # dependencies. For IDE targets where we can build both debug and
    # release configurations, create DEBUG_ and RELEASE_ versions of
    # the macros.
    if (THIS_EXE_DEBUG_AND_RELEASE)
      boost_library_variant_target_name(RELEASE ${THIS_EXE_VARIANT})
      set(RELEASE_VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}")
      boost_library_variant_target_name(DEBUG ${THIS_EXE_VARIANT})
      set(DEBUG_VARIANT_TARGET_NAME "${VARIANT_TARGET_NAME}")
    else (THIS_EXE_DEBUG_AND_RELEASE)
      boost_library_variant_target_name(${THIS_EXE_VARIANT})
    endif (THIS_EXE_DEBUG_AND_RELEASE)

    # Compute the actual set of library dependencies, based on the
    # variant name we computed above. The RELEASE and DEBUG versions
    # only apply when THIS_EXE_DEBUG_AND_RELEASE.
    set(THIS_EXE_ACTUAL_DEPENDS)
    set(THIS_EXE_RELEASE_ACTUAL_DEPENDS)
    set(THIS_EXE_DEBUG_ACTUAL_DEPENDS)
    foreach(LIB ${THIS_EXE_DEPENDS})
      if (LIB MATCHES ".*-.*")
        # The user tried to state exactly which variant to use. Just
        # propagate the dependency and hope that s/he was
        # right. Eventually, this should at least warn, because it is
        # not the "proper" way to do things
        list(APPEND THIS_EXE_ACTUAL_DEPENDS ${LIB})
        list(APPEND THIS_EXE_RELEASE_ACTUAL_DEPENDS ${LIB})
        list(APPEND THIS_EXE_DEBUG_ACTUAL_DEPENDS ${LIB})
      else (LIB MATCHES ".*-.*")
        # The user has given the name of just the library target,
        # e.g., "boost_filesystem". We add on the appropriate variant
        # name(s).
        list(APPEND THIS_EXE_ACTUAL_DEPENDS "${LIB}${VARIANT_TARGET_NAME}")
        list(APPEND THIS_EXE_RELEASE_ACTUAL_DEPENDS "${LIB}${RELEASE_VARIANT_TARGET_NAME}")
        list(APPEND THIS_EXE_DEBUG_ACTUAL_DEPENDS "${LIB}${DEBUG_VARIANT_TARGET_NAME}")
      endif (LIB MATCHES ".*-.*")
    endforeach(LIB ${THIS_EXE_DEPENDS})

    # Build the executable
    # TODO: the use of ${PROJECT_NAME}/${EXENAME} is a bit strange.
    # It's designed to keep the names of regression tests from one library
    # separate from the regression tests of another library, but this can
    # be handled better with OUTPUT_NAME. This would also allow us to eliminate
    # the directory-creation logic in boost_library_project.
    if (THIS_PROJECT_IS_TOOL)
      set(THIS_EXE_NAME ${EXENAME})
    else()
      set(THIS_EXE_NAME ${PROJECT_NAME}-${EXENAME})
    endif()
    add_executable(${THIS_EXE_NAME} ${THIS_EXE_SOURCES})
    
    # Set the various compilation and linking flags
    set_target_properties(${THIS_EXE_NAME}
      PROPERTIES
      COMPILE_FLAGS "${THIS_EXE_COMPILE_FLAGS}"
      LINK_FLAGS "${THIS_EXE_LINK_FLAGS}"
      )

    # For IDE generators where we can build both debug and release
    # configurations, pass the configurations along separately.
    if (THIS_EXE_DEBUG_AND_RELEASE)
      set_target_properties(${THIS_EXE_NAME}
        PROPERTIES
        COMPILE_FLAGS_DEBUG "${DEBUG_COMPILE_FLAGS} ${THIS_EXE_COMPILE_FLAGS}"
        COMPILE_FLAGS_RELEASE "${RELEASE_COMPILE_FLAGS} ${THIS_EXE_COMPILE_FLAGS}"
        LINK_FLAGS_DEBUG "${DEBUG_LINK_FLAGS} ${DEBUG_EXE_LINK_FLAGS} ${THIS_EXE_LINK_FLAGS}"
        LINK_FLAGS_RELEASE "${RELEASE_LINK_FLAGS} ${RELEASE_EXE_LINK_FLAGS} ${THIS_EXE_LINK_FLAGS}"
        )
    endif (THIS_EXE_DEBUG_AND_RELEASE)

    # If the user gave an output name, use it.
    if(THIS_EXE_OUTPUT_NAME)
      set_target_properties(${THIS_EXE_NAME}
        PROPERTIES
        OUTPUT_NAME ${THIS_EXE_OUTPUT_NAME}
        )
    endif()

    # Link against the various libraries 
    if (THIS_EXE_DEBUG_AND_RELEASE)
      # Configuration-agnostic libraries
      target_link_libraries(${THIS_EXE_NAME} ${THIS_EXE_LINK_LIBS})
      
      # Link against libraries for "release" configuration
      foreach(LIB ${THIS_EXE_RELEASE_ACTUAL_DEPENDS} ${THIS_EXE_RELEASE_LINK_LIBS})     
        target_link_libraries(${THIS_EXE_NAME} optimized ${LIB})
      endforeach(LIB ${THIS_EXE_RELEASE_ACTUAL_DEPENDS} ${THIS_EXE_RELEASE_LINK_LIBS})     
        
      # Link against libraries for "debug" configuration
      foreach(LIB ${THIS_EXE_DEBUG_ACTUAL_DEPENDS} ${THIS_EXE_DEBUG_LINK_LIBS})     
        target_link_libraries(${THIS_EXE_NAME} debug ${LIB})
      endforeach(LIB ${THIS_EXE_DEBUG_ACTUAL_DEPENDS} ${THIS_EXE_DEBUG_LINK_LIBS})     
    else (THIS_EXE_DEBUG_AND_RELEASE)
      target_link_libraries(${THIS_EXE_NAME} 
        ${THIS_EXE_ACTUAL_DEPENDS} 
        ${THIS_EXE_LINK_LIBS})
    endif (THIS_EXE_DEBUG_AND_RELEASE)

    # Install the executable, if not suppressed
    if (NOT THIS_EXE_NO_INSTALL)
      install(TARGETS ${THIS_EXE_NAME} DESTINATION bin)
    endif (NOT THIS_EXE_NO_INSTALL)
  endif (THIS_EXE_OKAY)
endmacro(boost_add_executable)
