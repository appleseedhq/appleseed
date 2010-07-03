if (PYTHON_LIBRARIES)
  include_directories(${PYTHON_INCLUDE_PATH})

  # Determine extra libraries we need to link against to build Python
  # extension modules.
  if(CMAKE_SYSTEM_NAME STREQUAL "SunOS")
    set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "dl")
    if(CMAKE_COMPILER_IS_GNUCXX)
      set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "rt")
    endif(CMAKE_COMPILER_IS_GNUCXX)
  elseif(CMAKE_SYSTEM_NAME MATCHES ".*BSD")
    set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "pthread")
  elseif(CMAKE_SYSTEM_NAME STREQUAL "DragonFly")
    # DragonFly is a variant of FreeBSD
    set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "pthread")
  elseif(CMAKE_SYSTEM_NAME STREQUAL "OSF")
    set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "pthread" "dl")
    if(CMAKE_COMPILER_IS_GNUCXX)
      set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "rt")
    endif(CMAKE_COMPILER_IS_GNUCXX)    
  elseif(CMAKE_SYSTEM_NAME STREQUAL "QNX")
    # No options necessary for QNX
  elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    # No options necessary for Mac OS X
  elseif(CMAKE_SYSTEM_NAME STREQUAL "HP-UX")
    set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "rt")
  elseif(UNIX)
    # Assume -pthread and -ldl on all other variants
    set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "pthread" "dl")
    if(CMAKE_COMPILER_IS_GNUCXX)
      set(PYTHON_LIBRARIES ${PYTHON_LIBRARIES} "util")
    endif(CMAKE_COMPILER_IS_GNUCXX)    
  endif(CMAKE_SYSTEM_NAME STREQUAL "SunOS")

################################################################################
#-- Macro for building Boost.Python extension modules
  macro(boost_python_extension MODULE_NAME)
    parse_arguments(BPL_EXT 
      "" 
      "" 
      ${ARGN})
      
    #TODO: The target properties are NOT being set correctly for the test libraries  
    if (FALSE)
        # Create the library target itself
        add_library(${MODULE_NAME} MODULE ${BPL_EXT_DEFAULT_ARGS} )
    
        # Miscellaneous target properties
        set_target_properties(${MODULE_NAME} PROPERTIES PREFIX "")
    
        # Link against Boost.Python library
        target_link_libraries(${MODULE_NAME} boost_python-static)
    
        # Link against Python libraries
        target_link_libraries(${MODULE_NAME} ${PYTHON_LIBRARIES})
    endif(FALSE)

    boost_add_library(
      ${MODULE_NAME}
      ${BPL_EXT_DEFAULT_ARGS}
      MODULE
      LINK_LIBS ${PYTHON_LIBRARIES}
      DEPENDS boost_python
    )
       
  endmacro(boost_python_extension)
#--
################################################################################


  boost_library_project(
    Python
    SRCDIRS src
    TESTDIRS test
    HEADERS python.hpp python
    MODULARIZED
    DESCRIPTION "A framework for interfacing Python and C++. It allows you to quickly and seamlessly expose C++ classes functions and objects to Python, and vice-versa, using no special tools -- just your C++ compiler."
    AUTHORS "David Abrahams <dave -at- boostpro.com>"
    )
endif (PYTHON_LIBRARIES)
