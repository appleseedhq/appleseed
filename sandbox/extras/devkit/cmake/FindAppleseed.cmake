#-*-cmake-*-
#
# This auxiliary CMake file helps in find the MtoA headers and libraries
#
# APPLESEED_FOUND            set if Appleseed is found.
# APPLESEED_INCLUDE_DIR      Appleseed's include directory

FIND_PACKAGE ( PackageHandleStandardArgs )

##
## Obtain Appleseed install location
##
FIND_PATH( APPLESEED_LOCATION extras/devkit/include/appleseed/foundation/core/appleseed.h
  "$ENV{APPLESEED_ROOT}"
  NO_DEFAULT_PATH
  NO_SYSTEM_ENVIRONMENT_PATH
)

FIND_PACKAGE_HANDLE_STANDARD_ARGS ( Appleseed
  REQUIRED_VARS APPLESEED_LOCATION
  )

IF (APPLESEED_FOUND)

  SET ( ORIGINAL_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})
  IF (WIN32)
  ELSE (WIN32)
	# Non-Windows e.g Linux and OS X
	SET( APPLESEED_LIBRARY_DIR "${APPLESEED_LOCATION}/bin" CACHE_STRING "Appleseed library path" )
    FIND_LIBRARY ( Appleseed_appleseed_LIBRARY  appleseed  ${APPLESEED_LIBRARY_DIR} )
    FIND_LIBRARY ( Appleseed_appleseed_shared_LIBRARY  appleseed.shared  ${APPLESEED_LIBRARY_DIR} )
  ENDIF (WIN32)
  SET(CMAKE_FIND_LIBRARY_SUFFIXES ${ORIGINAL_CMAKE_FIND_LIBRARY_SUFFIXES})
  
  SET( APPLESEED_INCLUDE_DIRS "${APPLESEED_LOCATION}/extras/devkit/include/appleseed;${APPLESEED_LOCATION}/extras/devkit/include/appleseed.shared" CACHE STRING "Appleseed include path")

  
ENDIF (APPLESEED_FOUND)
