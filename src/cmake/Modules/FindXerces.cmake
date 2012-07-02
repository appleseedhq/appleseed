# - Find Xerces library
# Find Xerces-C includes and libraries
# This module defines
#  XERCES_INCLUDE_DIRS,
#  XERCES_LIBRARIES,.
#  XERCES_FOUND.
#

find_path(XERCES_INCLUDE_DIR NAMES xercesc/parsers/SAXParser.hpp)

find_library(XERCES_LIBRARY NAMES xerces-c)

# handle the QUIETLY and REQUIRED arguments and set XERCES_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(XERCES DEFAULT_MSG XERCES_LIBRARY XERCES_INCLUDE_DIR)

if(XERCES_FOUND)
  set(XERCES_LIBRARIES ${XERCES_LIBRARY})
  set(XERCES_INCLUDE_DIRS ${XERCES_INCLUDE_DIR})
endif(XERCES_FOUND)

mark_as_advanced(
  XERCES_INCLUDE_DIR
  XERCES_LIBRARY
)
