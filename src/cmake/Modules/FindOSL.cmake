#  Find Open Shading Language
#
#  OSL_INCLUDE_DIRS - where to find OSL includes.
#  OSL_LIBRARIES    - List of libraries when using OSL.
#  OSL_FOUND        - True if OSL found.

# Look for the header file.
find_path(OSL_INCLUDE_DIR NAMES OSL/oslexec.h)

# Look for the libraries.
find_library(OSL_EXEC_LIBRARY NAMES oslexec)
find_library(OSL_COMP_LIBRARY NAMES oslcomp)
find_library(OSL_QUERY_LIBRARY NAMES oslquery)

# handle the QUIETLY and REQUIRED arguments and set OSL_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(OSL DEFAULT_MSG OSL_INCLUDE_DIR
                                                  OSL_EXEC_LIBRARY
                                                  OSL_COMP_LIBRARY
                                                  OSL_QUERY_LIBRARY
                                                  )

# Copy the results to the output variables.
if(OSL_FOUND)
    set(OSL_LIBRARIES ${OSL_EXEC_LIBRARY} ${OSL_COMP_LIBRARY} ${OSL_QUERY_LIBRARY})
    set(OSL_INCLUDE_DIRS ${OSL_INCLUDE_DIR})
else(OSL_FOUND)
    set(OSL_LIBRARIES)
    set(OSL_INCLUDE_DIRS)
endif(OSL_FOUND)

#mark_as_advanced(OSL_INCLUDE_DIR OSL_EXEC_LIBRARY OSL_COMP_LIBRARY OSL_QUERY_LIBRARY)
