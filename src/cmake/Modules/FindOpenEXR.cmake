# Find OpenEXR
# Find OPENEXR headers and libraries.
#
#  OPENEXR_INCLUDE_DIRS - where to find OPENEXR uncludes.
#  OPENEXR_LIBRARIES    - List of libraries when using OPENEXR.
#  OPENEXR_FOUND        - True if OPENEXR found.

# Look for the header file.
find_path( OPENEXR_INCLUDE_DIR NAMES OpenEXR/ImfHeader.h)

# Look for the libraries.
find_library( OPENEXR_IMF_LIBRARY NAMES IlmImf)
find_library( OPENEXR_THREADS_LIBRARY NAMES IlmThread)

# handle the QUIETLY and REQUIRED arguments and set OPENEXR_FOUND to TRUE if
# all listed variables are TRUE
include( FindPackageHandleStandardArgs)
find_package_handle_standard_args( OPENEXR DEFAULT_MSG  OPENEXR_INCLUDE_DIR
                                                        OPENEXR_IMF_LIBRARY
                                                        OPENEXR_THREADS_LIBRARY
                                                        )

# Copy the results to the output variables.
if( OPENEXR_FOUND)
    set( OPENEXR_LIBRARIES  ${OPENEXR_IMF_LIBRARY} ${OPENEXR_THREADS_LIBRARY})
    set( OPENEXR_INCLUDE_DIRS ${OPENEXR_INCLUDE_DIR})
else( OPENEXR_FOUND)
    set( OPENEXR_LIBRARIES)
    set( OPENEXR_INCLUDE_DIRS)
endif( OPENEXR_FOUND)

mark_as_advanced( OPENEXR_IMF_LIBRARY
                  OPENEXR_INCLUDE_DIR
                  OPENEXR_THREADS_LIBRARY
                  )
