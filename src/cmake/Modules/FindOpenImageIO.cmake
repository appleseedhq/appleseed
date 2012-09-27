#  Find OPENIMAGEIO headers and libraries.
#
#  OPENIMAGEIO_INCLUDE_DIRS - where to find OpenImageIO uncludes.
#  OPENIMAGEIO_LIBRARIES    - List of libraries when using OpenImageIO.
#  OPENIMAGEIO_FOUND        - True if OpenImageIO found.

# Look for the header file.
find_path( OPENIMAGEIO_INCLUDE_DIR NAMES OpenImageIO/imageio.h)

# Look for the library.
find_library( OPENIMAGEIO_LIBRARY NAMES OpenImageIO)

# handle the QUIETLY and REQUIRED arguments and set OPENIMAGEIO_FOUND to TRUE if
# all listed variables are TRUE
include( FindPackageHandleStandardArgs)
find_package_handle_standard_args( OPENIMAGEIO DEFAULT_MSG OPENIMAGEIO_LIBRARY OPENIMAGEIO_INCLUDE_DIR)

# Copy the results to the output variables.
if( OPENIMAGEIO_FOUND)
    set( OPENIMAGEIO_LIBRARIES ${OPENIMAGEIO_LIBRARY})
    set( OPENIMAGEIO_INCLUDE_DIRS ${OPENIMAGEIO_INCLUDE_DIR})
else( OPENIMAGEIO_FOUND)
    set( OPENIMAGEIO_LIBRARIES)
    set( OPENIMAGEIO_INCLUDE_DIRS)
endif()

mark_as_advanced( OPENIMAGEIO_INCLUDE_DIR OPENIMAGEIO_LIBRARY)
