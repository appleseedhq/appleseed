#  Find Imath
#  Find Imath headers and libraries.
#
#  IMATH_INCLUDE_DIRS - where to find IMATH uncludes.
#  IMATH_LIBRARIES    - List of libraries when using IMATH.
#  IMATH_FOUND        - True if IMATH found.

# Look for the header file.
find_path( IMATH_INCLUDE_DIR NAMES OpenEXR/ImathVec.h)

# Look for the libraries.
find_library( IMATH_HALF_LIBRARY NAMES Half)
find_library( IMATH_IEX_LIBRARY NAMES Iex)
find_library( IMATH_MATH_LIBRARY NAMES Imath)

# handle the QUIETLY and REQUIRED arguments and set IMATH_FOUND to TRUE if
# all listed variables are TRUE
include( FindPackageHandleStandardArgs)
find_package_handle_standard_args( IMATH DEFAULT_MSG	IMATH_HALF_LIBRARY
                                                        IMATH_IEX_LIBRARY
                                                        IMATH_MATH_LIBRARY
                                                        IMATH_INCLUDE_DIR
                                                        )
# Copy the results to the output variables.
if( IMATH_FOUND)
    set( IMATH_LIBRARIES ${IMATH_HALF_LIBRARY}
                         ${IMATH_IEX_LIBRARY}
                         ${IMATH_MATH_LIBRARY}
                         )

    set( IMATH_INCLUDE_DIRS ${IMATH_INCLUDE_DIR})
else( IMATH_FOUND)
    set( IMATH_LIBRARIES)
    set( IMATH_INCLUDE_DIRS)
endif( IMATH_FOUND)

mark_as_advanced( IMATH_HALF_LIBRARY
                  IMATH_IEX_LIBRARY
                  IMATH_MATH_LIBRARY
                  IMATH_INCLUDE_DIR
                  )
