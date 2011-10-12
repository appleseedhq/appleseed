cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_LIB)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
#INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
#INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

SET (H5_TOOLS_LIB_SRCS
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_array.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_attr.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_dset.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_util.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_filters.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_ref.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_str.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_type.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_utils.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5trav.c
)

SET (H5_TOOLS_LIB_HDRS
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5trav.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_utils.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_str.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_ref.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff.h
)

ADD_LIBRARY (${HDF5_TOOLS_LIB_TARGET} ${LIB_TYPE} ${H5_TOOLS_LIB_SRCS} ${H5_TOOLS_LIB_HDRS})
TARGET_LINK_LIBRARIES (${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_GLOBAL_VARIABLE( HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_TOOLS_LIB_TARGET}")
H5_SET_LIB_OPTIONS (
    ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TOOLS_LIB_NAME}
    ${LIB_TYPE}
    HDF5_TOOLS_LIB_NAME_RELEASE
	HDF5_TOOLS_LIB_NAME_DEBUG
)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install 
#-----------------------------------------------------------------------------
INSTALL (
    FILES
        ${H5_TOOLS_LIB_HDRS}
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}/tools
    COMPONENT
        toolsheaders
)

#-----------------------------------------------------------------------------
# Because tools are installed into bin/tools we need to make sure the hdf5tools
# library will be installed into the proper location.
#-----------------------------------------------------------------------------
IF (APPLE)
    OPTION (HDF5_BUILD_WITH_INSTALL_NAME "Build with library install_name set to the installation path" OFF)
    IF (HDF5_BUILD_WITH_INSTALL_NAME)
      SET_TARGET_PROPERTIES(${HDF5_TOOLS_LIB_TARGET} PROPERTIES
      LINK_FLAGS "-current_version ${HDF5_PACKAGE_VERSION} -compatibility_version ${HDF5_PACKAGE_VERSION}"
      INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib/tools"
      BUILD_WITH_INSTALL_RPATH ${HDF5_BUILD_WITH_INSTALL_NAME}
      )
    ENDIF (HDF5_BUILD_WITH_INSTALL_NAME)
ENDIF (APPLE)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
IF (HDF5_EXPORTED_TARGETS)
  INSTALL (
      TARGETS
          ${HDF5_TOOLS_LIB_TARGET}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR}/tools COMPONENT toolslibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR}/tools COMPONENT toolslibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR}/tools COMPONENT toolslibraries
  )
ENDIF (HDF5_EXPORTED_TARGETS)
