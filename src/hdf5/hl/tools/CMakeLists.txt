cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_TOOLS)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (GIF2H5_SRCS
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif.h
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/decompress.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif2hdf.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif2mem.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gifread.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/writehdf.c
)

#-- Add gif2hdf5 program
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5)

ADD_EXECUTABLE (gif2h5 ${GIF2H5_SRCS})
TARGET_NAMING (gif2h5 ${LIB_TYPE})
TARGET_LINK_LIBRARIES (gif2h5 ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

#-- Add h52gif program
SET (hdf2gif_SRCS
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/hdf2gif.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/hdfgifwr.c
)
ADD_EXECUTABLE (h52gif ${hdf2gif_SRCS})
TARGET_NAMING (h52gif ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h52gif ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

# --------------------------------------------------------------------
# This executable can generate the actual test files - Currently not
#  used in the CMake Build system as we rely on the test files that are
#  shipped with HDF5 source archives
# --------------------------------------------------------------------
IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (hl_h52gifgentest ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/h52gifgentst.c)
    TARGET_NAMING (hl_h52gifgentest ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (hl_h52gifgentest ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
    
#    ADD_TEST (NAME hl_h52gifgentest COMMAND $<TARGET_FILE:hl_h52gifgentest>)
ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        gif2h5
        h52gif
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        hltoolsapplications
)
