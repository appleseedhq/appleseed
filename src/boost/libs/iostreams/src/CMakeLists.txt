set(BOOST_IOSTREAMS_EXTRA_SOURCES)
set(BOOST_IOSTREAMS_LIBRARIES)

# Find zlib. If it's available, include it in the iostreams build
set(ZLIB_FIND_QUIETLY TRUE)
include(FindZLIB)
if (ZLIB_FOUND)
  include_directories(${ZLIB_INCLUDE_DIR})
  set(BOOST_IOSTREAMS_EXTRA_SOURCES 
    ${BOOST_IOSTREAMS_EXTRA_SOURCES} zlib.cpp)
  set(BOOST_IOSTREAMS_LIBRARIES
    ${BOOST_IOSTREAMS_LIBRARIES} ${ZLIB_LIBRARIES})
endif(ZLIB_FOUND)

# Find bzip2. If it's available, include it in the iostreams build
set(BZip2_FIND_QUIETLY TRUE)
include(FindBZip2)
if (BZIP2_FOUND)
  include_directories(${BZIP2_INCLUDE_DIR})
  add_definitions(${BZIP2_DEFINITIONS})
  set(BOOST_IOSTREAMS_EXTRA_SOURCES 
    ${BOOST_IOSTREAMS_EXTRA_SOURCES} bzip2.cpp)
  set(BOOST_IOSTREAMS_LIBRARIES
    ${BOOST_IOSTREAMS_LIBRARIES} ${BZIP2_LIBRARIES})

  # NOTE: What to do about BZIP2_NEED_PREFIX?
endif(BZIP2_FOUND)

boost_add_library(boost_iostreams
  file_descriptor.cpp mapped_file.cpp ${BOOST_IOSTREAMS_EXTRA_SOURCES}
  LINK_LIBS "${BOOST_IOSTREAMS_LIBRARIES}"
  SHARED_COMPILE_FLAGS "-DBOOST_IOSTREAMS_DYN_LINK=1")
