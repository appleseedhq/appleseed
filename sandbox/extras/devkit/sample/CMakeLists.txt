CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

PROJECT ( appleseed_sample )

FIND_PACKAGE ( Appleseed )
FIND_PACKAGE ( ILMBase )
FIND_PACKAGE ( Boost )

INCLUDE_DIRECTORIES ( ${ILMBASE_INCLUDE_DIRS} )
INCLUDE_DIRECTORIES ( ${APPLESEED_INCLUDE_DIRS} )
INCLUDE_DIRECTORIES ( ${Boost_INCLUDE_DIR} )

MESSAGE ( "APPLESEED_INCLUDE_DIRS = ${APPLESEED_INCLUDE_DIRS}" )

ADD_EXECUTABLE ( sample
  half.cpp
  sample.cpp
  )

MESSAGE ( "Appleseed_appleseed_LIBRARY=${Appleseed_appleseed_LIBRARY}" )
MESSAGE ( "Appleseed_appleseed_shared_LIBRARY=${Appleseed_appleseed_shared_LIBRARY}" )

TARGET_LINK_LIBRARIES ( sample
  ${Appleseed_appleseed_LIBRARY}
  ${Appleseed_appleseed_shared_LIBRARY}
)
