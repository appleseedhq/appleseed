boost_add_library(
  boost_system
  error_code.cpp
  SHARED_COMPILE_FLAGS "-DBOOST_SYSTEM_DYN_LINK=1"
  STATIC_COMPILE_FLAGS "-DBOOST_SYSTEM_STATIC_LINK=1"
  )

