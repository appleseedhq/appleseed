add_definitions(-DBOOST_DATE_TIME_NO_LIB=1)
boost_add_library(
  boost_date_time
  gregorian/greg_month.cpp gregorian/greg_weekday.cpp gregorian/date_generators.cpp 
  STATIC_COMPILE_FLAGS -DBOOST_DATE_TIME_STATIC_LINK
  SHARED_COMPILE_FLAGS -DBOOST_ALL_DYN_LINK=1
  )
