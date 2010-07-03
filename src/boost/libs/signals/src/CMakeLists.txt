add_definitions(-DBOOST_SIGNALS_NO_LIB=1)

boost_add_library(
  boost_signals 
  trackable.cpp connection.cpp named_slot_map.cpp signal_base.cpp slot.cpp
  SHARED_COMPILE_FLAGS "-DBOOST_SIGNALS_DYN_LINK=1"
  )
