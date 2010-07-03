# Look for the ICU library. If we find it, we'll compile in support for ICU
include(FindICU)
set(BOOST_REGEX_LIBRARIES)
if (ICU_FOUND AND ICU_I18N_FOUND)
  add_definitions(-DBOOST_HAS_ICU=1)
  include_directories(${ICU_INCLUDE_DIRS})
  set(BOOST_REGEX_LIBRARIES ${ICU_LIBRARIES} ${ICU_I18N_LIBRARIES})
endif (ICU_FOUND AND ICU_I18N_FOUND)
 
boost_add_library(boost_regex
   c_regex_traits.cpp
   cpp_regex_traits.cpp
   cregex.cpp
   fileiter.cpp
   icu.cpp
   instances.cpp
   posix_api.cpp
   regex.cpp
   regex_debug.cpp
   regex_raw_buffer.cpp
   regex_traits_defaults.cpp
   static_mutex.cpp
   w32_regex_traits.cpp
   wc_regex_traits.cpp
   wide_posix_api.cpp
   winstances.cpp 
   usinstances.cpp 
   LINK_LIBS ${BOOST_REGEX_LIBRARIES}
   SHARED_COMPILE_FLAGS -DBOOST_REGEX_DYN_LINK=1)





