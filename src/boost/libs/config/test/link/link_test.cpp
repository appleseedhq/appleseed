//  (C) Copyright John Maddock 2003. 
//  Use, modification and distribution are subject to the 
//  Boost Software License, Version 1.0. (See accompanying file 
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for the most recent version.


#define BOOST_CONFIG_SOURCE

#include "link_test.hpp"

bool BOOST_CONFIG_DECL check_options(
                   bool m_dyn_link,
                   bool m_dyn_rtl,
                   bool m_has_threads,
                   bool m_debug,
                   bool m_stlp_debug)
{
   return (m_dyn_link == dyn_link) 
      && (m_dyn_rtl == dyn_rtl)
      && (m_has_threads == has_threads)
      && (m_debug == debug)
      && (m_stlp_debug == stl_debug);
}

