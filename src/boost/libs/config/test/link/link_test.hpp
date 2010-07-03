//  (C) Copyright John Maddock 2003. 
//  Use, modification and distribution are subject to the 
//  Boost Software License, Version 1.0. (See accompanying file 
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for the most recent version.

#ifndef BOOST_LINK_TEST_HPP
#define BOOST_LINK_TEST_HPP

#include <boost/config.hpp>

//
// set up code to determine our compilers options, 
// we will check that these are the same in the
// .exe and the .dll:
//
#ifdef BOOST_DYN_LINK
static const bool dyn_link = true;
#else
static const bool dyn_link = false;
#endif
#if defined(_DLL) || defined(_RTLDLL)
static const bool dyn_rtl = true;
#else
static const bool dyn_rtl = false;
#endif
#if defined(BOOST_HAS_THREADS)
static const bool has_threads = true;
#else
static const bool has_threads = false;
#endif
#if defined(_DEBUG)
static const bool debug = true;
#else
static const bool debug = false;
#endif
#if defined(__STL_DEBUG) || defined(_STLP_DEBUG)
static const bool stl_debug = true;
#else
static const bool stl_debug = false;
#endif

//
// set up import and export options:
//
#if defined(BOOST_HAS_DECLSPEC) && defined(BOOST_DYN_LINK)
#  ifdef BOOST_CONFIG_SOURCE
#      define BOOST_CONFIG_DECL __declspec(dllexport)
#  else
#      define BOOST_CONFIG_DECL __declspec(dllimport)
#  endif
#endif
#ifndef BOOST_CONFIG_DECL
#  define BOOST_CONFIG_DECL
#endif

//
// define our entry point:
//
bool BOOST_CONFIG_DECL check_options(
                   bool m_dyn_link,
                   bool m_dyn_rtl,
                   bool m_has_threads,
                   bool m_debug,
                   bool m_stlp_debug);

//
// set up automatic linking:
//
#if !defined(BOOST_CONFIG_SOURCE) && !defined(BOOST_CONFIG_NO_LIB)
#  define BOOST_LIB_NAME link_test
#  include <boost/config/auto_link.hpp>
#endif

#endif // BOOST_LINK_TEST_HPP

