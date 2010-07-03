//  This file was automatically generated on Wed Sep 24 11:44:21 2008
//  by libs/config/tools/generate.cpp
//  Copyright John Maddock 2002-4.
//  Use, modification and distribution are subject to the 
//  Boost Software License, Version 1.0. (See accompanying file 
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for the most recent version.//
//  Revision $Id: no_initializer_lists_pass.cpp 50451 2009-01-03 23:18:33Z danieljames $
//


// Test file for macro BOOST_NO_INITIALIZER_LISTS
// This file should compile, if it does not then
// BOOST_NO_INITIALIZER_LISTS should be defined.
// See file boost_no_initializer_lists.ipp for details

// Must not have BOOST_ASSERT_CONFIG set; it defeats
// the objective of this file:
#ifdef BOOST_ASSERT_CONFIG
#  undef BOOST_ASSERT_CONFIG
#endif

#include <boost/config.hpp>
#include "test.hpp"

#ifndef BOOST_NO_INITIALIZER_LISTS
#include "boost_no_initializer_lists.ipp"
#else
namespace boost_no_initializer_lists = empty_boost;
#endif

int main( int, char *[] )
{
   return boost_no_initializer_lists::test();
}

