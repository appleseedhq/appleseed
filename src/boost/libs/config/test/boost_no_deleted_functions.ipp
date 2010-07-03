//  (C) Copyright Beman Dawes 2008

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for more information.

//  MACRO:         BOOST_NO_DELETED_FUNCTIONS 
//  TITLE:         C++0x delete functions unavailable
//  DESCRIPTION:   The compiler does not support C++0x delete functions

namespace boost_no_constexpr {

  struct foo {
    foo() = delete;
  };

int test()
{
  return 0;
}

}
