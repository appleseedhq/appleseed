//  (C) Copyright Beman Dawes 2008

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for more information.

//  MACRO:         BOOST_NO_CHAR32_T
//  TITLE:         C++0x char32_t unavailable
//  DESCRIPTION:   The compiler does not support C++0x char32_t

namespace boost_no_char32_t {

int test()
{
  char32_t c;
  return 0;
}

}
