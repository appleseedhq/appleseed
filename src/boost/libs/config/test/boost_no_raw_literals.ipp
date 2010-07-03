//  (C) Copyright Beman Dawes 2008

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for more information.

//  MACRO:         BOOST_NO_RAW_LITERALS
//  TITLE:         C++0x raw string literals unavailable
//  DESCRIPTION:   The compiler does not support C++0x raw string literals

namespace boost_no_raw_literals {

int test()
{
  const char* s = R"[abc]";
  const wchar_t* ws = LR"[abc]";
  return 0;
}

}
