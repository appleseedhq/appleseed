//  (C) Copyright Beman Dawes 2008

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for more information.

//  MACRO:         BOOST_NO_UNICODE_LITERALS
//  TITLE:         C++0x unicode literals unavailable
//  DESCRIPTION:   The compiler does not support C++0x unicode literals

namespace boost_no_unicode_literals {

int test()
{
  const char* u8 = u8"";
  const char16_t* u16 = u"";
  const char32_t* u32 = U"";
  return 0;
}

}
