//  (C) Copyright Beman Dawes 2008

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for more information.

//  MACRO:         BOOST_NO_EXTERN_TEMPLATE
//  TITLE:         C++0x extern template unavailable
//  DESCRIPTION:   The compiler does not support C++0x extern template

namespace boost_no_extern_template {

extern template<class T> void f(T);

int test()
{
  return 0;
}

}
