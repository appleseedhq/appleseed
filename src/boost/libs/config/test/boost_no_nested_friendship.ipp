//  Copyright (C) 2008 N. Musatti
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/config for most recent version.

//  MACRO:         BOOST_NO_NESTED_FRIENDSHIP
//  TITLE:         Access to private members from nested classes
//  DESCRIPTION:   If the compiler fails to support access to private members
//                 from nested classes

namespace boost_no_nested_friendship {

class A {
   static int b;
   class B {
      int f() { return b; }
   };
};

int test()
{
    return 0;
}

}

