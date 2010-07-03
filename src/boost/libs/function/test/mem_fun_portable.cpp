// Function library

// Copyright (C) 2001-2003 Douglas Gregor

// Use, modification and distribution is subject to the Boost Software 
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt) 

// For more information, see http://www.boost.org/

    
#include <boost/function.hpp>
#include <iostream>
#include <functional>

struct X {
  int foo(int);
};
int X::foo(int x) { return -x; }

int main()
{
    boost::function2<int, X*, int> f;

f = &X::foo;
  
X x;
f(&x, 5);

    return 0;
}
