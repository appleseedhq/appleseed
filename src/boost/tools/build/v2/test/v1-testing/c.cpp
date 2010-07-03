// Copyright David Abrahams 2002.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#include <iostream>

#ifndef RESULTCODE
# define RESULTCODE 0
#endif

int main()
{
    std::cout << "returning result: " << RESULTCODE << std::endl;
    return RESULTCODE;
}

