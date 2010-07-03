// Copyright Alexander Nasonov 2007-2008
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#include <boost/scope_exit.hpp>

int main()
{
    int const i = 0;
    BOOST_SCOPE_EXIT( (&i) )
    {
        i = 5;
    } BOOST_SCOPE_EXIT_END
}

