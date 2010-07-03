// Copyright Alexander Nasonov 2007-2008
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#include <string>

#include <boost/scope_exit.hpp>

#include <boost/typeof/typeof.hpp>
#include <boost/typeof/std/string.hpp>

int main()
{
    std::string const volatile s;
    BOOST_SCOPE_EXIT( (&s) )
    {
        s = "";
    } BOOST_SCOPE_EXIT_END
}

