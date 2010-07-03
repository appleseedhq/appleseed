//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/karma_auxiliary.hpp>
#include <boost/spirit/include/phoenix_core.hpp>

#include <iostream>
#include "test.hpp"

int
main()
{
    using spirit_test::test;
    using namespace boost::spirit;

    {
        BOOST_TEST(test("", eps));
    }

    {   // test action

        using namespace boost::phoenix;
        BOOST_TEST(test("", eps(val(true))));
        BOOST_TEST(!test("", eps(val(false))));
    }

    return boost::report_errors();
}
