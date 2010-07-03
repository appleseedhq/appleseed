//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/utility/enable_if.hpp>

#include <boost/spirit/include/karma_operator.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_string.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_directive.hpp>
#include <boost/spirit/include/karma_action.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/assign/std/vector.hpp>

#include <string>
#include <vector>
#include <iostream>

#include "test.hpp"

using namespace spirit_test;

int
main()
{
    using namespace boost::spirit;
    using namespace boost::spirit::ascii;

    using namespace boost::assign;

    std::vector<char> v;
    v += 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h';
    
    {
        BOOST_TEST(test("a,b,c,d,e,f,g,h", char_ % ',', v));
        BOOST_TEST(test_delimited("a , b , c , d , e , f , g , h ", 
            char_ % ',', v, space));
    }

    {
        std::string s ("abcdefgh");
        BOOST_TEST(test("a,b,c,d,e,f,g,h", char_ % ',', s));
        BOOST_TEST(test_delimited("a , b , c , d , e , f , g , h ", 
            char_ % ',', s, space));
    }

    { // actions
        namespace phx = boost::phoenix;
        using boost::spirit::arg_names::_1;

        BOOST_TEST(test("a,b,c,d,e,f,g,h", (char_ % ',')[_1 = phx::ref(v)]));
        BOOST_TEST(test_delimited("a , b , c , d , e , f , g , h ", 
            (char_ % ',')[_1 = phx::ref(v)], space));
    }

    return boost::report_errors();
}

