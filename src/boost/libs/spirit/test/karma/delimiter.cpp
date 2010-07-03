//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//#define KARMA_FAIL_COMPILATION

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_directive.hpp>
#include <boost/spirit/include/karma_operator.hpp>

#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
int
main()
{
    using namespace boost::spirit;
    
    {
        BOOST_TEST(test("a b ", delimit[char_('a') << 'b']));
        BOOST_TEST(test("a*b*", delimit('*')[char_('a') << 'b']));

        BOOST_TEST(test("ab c d", 
            char_('a') << delimit[char_('b') << 'c'] << 'd'));
        BOOST_TEST(test("ab*c*d", 
            char_('a') << delimit('*')[char_('b') << 'c'] << 'd'));

        BOOST_TEST(test_delimited("a b ", delimit[char_('a') << 'b'], char_(' ')));
        BOOST_TEST(test_delimited("a*b*", delimit('*')[char_('a') << 'b'], char_(' ')));

        BOOST_TEST(test_delimited("a b c d ", 
            char_('a') << delimit[char_('b') << 'c'] << 'd', char_(' ')));
        BOOST_TEST(test_delimited("a b*c*d ", 
            char_('a') << delimit('*')[char_('b') << 'c'] << 'd', char_(' ')));
    }
    
    {
        BOOST_TEST(test("ab", verbatim[char_('a') << 'b']));
        BOOST_TEST(test("abcd", 
            char_('a') << verbatim[char_('b') << 'c'] << 'd'));

        BOOST_TEST(test_delimited("ab ", 
            verbatim[char_('a') << 'b'], char_(' ')));
        BOOST_TEST(test_delimited("a bc d ", 
            char_('a') << verbatim[char_('b') << 'c'] << 'd', char_(' ')));
    }
    
    return boost::report_errors();
}
