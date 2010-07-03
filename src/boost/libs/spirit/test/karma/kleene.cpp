//  Copyright (c) 2001-2009 Hartmut Kaiser
// 
//  Distributed under the Boost Software License, Version 1.0. (See accompanying 
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>

#include <boost/assign/std/vector.hpp>

#include <boost/spirit/include/support_argument.hpp>
// #include <boost/spirit/include/support_pack.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_string.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/spirit/include/karma_operator.hpp>
#include <boost/spirit/include/karma_action.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/spirit/include/support_unused.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>

#include "test.hpp"

using namespace spirit_test;

///////////////////////////////////////////////////////////////////////////////
int main()
{
    using namespace boost::spirit;
    namespace fusion = boost::fusion;
    
    {
        using namespace boost::assign;

        std::vector<char> v;
        v += 'a', 'b', 'c';

        BOOST_TEST(test("abc", *char_, v));
        BOOST_TEST(test_delimited("a b c ", *char_, v, ' '));
    }
    
    {
        using namespace boost::assign;

        std::vector<char> v;
        
        // these need to fail, because the return value should be false
        BOOST_TEST(!test("", +char_, v));
        BOOST_TEST(!test_delimited("", +char_, v, ' '));
        
        v += 'a', 'b', 'c';

        BOOST_TEST(test("abc", +char_, v));
        BOOST_TEST(test_delimited("a b c ", +char_, v, ' '));
    }
    
    {
        using namespace boost::assign;
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        
        std::vector<int> v;
        v += 10, 20, 30;

        BOOST_TEST(test("102030", *int_, v));
        BOOST_TEST(test_delimited("10, 20, 30, ", *int_, v, lit(", ")));

        typedef fusion::vector<int> fvec;
        std::vector<fvec> sv;
        sv += fvec(10), fvec(20), fvec(30);

        BOOST_TEST(test("10,20,30,", *(int_ << ','), sv));
        BOOST_TEST(test_delimited("10 , 20 , 30 , ", *(int_ << ','), sv, lit(" ")));
 
        fusion::vector<char, char> cc ('a', 'c');
        BOOST_TEST(test("ac", char_ << *(char_(' ') << ',') << char_, cc));
        BOOST_TEST(test_delimited("a c ", 
            char_ << *(char_(' ') << ',') << char_, cc, " "));
    }
    
    {
        using namespace boost::assign;
        using namespace boost::phoenix;
        using namespace boost::spirit::arg_names;
        
        std::vector<int> v;

        BOOST_TEST(!test("", +int_, v));
        BOOST_TEST(!test_delimited("", +int_, v, lit(", ")));

        v += 10, 20, 30;

        BOOST_TEST(test("102030", +int_, v));
        BOOST_TEST(test_delimited("10, 20, 30, ", +int_, v, lit(", ")));

        typedef fusion::vector<int> fvec;
        std::vector<fvec> sv;
        sv += fvec(10), fvec(20), fvec(30);

        BOOST_TEST(test("10,20,30,", +(int_ << ','), sv));
        BOOST_TEST(test_delimited("10 , 20 , 30 , ", +(int_ << ','), sv, lit(" ")));

        fusion::vector<char, char> cc ('a', 'c');
        BOOST_TEST(!test("", char_ << +(char_(' ') << ',') << char_, cc));
        BOOST_TEST(!test_delimited("", 
            char_ << +(char_(' ') << ',') << char_, cc, " "));
    }

    return boost::report_errors();
}

