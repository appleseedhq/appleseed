/*=============================================================================
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/karma_operator.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_string.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_nonterminal.hpp>
#include <boost/spirit/include/karma_generate.hpp>

#include "test.hpp"

using namespace boost::spirit;
using namespace boost::spirit::karma;
using namespace boost::spirit::ascii;

struct num_list : grammar<char const*, rule<char const*> >
{
    num_list()
    {
        using boost::spirit::int_;
        start = int_(1) << ',' << int_(0);
    }

    rule<char const*, rule<char const*> > start;
};

// this test must fail compiling
int main()
{
    using boost::make_function_output_iterator;
    using spirit_test::make_string_appender;
    
    std::string generated;
    
    num_list def;
    bool r = generate_delimited(
                make_function_output_iterator(make_string_appender(generated)), 
                make_generator(def), char_('%') << '\n');

    return 0;
}
