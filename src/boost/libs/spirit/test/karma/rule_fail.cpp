/*=============================================================================
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/karma_operator.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_numeric.hpp>
#include <boost/spirit/include/karma_nonterminal.hpp>
#include <boost/spirit/include/karma_generate.hpp>
#include <boost/function_output_iterator.hpp>

#include "test.hpp"

using namespace boost::spirit;
using namespace boost::spirit::karma;
using namespace boost::spirit::ascii;

// this test must fail compiling
int main()
{
    using boost::make_function_output_iterator;
    using spirit_test::make_string_appender;
    
    std::string generated;
    
    rule<char const*, rule<char const*> > def;
    def = int_(1) << ',' << int_(0);
    
    bool r = generate_delimited(
                make_function_output_iterator(make_string_appender(generated)), 
                def, char_('%') << '\n');

    return 0;
}
