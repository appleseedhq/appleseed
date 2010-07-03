/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include <boost/config/warning_disable.hpp>
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/karma_char.hpp>
#include <boost/spirit/include/karma_auxiliary.hpp>
#include <boost/spirit/include/karma_action.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include "test.hpp"

///////////////////////////////////////////////////////////////////////////////
struct number_generator : public boost::spirit::karma::functor_base
{
    template <typename Context>
    struct apply
    {
        typedef int type;
    };

    template <typename Parameter, typename Context, typename OutputIterator>
    bool operator()(Parameter v, Context& ctx, OutputIterator& sink) const
    {
        char ch = v % 10 + '0';
        v /= 10;
        
        if (0 != v) 
            (*this)(v, ctx, sink);
            
        *sink = ch;
        ++sink;
        return true;
    }
};

boost::spirit::karma::functor_generator<number_generator> number;

///////////////////////////////////////////////////////////////////////////////
int main()
{
    using spirit_test::test;
    using namespace boost::spirit;
    using namespace boost::spirit::karma;

    {
        BOOST_TEST(test("0", number));
        BOOST_TEST(test("1234", number, 1234));
    }
        
    return boost::report_errors();
}
