/*=============================================================================
    Copyright (c) 2001-2007 Joel de Guzman
    Copyright (c) 2001-2009 Hartmut Kaiser

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi_char.hpp>
#include <boost/spirit/include/qi_auxiliary.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/spirit/include/support_argument.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <iostream>
#include "test.hpp"

///////////////////////////////////////////////////////////////////////////////
struct number_parser : public boost::spirit::qi::functor_base
{
    template <typename Context, typename Iterator>
    struct apply
    {
        typedef int type;
    };

    template <typename Attribute, typename Iterator, typename Context>
    bool operator()(Attribute& attr, Context& ctx,
        Iterator& first, Iterator const& last) const
    {
        if (first == last)
            return false;

        char ch = *first;
        if (ch < '0' || ch > '9')
            return false;

        attr = 0;
        do {
            attr = attr * 10 + int(ch - '0');
            ++first;
        } while (first != last && (ch = *first, ch >= '0' && ch <= '9'));
        return true;
    }
};

boost::spirit::qi::functor_parser<number_parser> number;

///////////////////////////////////////////////////////////////////////////////
int main()
{
    using spirit_test::test;
    using namespace boost::spirit;
    using namespace boost::spirit::qi;

    {
        using namespace boost::phoenix;
        using boost::spirit::arg_names::_1;

        int n = 0;
        BOOST_TEST(test("1234", number));
        BOOST_TEST(test("1234", number[ref(n) = _1]));
        BOOST_TEST(n == 1234);
    }

    return boost::report_errors();
}
