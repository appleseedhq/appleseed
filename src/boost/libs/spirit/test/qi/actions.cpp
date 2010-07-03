/*=============================================================================
    Copyright (c) 2001-2008 Joel de Guzman

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/bind.hpp>
#include <cstring>

using namespace boost::spirit;

int x = 0;

void fun1(int const& i)
{
    x += i;
}

void fun2(int i)
{
    x += i;
}

struct fun_action
{
    void operator()(int const& i, unused_type, unused_type) const
    {
        x += i;
    }
};

int main()
{
    {
        char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
        qi::parse(s1, e1, '{' >> int_[&fun1] >> '}');
    }

    {
        char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
        qi::parse(s1, e1, '{' >> int_[&fun2] >> '}');
    }

    {
        // $$$ uncomment me! $$$
        //~ char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
        //~ qi::parse(s1, e1, '{' >> int_[fun2] >> '}');

        //~ int ii;
        //~ boost::spirit::detail::action_dispatch(fun2, ii, ii);
    }

    {
        char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
        qi::parse(s1, e1, '{' >> int_[fun_action()] >> '}');
    }

    {
        char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
        qi::parse(s1, e1, '{' >> int_[boost::bind(&fun1, _1)] >> '}');
    }

    {
        namespace lambda = boost::lambda;
        char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
        qi::parse(s1, e1, '{' >> int_[lambda::var(x) += lambda::_1] >> '}');
    }

    BOOST_TEST(x == (42*6));
    return 0;
}




