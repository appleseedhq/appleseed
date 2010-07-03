// Copyright Alexander Nasonov 2007-2008
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>
#include <ostream>
#include <vector>

#include <boost/scope_exit.hpp>

#include <boost/rational.hpp>
#include <boost/typeof/typeof.hpp>
#include <boost/typeof/std/vector.hpp>
#include <boost/test/unit_test.hpp>

using namespace boost::unit_test;

template<class type>
void tpl_long( type tval
             , type& t
             , type const& tc
             , type volatile& tv
             , type const volatile& tcv
             )
{
    int i = 0; // non-dependent name
    type const remember(tval);

    {
        BOOST_SCOPE_EXIT_TPL( (&tval)(&t)(&tc)(&tv)(&tcv)(&i) )
        {
            tval = 1;
            ++t;
            ++tv;
        }
        BOOST_SCOPE_EXIT_END

        BOOST_CHECK(t == remember);
        BOOST_CHECK(tval == remember);
    }

    BOOST_CHECK(tval == 1);
    BOOST_CHECK(t == remember + 2);
}

template<class Vector, int Value>
void tpl_vector( Vector vval
               , Vector& v
               , Vector const& vc
               )
{
    Vector const remember(vval);

    {
        BOOST_SCOPE_EXIT_TPL( (&vval)(&v)(&vc) )
        {
            v.push_back(-Value);
            vval.push_back(Value);
        }
        BOOST_SCOPE_EXIT_END

        BOOST_CHECK(v.size() == remember.size());
        BOOST_CHECK(vval.size() == remember.size());
    }

    BOOST_CHECK(v.size() == 1 + remember.size());
    BOOST_CHECK(vval.size() == 1 + remember.size());
}

void test_tpl()
{
    long l = 137;
    tpl_long(l, l, l, l, l);

    std::vector<int> v(10, 137);
    tpl_vector<std::vector<int>, 13>(v, v, v);
}

test_suite* init_unit_test_suite( int, char* [] )
{
    framework::master_test_suite().p_name.value = "Unit test for ScopeExit TPL";
    framework::master_test_suite().add( BOOST_TEST_CASE( &test_tpl ));
    return 0;
}

