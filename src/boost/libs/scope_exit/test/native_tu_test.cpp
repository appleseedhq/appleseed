// Copyright Alexander Nasonov 2007-2008
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#include "tu_test.hpp"

#include <boost/test/unit_test.hpp>

using namespace boost::unit_test;

void test()
{
    BOOST_CHECK(tu1() == 1);
    BOOST_CHECK(tu2() == 2);
}

test_suite* init_unit_test_suite( int, char* [] )
{
    framework::master_test_suite().p_name.value = "TU unit test for ScopeExit";
    framework::master_test_suite().add( BOOST_TEST_CASE( &test ));
    return 0;
}
