// Copyright Alexander Nasonov 2007-2008
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>
#include <ostream>
#include <string>

#include <boost/scope_exit.hpp>

#include <boost/typeof/typeof.hpp>
#include <boost/typeof/std/string.hpp>
#include <boost/test/unit_test.hpp>

using namespace boost::unit_test;

std::string g_str;

template<int Dummy = 0>
struct Holder
{
    static long g_long;
};

template<int Dummy> long Holder<Dummy>::g_long;

void test_non_local()
{
    // ... and one local variable as well:
    int i = 0;

    BOOST_SCOPE_EXIT( (i) )
    {
        BOOST_CHECK(i == 0);
        BOOST_CHECK(Holder<>::g_long == 3);
        BOOST_CHECK(g_str == "try: g_str");
    } BOOST_SCOPE_EXIT_END

    BOOST_SCOPE_EXIT( (&i) )
    {
        BOOST_CHECK(i == 3);
        BOOST_CHECK(Holder<>::g_long == 3);
        BOOST_CHECK(g_str == "try: g_str");
    } BOOST_SCOPE_EXIT_END

    {
        g_str = "";
        Holder<>::g_long = 1;

        BOOST_SCOPE_EXIT( (&i) )
        {
            i = 1;
            g_str = "g_str";
        } BOOST_SCOPE_EXIT_END

        BOOST_SCOPE_EXIT( (&i) )
        {
          try
          {
              i = 2;
              Holder<>::g_long = 2;
              throw 0;
          } catch(...) {}
        } BOOST_SCOPE_EXIT_END

        BOOST_CHECK(i == 0);
        BOOST_CHECK(g_str == "");
        BOOST_CHECK(Holder<>::g_long == 1);
    }

    BOOST_CHECK(Holder<>::g_long == 2);
    BOOST_CHECK(g_str == "g_str");
    BOOST_CHECK(i == 1); // Check that first declared is executed last

    BOOST_SCOPE_EXIT( (&i) )
    {
        BOOST_CHECK(i == 3);
        BOOST_CHECK(Holder<>::g_long == 3);
        BOOST_CHECK(g_str == "try: g_str");
    } BOOST_SCOPE_EXIT_END

    BOOST_SCOPE_EXIT( (i) )
    {
        BOOST_CHECK(i == 1);
        BOOST_CHECK(Holder<>::g_long == 3);
        BOOST_CHECK(g_str == "try: g_str");
    } BOOST_SCOPE_EXIT_END

    try
    {
        BOOST_SCOPE_EXIT( (&i) )
        {
            i = 3;
            g_str = "try: g_str";
        } BOOST_SCOPE_EXIT_END
        
        BOOST_SCOPE_EXIT( (&i) )
        {
            i = 4;
            Holder<>::g_long = 3;
        } BOOST_SCOPE_EXIT_END

        BOOST_CHECK(i == 1);
        BOOST_CHECK(g_str == "g_str");
        BOOST_CHECK(Holder<>::g_long == 2);

        throw 0;
    }
    catch(int)
    {
        BOOST_CHECK(Holder<>::g_long == 3);
        BOOST_CHECK(g_str == "try: g_str");
        BOOST_CHECK(i == 3); // Check that first declared is executed last
    }
}

bool foo()
{
    return true;
}

void test_types()
{
    bool (*pf)() = 0;
    bool (&rf)() = foo;
    bool results[2] = {};

    {
        BOOST_SCOPE_EXIT( (&results)(&pf)(&rf) )
        {
            results[0] = pf();
            results[1] = rf();
        }
        BOOST_SCOPE_EXIT_END

        pf = &foo;

        BOOST_CHECK(results[0] == false);
        BOOST_CHECK(results[1] == false);
    }

    BOOST_CHECK(results[0] == true);
    BOOST_CHECK(results[1] == true);

    {
        BOOST_SCOPE_EXIT( (&results)(pf) )
        {
            results[0] = !pf();
            results[1] = !pf();
        }
        BOOST_SCOPE_EXIT_END

        pf = 0;

        BOOST_CHECK(results[0] == true);
        BOOST_CHECK(results[1] == true);
    }

    BOOST_CHECK(results[0] == false);
    BOOST_CHECK(results[1] == false);
}

test_suite* init_unit_test_suite( int, char* [] )
{
    framework::master_test_suite().p_name.value = "Unit test for ScopeExit";
    framework::master_test_suite().add( BOOST_TEST_CASE( &test_non_local ));
    framework::master_test_suite().add( BOOST_TEST_CASE( &test_types     ));
    return 0;
}

