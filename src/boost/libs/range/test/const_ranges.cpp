// Boost.Range library
//
//  Copyright Thorsten Ottosen 2003-2004. Use, modification and
//  distribution is subject to the Boost Software License, Version
//  1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// For more information, see http://www.boost.org/libs/range/
//


#include <boost/detail/workaround.hpp>

#if BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x564))
#  pragma warn -8091 // supress warning in Boost.Test
#  pragma warn -8057 // unused argument argc/argv in Boost.Test
#endif

#include <boost/range.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test.hpp>
#include <string>

using namespace boost;
using namespace std;

template< class T >
const T& as_const( const T& r )
{
    return r;
}

void check_const_ranges()
{
    std::string       foo( "foo" );
    const std::string bar( "bar" ); 
    
    BOOST_CHECK( const_begin( foo )  == begin( as_const( foo ) ) );
    BOOST_CHECK( const_end( foo )    == end( as_const( foo ) ) );
    BOOST_CHECK( const_rbegin( foo ) == rbegin( as_const( foo ) ) );
    BOOST_CHECK( const_rend( foo )   == rend( as_const( foo ) ) );

    BOOST_CHECK( const_begin( bar )  == begin( as_const( bar ) ) );
    BOOST_CHECK( const_end( bar )    == end( as_const( bar ) ) );
    BOOST_CHECK( const_rbegin( bar ) == rbegin( as_const( bar ) ) );
    BOOST_CHECK( const_rend( bar )   == rend( as_const( bar ) ) );

}



using boost::unit_test::test_suite;

test_suite* init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Range Test Suite" );

    test->add( BOOST_TEST_CASE( &check_const_ranges ) );

    return test;
}





