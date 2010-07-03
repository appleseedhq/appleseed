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

#include <boost/range/rbegin.hpp>
#include <boost/range/rend.hpp>
#include <boost/range/begin.hpp>
#include <boost/range/end.hpp>
#include <boost/static_assert.hpp>
#include <boost/type_traits.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test.hpp>
#include <vector>
#include <algorithm>

using namespace boost;
using namespace std;

void check_iterator()
{
    typedef vector<int>                           vec_t;
    typedef vec_t::iterator                       iterator;
    typedef pair<iterator,iterator>               pair_t;
    typedef range_reverse_iterator<pair_t>::type  rev_iterator;
    typedef pair<rev_iterator,rev_iterator>       rev_pair_t;
    
    vec_t                            vec;
    pair_t                           p    = make_pair( vec.begin(), vec.end() );
    rev_pair_t                       rp   = make_pair( rbegin( p ), rend( p ) );
    int                             a[]  = {1,2,3,4,5,6,7,8,9,10};
    const int                       ca[] = {1,2,3,4,5,6,7,8,9,10,11,12};                        
    BOOST_CHECK( rbegin( vec ) == range_reverse_iterator<vec_t>::type( vec.end() ) );
    BOOST_CHECK( rend( vec ) == range_reverse_iterator<vec_t>::type( vec.begin() ) );
    BOOST_CHECK( std::distance( rbegin( vec ), rend( vec ) ) == std::distance( begin( vec ), end( vec ) ) );

    BOOST_CHECK( rbegin( p ) == begin( rp ) );
    BOOST_CHECK( rend( p ) == end( rp ) );
    BOOST_CHECK( std::distance( rbegin( p ), rend( p ) ) == std::distance( begin( rp ), end( rp ) ) );
    BOOST_CHECK( std::distance( begin( p ), end( p ) ) == std::distance( rbegin( rp ), rend( rp ) ) );


    BOOST_CHECK_EQUAL( &*begin( a ), &*( rend( a ) - 1 ) );
    BOOST_CHECK_EQUAL( &*( end( a ) - 1 ), &*rbegin( a ) );
    BOOST_CHECK_EQUAL( &*begin( ca ), &*( rend( ca ) - 1 ) );
    BOOST_CHECK_EQUAL( &*( end( ca ) - 1 ), &*rbegin( ca ) );
}


using boost::unit_test::test_suite;

test_suite* init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Range Test Suite" );

    test->add( BOOST_TEST_CASE( &check_iterator ) );

    return test;
}







