// Boost.Range library
//
//  Copyright Thorsten Ottosen & Larry Evans 2003-2005. Use, modification and
//  distribution is subject to the Boost Software License, Version
//  1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// For more information, see http://www.boost.org/libs/range/
//

//#include <boost/range/as_array.hpp>

#include <boost/detail/workaround.hpp>

#if BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x564))
#  pragma warn -8091 // supress warning in Boost.Test
#  pragma warn -8057 // unused argument argc/argv in Boost.Test
#endif

#include <boost/range/iterator_range.hpp>
#include <boost/range/functions.hpp>
#include <boost/range/as_literal.hpp> 
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test.hpp>
#include <iostream>
#include <string>

using namespace boost;
using namespace std;

void check_reference_type();

void check_iterator_range()
{
   
    typedef string::iterator               iterator;
    typedef string::const_iterator         const_iterator;
    typedef iterator_range<iterator>       irange;
    typedef iterator_range<const_iterator> cirange;
    string       str  = "hello world";
    const string cstr = "const world";
    irange r    = make_iterator_range( str );
    r           = make_iterator_range( str.begin(), str.end() );
    cirange r2  = make_iterator_range( cstr );
    r2          = make_iterator_range( cstr.begin(), cstr.end() );
    r2          = make_iterator_range( str );
 
    BOOST_CHECK( !r.empty() );
    BOOST_CHECK( !r2.empty() );

//#if BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x564))
//    if( !(bool)r )
//        BOOST_CHECK( false );
//    if( !(bool)r2 )
//        BOOST_CHECK( false );
//#else    
    if( !r )
        BOOST_CHECK( false );
    if( !r2 )
        BOOST_CHECK( false );
//#endif

    BOOST_CHECK_EQUAL( r.size(), size( r ) );
    BOOST_CHECK_EQUAL( r2.size(), size( r2 ) );
    
    BOOST_CHECK_EQUAL( distance( r.begin(), r.end() ), 
                       distance( begin( r2 ), end( r2 ) ) );
    cout << r << r2;

        
#ifndef BOOST_NO_STD_WSTRING
    wcout << make_iterator_range( wstring( L"a wide string" ) ) 
          << make_iterator_range( L"another wide string" );
#endif    
    
    string res  = copy_range<string>( r );
    BOOST_CHECK( equal( res.begin(), res.end(), r.begin() ) );

    irange rr = make_iterator_range( str );
    BOOST_CHECK( rr.equal( r ) );

    rr  = make_iterator_range( str.begin(), str.begin() + 5 );
    BOOST_CHECK( rr == as_literal("hello") );
    BOOST_CHECK( rr != as_literal("hell") );
    BOOST_CHECK( rr < as_literal("hello dude") );
    BOOST_CHECK( as_literal("hello") == rr );
    BOOST_CHECK( as_literal("hell")  != rr );
    BOOST_CHECK( ! (as_literal("hello dude") < rr ) );
    irange rrr = rr;
    BOOST_CHECK( rrr == rr );
    BOOST_CHECK( !( rrr != rr ) );
    BOOST_CHECK( !( rrr < rr ) );

    const irange cr = make_iterator_range( str );
    BOOST_CHECK_EQUAL( cr.front(), 'h' );
    BOOST_CHECK_EQUAL( cr.back(), 'd' );
    BOOST_CHECK_EQUAL( cr[1], 'e' );
    BOOST_CHECK_EQUAL( cr(1), 'e' );

    rrr = make_iterator_range( str, 1, -1 );
    BOOST_CHECK( rrr == as_literal("ello worl") );
    rrr = make_iterator_range( rrr, -1, 1 );
    BOOST_CHECK( rrr == str );

    check_reference_type();
}


using boost::unit_test::test_suite;

test_suite* init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test = BOOST_TEST_SUITE( "Range Test Suite" );

    test->add( BOOST_TEST_CASE( &check_iterator_range ) );

    return test;
}


//
//
// Check that constness is propgated correct from
// the iterator types.
// 
// Test contributed by Larry Evans.
// 

template< class Container >
int test_iter_range( Container& a_cont )
{
    typedef BOOST_DEDUCED_TYPENAME range_iterator<Container>::type citer_type;
    typedef iterator_range<citer_type> riter_type;
    riter_type a_riter( make_iterator_range( a_cont ) );
    a_riter.front();
    a_riter.back();
    int i = a_riter[0];
    return i;
}



void check_reference_type()
{
    typedef vector<int> veci_type;
    veci_type a_vec;
    a_vec.push_back( 999 );
    test_iter_range<veci_type>(a_vec);
    test_iter_range<veci_type const>(a_vec);
}
