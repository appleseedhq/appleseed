//  boost integer_mask.hpp test program  -------------------------------------//

//  (C) Copyright Daryle Walker 2001.
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org for most recent version including documentation.

//  Revision History
//  23 Sep 2001  Initial version (Daryle Walker)

#include <boost/test/minimal.hpp>  // for main

#include <boost/cstdlib.hpp>               // for boost::exit_success
#include <boost/integer/integer_mask.hpp>  // for boost::high_bit_mask_t, etc.

#include <iostream>  // for std::cout (std::endl indirectly)


#define PRIVATE_HIGH_BIT_SLOW_TEST(v)  BOOST_CHECK( ::boost::high_bit_mask_t< \
 (v) >::high_bit == (1ul << (v)) );
#define PRIVATE_HIGH_BIT_FAST_TEST(v)  BOOST_CHECK( ::boost::high_bit_mask_t< \
 (v) >::high_bit_fast == (1ul << (v)) );
#define PRIVATE_HIGH_BIT_TEST(v)  do { PRIVATE_HIGH_BIT_SLOW_TEST(v); \
 PRIVATE_HIGH_BIT_FAST_TEST(v); } while (false)

#define PRIVATE_LOW_BITS_SLOW_TEST(v)  BOOST_CHECK( ::boost::low_bits_mask_t< \
 (v) >::sig_bits == ((1ul << (v)) - 1) );
#define PRIVATE_LOW_BITS_FAST_TEST(v)  BOOST_CHECK( ::boost::low_bits_mask_t< \
 (v) >::sig_bits_fast == ((1ul << (v)) - 1) );
#define PRIVATE_LOW_BITS_TEST(v)  do { PRIVATE_LOW_BITS_SLOW_TEST(v); \
 PRIVATE_LOW_BITS_FAST_TEST(v); } while (false)


int test_main( int, char*[] )
{
    using std::cout;
    using std::endl;

    cout << "Doing high_bit_mask_t tests." << endl;
    PRIVATE_HIGH_BIT_TEST( 31 );
    PRIVATE_HIGH_BIT_TEST( 30 );
    PRIVATE_HIGH_BIT_TEST( 29 );
    PRIVATE_HIGH_BIT_TEST( 28 );
    PRIVATE_HIGH_BIT_TEST( 27 );
    PRIVATE_HIGH_BIT_TEST( 26 );
    PRIVATE_HIGH_BIT_TEST( 25 );
    PRIVATE_HIGH_BIT_TEST( 24 );
    PRIVATE_HIGH_BIT_TEST( 23 );
    PRIVATE_HIGH_BIT_TEST( 22 );
    PRIVATE_HIGH_BIT_TEST( 21 );
    PRIVATE_HIGH_BIT_TEST( 20 );
    PRIVATE_HIGH_BIT_TEST( 19 );
    PRIVATE_HIGH_BIT_TEST( 18 );
    PRIVATE_HIGH_BIT_TEST( 17 );
    PRIVATE_HIGH_BIT_TEST( 16 );
    PRIVATE_HIGH_BIT_TEST( 15 );
    PRIVATE_HIGH_BIT_TEST( 14 );
    PRIVATE_HIGH_BIT_TEST( 13 );
    PRIVATE_HIGH_BIT_TEST( 12 );
    PRIVATE_HIGH_BIT_TEST( 11 );
    PRIVATE_HIGH_BIT_TEST( 10 );
    PRIVATE_HIGH_BIT_TEST(  9 );
    PRIVATE_HIGH_BIT_TEST(  8 );
    PRIVATE_HIGH_BIT_TEST(  7 );
    PRIVATE_HIGH_BIT_TEST(  6 );
    PRIVATE_HIGH_BIT_TEST(  5 );
    PRIVATE_HIGH_BIT_TEST(  4 );
    PRIVATE_HIGH_BIT_TEST(  3 );
    PRIVATE_HIGH_BIT_TEST(  2 );
    PRIVATE_HIGH_BIT_TEST(  1 );
    PRIVATE_HIGH_BIT_TEST(  0 );

    cout << "Doing low_bits_mask_t tests." << endl;
    PRIVATE_LOW_BITS_TEST( 32 );  // Undefined behavior?  Whoops!
    PRIVATE_LOW_BITS_TEST( 31 );
    PRIVATE_LOW_BITS_TEST( 30 );
    PRIVATE_LOW_BITS_TEST( 29 );
    PRIVATE_LOW_BITS_TEST( 28 );
    PRIVATE_LOW_BITS_TEST( 27 );
    PRIVATE_LOW_BITS_TEST( 26 );
    PRIVATE_LOW_BITS_TEST( 25 );
    PRIVATE_LOW_BITS_TEST( 24 );
    PRIVATE_LOW_BITS_TEST( 23 );
    PRIVATE_LOW_BITS_TEST( 22 );
    PRIVATE_LOW_BITS_TEST( 21 );
    PRIVATE_LOW_BITS_TEST( 20 );
    PRIVATE_LOW_BITS_TEST( 19 );
    PRIVATE_LOW_BITS_TEST( 18 );
    PRIVATE_LOW_BITS_TEST( 17 );
    PRIVATE_LOW_BITS_TEST( 16 );
    PRIVATE_LOW_BITS_TEST( 15 );
    PRIVATE_LOW_BITS_TEST( 14 );
    PRIVATE_LOW_BITS_TEST( 13 );
    PRIVATE_LOW_BITS_TEST( 12 );
    PRIVATE_LOW_BITS_TEST( 11 );
    PRIVATE_LOW_BITS_TEST( 10 );
    PRIVATE_LOW_BITS_TEST(  9 );
    PRIVATE_LOW_BITS_TEST(  8 );
    PRIVATE_LOW_BITS_TEST(  7 );
    PRIVATE_LOW_BITS_TEST(  6 );
    PRIVATE_LOW_BITS_TEST(  5 );
    PRIVATE_LOW_BITS_TEST(  4 );
    PRIVATE_LOW_BITS_TEST(  3 );
    PRIVATE_LOW_BITS_TEST(  2 );
    PRIVATE_LOW_BITS_TEST(  1 );

    return boost::exit_success;
}
