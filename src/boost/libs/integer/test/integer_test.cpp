//  boost integer.hpp test program  ------------------------------------------//

//  Copyright Beman Dawes 1999.  Distributed under the Boost
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


//  See http://www.boost.org/libs/integer for documentation.

//  Revision History
//   04 Oct 01  Added tests for new templates; rewrote code (Daryle Walker)
//   10 Mar 01  Boost Test Library now used for tests (Beman Dawes)
//   31 Aug 99  Initial version

#include <boost/test/minimal.hpp>  // for main, BOOST_CHECK

#include <boost/config.hpp>   // for BOOST_NO_USING_TEMPLATE
#include <boost/cstdlib.hpp>  // for boost::exit_success
#include <boost/integer.hpp>  // for boost::int_t, boost::uint_t

#include <climits>   // for ULONG_MAX, LONG_MAX, LONG_MIN
#include <iostream>  // for std::cout (std::endl indirectly)
#include <typeinfo>  // for std::type_info


// Control if the names of the types for each version
// of the integer templates will be printed.
#ifndef CONTROL_SHOW_TYPES
#define CONTROL_SHOW_TYPES  0
#endif


// If specializations have not already been done, then we can confirm
// the effects of the "fast" types by making a specialization.
namespace boost
{
    template < >
    struct int_fast_t< short >
    {
        typedef long  fast;
    };
}


// Show the types of an integer template version
#if CONTROL_SHOW_TYPES
#define SHOW_TYPE(Template, Number, Type)  ::std::cout << "Type \"" \
 #Template "<" #Number ">::" #Type "\" is \"" << typeid(Template <  \
 Number > :: Type).name() << ".\"\n"
#else
#define SHOW_TYPE(Template, Number, Type)
#endif

#define SHOW_TYPES(Template, Type)  SHOW_TYPE(Template, 32, Type); \
 SHOW_TYPE(Template, 31, Type); SHOW_TYPE(Template, 30, Type); \
 SHOW_TYPE(Template, 29, Type); SHOW_TYPE(Template, 28, Type); \
 SHOW_TYPE(Template, 27, Type); SHOW_TYPE(Template, 26, Type); \
 SHOW_TYPE(Template, 25, Type); SHOW_TYPE(Template, 24, Type); \
 SHOW_TYPE(Template, 23, Type); SHOW_TYPE(Template, 22, Type); \
 SHOW_TYPE(Template, 21, Type); SHOW_TYPE(Template, 20, Type); \
 SHOW_TYPE(Template, 19, Type); SHOW_TYPE(Template, 18, Type); \
 SHOW_TYPE(Template, 17, Type); SHOW_TYPE(Template, 16, Type); \
 SHOW_TYPE(Template, 15, Type); SHOW_TYPE(Template, 14, Type); \
 SHOW_TYPE(Template, 13, Type); SHOW_TYPE(Template, 12, Type); \
 SHOW_TYPE(Template, 11, Type); SHOW_TYPE(Template, 10, Type); \
 SHOW_TYPE(Template, 9, Type); SHOW_TYPE(Template, 8, Type); \
 SHOW_TYPE(Template, 7, Type); SHOW_TYPE(Template, 6, Type); \
 SHOW_TYPE(Template, 5, Type); SHOW_TYPE(Template, 4, Type); \
 SHOW_TYPE(Template, 3, Type); SHOW_TYPE(Template, 2, Type); \
 SHOW_TYPE(Template, 1, Type); SHOW_TYPE(Template, 0, Type)

#define SHOW_SHIFTED_TYPE(Template, Number, Type)  SHOW_TYPE(Template, (1UL << Number), Type)

#define SHOW_SHIFTED_TYPES(Template, Type)  SHOW_SHIFTED_TYPE(Template, 30, Type); \
 SHOW_SHIFTED_TYPE(Template, 29, Type); SHOW_SHIFTED_TYPE(Template, 28, Type); \
 SHOW_SHIFTED_TYPE(Template, 27, Type); SHOW_SHIFTED_TYPE(Template, 26, Type); \
 SHOW_SHIFTED_TYPE(Template, 25, Type); SHOW_SHIFTED_TYPE(Template, 24, Type); \
 SHOW_SHIFTED_TYPE(Template, 23, Type); SHOW_SHIFTED_TYPE(Template, 22, Type); \
 SHOW_SHIFTED_TYPE(Template, 21, Type); SHOW_SHIFTED_TYPE(Template, 20, Type); \
 SHOW_SHIFTED_TYPE(Template, 19, Type); SHOW_SHIFTED_TYPE(Template, 18, Type); \
 SHOW_SHIFTED_TYPE(Template, 17, Type); SHOW_SHIFTED_TYPE(Template, 16, Type); \
 SHOW_SHIFTED_TYPE(Template, 15, Type); SHOW_SHIFTED_TYPE(Template, 14, Type); \
 SHOW_SHIFTED_TYPE(Template, 13, Type); SHOW_SHIFTED_TYPE(Template, 12, Type); \
 SHOW_SHIFTED_TYPE(Template, 11, Type); SHOW_SHIFTED_TYPE(Template, 10, Type); \
 SHOW_SHIFTED_TYPE(Template, 9, Type); SHOW_SHIFTED_TYPE(Template, 8, Type); \
 SHOW_SHIFTED_TYPE(Template, 7, Type); SHOW_SHIFTED_TYPE(Template, 6, Type); \
 SHOW_SHIFTED_TYPE(Template, 5, Type); SHOW_SHIFTED_TYPE(Template, 4, Type); \
 SHOW_SHIFTED_TYPE(Template, 3, Type); SHOW_SHIFTED_TYPE(Template, 2, Type); \
 SHOW_SHIFTED_TYPE(Template, 1, Type); SHOW_SHIFTED_TYPE(Template, 0, Type)

#define SHOW_POS_SHIFTED_TYPE(Template, Number, Type)  SHOW_TYPE(Template, +(1L << Number), Type)

#define SHOW_POS_SHIFTED_TYPES(Template, Type)  SHOW_POS_SHIFTED_TYPE(Template, 30, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 29, Type); SHOW_POS_SHIFTED_TYPE(Template, 28, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 27, Type); SHOW_POS_SHIFTED_TYPE(Template, 26, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 25, Type); SHOW_POS_SHIFTED_TYPE(Template, 24, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 23, Type); SHOW_POS_SHIFTED_TYPE(Template, 22, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 21, Type); SHOW_POS_SHIFTED_TYPE(Template, 20, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 19, Type); SHOW_POS_SHIFTED_TYPE(Template, 18, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 17, Type); SHOW_POS_SHIFTED_TYPE(Template, 16, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 15, Type); SHOW_POS_SHIFTED_TYPE(Template, 14, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 13, Type); SHOW_POS_SHIFTED_TYPE(Template, 12, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 11, Type); SHOW_POS_SHIFTED_TYPE(Template, 10, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 9, Type); SHOW_POS_SHIFTED_TYPE(Template, 8, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 7, Type); SHOW_POS_SHIFTED_TYPE(Template, 6, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 5, Type); SHOW_POS_SHIFTED_TYPE(Template, 4, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 3, Type); SHOW_POS_SHIFTED_TYPE(Template, 2, Type); \
 SHOW_POS_SHIFTED_TYPE(Template, 1, Type); SHOW_POS_SHIFTED_TYPE(Template, 0, Type)

#define SHOW_NEG_SHIFTED_TYPE(Template, Number, Type)  SHOW_TYPE(Template, -(1L << Number), Type)

#define SHOW_NEG_SHIFTED_TYPES(Template, Type)  SHOW_NEG_SHIFTED_TYPE(Template, 30, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 29, Type); SHOW_NEG_SHIFTED_TYPE(Template, 28, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 27, Type); SHOW_NEG_SHIFTED_TYPE(Template, 26, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 25, Type); SHOW_NEG_SHIFTED_TYPE(Template, 24, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 23, Type); SHOW_NEG_SHIFTED_TYPE(Template, 22, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 21, Type); SHOW_NEG_SHIFTED_TYPE(Template, 20, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 19, Type); SHOW_NEG_SHIFTED_TYPE(Template, 18, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 17, Type); SHOW_NEG_SHIFTED_TYPE(Template, 16, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 15, Type); SHOW_NEG_SHIFTED_TYPE(Template, 14, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 13, Type); SHOW_NEG_SHIFTED_TYPE(Template, 12, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 11, Type); SHOW_NEG_SHIFTED_TYPE(Template, 10, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 9, Type); SHOW_NEG_SHIFTED_TYPE(Template, 8, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 7, Type); SHOW_NEG_SHIFTED_TYPE(Template, 6, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 5, Type); SHOW_NEG_SHIFTED_TYPE(Template, 4, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 3, Type); SHOW_NEG_SHIFTED_TYPE(Template, 2, Type); \
 SHOW_NEG_SHIFTED_TYPE(Template, 1, Type); SHOW_NEG_SHIFTED_TYPE(Template, 0, Type)


// Test if a constant can fit within a certain type
#define PRIVATE_FIT_TEST(Template, Number, Type, Value)  BOOST_CHECK( Template < Number > :: Type ( Value ) == Value )

#if ULONG_MAX > 0xFFFFFFFFL
#define PRIVATE_FIT_TESTS(Template, Type, ValType, InitVal)  do { ValType v = InitVal ; \
 PRIVATE_FIT_TEST(Template, 64, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 63, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 62, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 61, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 60, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 59, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 58, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 57, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 56, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 55, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 54, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 53, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 52, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 51, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 50, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 49, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 48, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 47, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 46, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 45, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 44, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 43, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 42, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 41, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 40, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 39, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 38, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 37, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 36, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 35, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 34, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 33, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 32, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 31, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 30, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 29, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 28, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 27, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 26, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 25, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 24, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 23, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 22, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 21, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 20, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 19, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 18, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 17, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 16, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 15, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 14, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 13, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 12, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 11, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 10, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 9, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 8, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 7, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 6, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 5, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 4, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 3, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 2, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 1, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 0, Type, v); } while ( false )
#else
#define PRIVATE_FIT_TESTS(Template, Type, ValType, InitVal)  do { ValType v = InitVal ; \
 PRIVATE_FIT_TEST(Template, 32, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 31, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 30, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 29, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 28, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 27, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 26, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 25, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 24, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 23, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 22, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 21, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 20, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 19, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 18, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 17, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 16, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 15, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 14, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 13, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 12, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 11, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 10, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 9, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 8, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 7, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 6, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 5, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 4, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 3, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 2, Type, v); v >>= 1; \
 PRIVATE_FIT_TEST(Template, 1, Type, v); v >>= 1; PRIVATE_FIT_TEST(Template, 0, Type, v); } while ( false )
#endif

#define PRIVATE_SHIFTED_FIT_TEST(Template, Number, Type, Value)  BOOST_CHECK( Template < (ULONG_MAX >> Number) > :: Type ( Value ) == Value )

#define PRIVATE_SHIFTED_FIT_TESTS(Template, Type, ValType, InitVal)  do { ValType v = InitVal ; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 0, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 1, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 2, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 3, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 4, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 5, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 6, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 7, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 8, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 9, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 10, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 11, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 12, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 13, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 14, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 15, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 16, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 17, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 18, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 19, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 20, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 21, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 22, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 23, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 24, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 25, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 26, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 27, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 28, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 29, Type, v); v >>= 1; \
 PRIVATE_SHIFTED_FIT_TEST(Template, 30, Type, v); v >>= 1; PRIVATE_SHIFTED_FIT_TEST(Template, 31, Type, v); } while ( false )

#define PRIVATE_POS_SHIFTED_FIT_TEST(Template, Number, Type, Value)  BOOST_CHECK( Template < (LONG_MAX >> Number) > :: Type ( Value ) == Value )

#define PRIVATE_POS_FIT_TESTS(Template, Type, ValType, InitVal)  do { ValType v = InitVal ; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 0, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 1, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 2, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 3, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 4, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 5, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 6, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 7, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 8, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 9, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 10, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 11, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 12, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 13, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 14, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 15, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 16, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 17, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 18, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 19, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 20, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 21, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 22, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 23, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 24, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 25, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 26, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 27, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 28, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 29, Type, v); v >>= 1; \
 PRIVATE_POS_SHIFTED_FIT_TEST(Template, 30, Type, v); v >>= 1; PRIVATE_POS_SHIFTED_FIT_TEST(Template, 31, Type, v); } while ( false )

#define PRIVATE_NEG_SHIFTED_FIT_TEST(Template, Number, Type, Value)  BOOST_CHECK( Template < (LONG_MIN >> Number) > :: Type ( Value ) == Value )

#define PRIVATE_NEG_FIT_TESTS(Template, Type, ValType, InitVal)  do { ValType v = InitVal ; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 0, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 1, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 2, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 3, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 4, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 5, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 6, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 7, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 8, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 9, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 10, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 11, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 12, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 13, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 14, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 15, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 16, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 17, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 18, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 19, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 20, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 21, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 22, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 23, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 24, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 25, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 26, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 27, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 28, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 29, Type, v); v >>= 1; \
 PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 30, Type, v); v >>= 1; PRIVATE_NEG_SHIFTED_FIT_TEST(Template, 31, Type, v); } while ( false )


// Test program
int
test_main
(
    int,
    char*[]
)
{
#ifndef BOOST_NO_USING_TEMPLATE
    using boost::int_t;
    using boost::uint_t;
    using boost::int_max_value_t;
    using boost::int_min_value_t;
    using boost::uint_value_t;
#else
    using namespace boost;
#endif

    SHOW_TYPES( int_t, least );
    SHOW_TYPES( int_t, fast );
    SHOW_TYPES( uint_t, least );
    SHOW_TYPES( uint_t, fast );
    SHOW_POS_SHIFTED_TYPES( int_max_value_t, least );
    SHOW_POS_SHIFTED_TYPES( int_max_value_t, fast );
    SHOW_NEG_SHIFTED_TYPES( int_min_value_t, least );
    SHOW_NEG_SHIFTED_TYPES( int_min_value_t, fast );
    SHOW_SHIFTED_TYPES( uint_value_t, least );
    SHOW_SHIFTED_TYPES( uint_value_t, fast );
     
    PRIVATE_FIT_TESTS( int_t, least, long, LONG_MAX );
    PRIVATE_FIT_TESTS( int_t, fast, long, LONG_MAX );
    PRIVATE_FIT_TESTS( uint_t, least, unsigned long, ULONG_MAX );
    PRIVATE_FIT_TESTS( uint_t, fast, unsigned long, ULONG_MAX );
    PRIVATE_POS_FIT_TESTS( int_max_value_t, least, long, LONG_MAX );
    PRIVATE_POS_FIT_TESTS( int_max_value_t, fast, long, LONG_MAX );
    PRIVATE_NEG_FIT_TESTS( int_min_value_t, least, long, LONG_MIN );
    PRIVATE_NEG_FIT_TESTS( int_min_value_t, fast, long, LONG_MIN );
    PRIVATE_SHIFTED_FIT_TESTS( uint_value_t, least, unsigned long, ULONG_MAX );
    PRIVATE_SHIFTED_FIT_TESTS( uint_value_t, fast, unsigned long, ULONG_MAX );
        
    return boost::exit_success;
}
