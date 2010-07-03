//  Copyright Xiaogang Zhang 2006
//  Copyright John Maddock 2006, 2007
//  Copyright Paul A. Bristow 2007

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifdef _MSC_VER
#  pragma warning(disable : 4756) // overflow in constant arithmetic
// Constants are too big for float case, but this doesn't matter for test.
#endif

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/ellint_2.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the Elliptic Integrals of the second kind.
// There are two sets of tests, spot
// tests which compare our results with selected values computed
// using the online special function calculator at
// functions.wolfram.com, while the bulk of the accuracy tests
// use values generated with NTL::RR at 1000-bit precision
// and our generic versions of these functions.
//
// Note that when this file is first run on a new platform many of
// these tests will fail: the default accuracy is 1 epsilon which
// is too tight for most platforms.  In this situation you will
// need to cast a human eye over the error rates reported and make
// a judgement as to whether they are acceptable.  Either way please
// report the results to the Boost mailing list.  Acceptable rates of
// error are marked up below as a series of regular expressions that
// identify the compiler/stdlib/platform/data-type/test-data/test-function
// along with the maximum expected peek and RMS mean errors for that
// test.
//

void expected_results()
{
   //
   // Define the max and mean errors expected for
   // various compilers and platforms.
   //
   const char* largest_type;
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   if(boost::math::policies::digits<double, boost::math::policies::policy<> >() == boost::math::policies::digits<long double, boost::math::policies::policy<> >())
   {
      largest_type = "(long\\s+)?double";
   }
   else
   {
      largest_type = "long double";
   }
#else
   largest_type = "(long\\s+)?double";
#endif

   //
   // Catch all cases come last:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",      // test data group
      ".*", 15, 6);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                  // test type(s)
      ".*",      // test data group
      ".*", 15, 6);  // test function
   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", "
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}


template <typename T>
void do_test_ellint_e2(const T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
    value_type (*fp2)(value_type, value_type) = boost::math::ellint_2<value_type, value_type>;
#else
    value_type (*fp2)(value_type, value_type) = boost::math::ellint_2;
#endif
    boost::math::tools::test_result<value_type> result;

    result = boost::math::tools::test(
      data,
      bind_func(fp2, 1, 0),
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(),
      type_name, "boost::math::ellint_2", test);

   std::cout << std::endl;
}

template <typename T>
void do_test_ellint_e1(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;
    boost::math::tools::test_result<value_type> result;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   value_type (*fp1)(value_type) = boost::math::ellint_2<value_type>;
#else
   value_type (*fp1)(value_type) = boost::math::ellint_2;
#endif
   result = boost::math::tools::test(
      data,
      bind_func(fp1, 0),
      extract_result(1));
   handle_test_result(result, data[result.worst()], result.worst(),
      type_name, "boost::math::ellint_2", test);

   std::cout << std::endl;
}

template <typename T>
void test_spots(T, const char* type_name)
{
    // Function values calculated on http://functions.wolfram.com/
    // Note that Mathematica's EllipticE accepts k^2 as the second parameter.
    #define SC_(x) static_cast<T>(BOOST_JOIN(x, L))
    static const boost::array<boost::array<T, 3>, 10> data1 = {
        SC_(0), SC_(0), SC_(0),
        SC_(-10), SC_(0), SC_(-10),
        SC_(-1), SC_(-1), SC_(-0.84147098480789650665250232163029899962256306079837),
        SC_(-4), SC_(900) / 1024, SC_(-3.1756145986492562317862928524528520686391383168377),
        SC_(8), SC_(-600) / 1024, SC_(7.2473147180505693037677015377802777959345489333465),
        SC_(1e-05), SC_(800) / 1024, SC_(9.999999999898274739584436515967055859383969942432E-6),
        SC_(1e+05), SC_(100) / 1024, SC_(99761.153306972066658135668386691227343323331995888),
        SC_(1e+10), SC_(-0.5), SC_(9.3421545766487137036576748555295222252286528414669e9),
        ldexp(SC_(1), 66), SC_(400) / 1024, SC_(7.0886102721911705466476846969992069994308167515242e19),
        ldexp(SC_(1), 166), SC_(900) / 1024, SC_(7.1259011068364515942912094521783688927118026465790e49),
    };
    #undef SC_

    do_test_ellint_e2(data1, type_name, "Elliptic Integral E: Mathworld Data");

#include "ellint_e2_data.ipp"

    do_test_ellint_e2(ellint_e2_data, type_name, "Elliptic Integral E: Random Data");

    // Function values calculated on http://functions.wolfram.com/
    // Note that Mathematica's EllipticE accepts k^2 as the second parameter.
    #define SC_(x) static_cast<T>(BOOST_JOIN(x, L))
    static const boost::array<boost::array<T, 2>, 10> data2 = {
        SC_(-1), SC_(1),
        SC_(0), SC_(1.5707963267948966192313216916397514420985846996876),
        SC_(100) / 1024, SC_(1.5670445330545086723323795143598956428788609133377),
        SC_(200) / 1024, SC_(1.5557071588766556854463404816624361127847775545087),
        SC_(300) / 1024, SC_(1.5365278991162754883035625322482669608948678755743),
        SC_(400) / 1024, SC_(1.5090417763083482272165682786143770446401437564021),
        SC_(-0.5), SC_(1.4674622093394271554597952669909161360253617523272),
        SC_(-600) / 1024, SC_(1.4257538571071297192428217218834579920545946473778),
        SC_(-800) / 1024, SC_(1.2927868476159125056958680222998765985004489572909),
        SC_(-900) / 1024, SC_(1.1966864890248739524112920627353824133420353430982),
    };
    #undef SC_

    do_test_ellint_e1(data2, type_name, "Elliptic Integral E: Mathworld Data");

#include "ellint_e_data.ipp"

    do_test_ellint_e1(ellint_e_data, type_name, "Elliptic Integral E: Random Data");
}

int test_main(int, char* [])
{
    expected_results();
    BOOST_MATH_CONTROL_FP;
#ifndef BOOST_MATH_BUGGY_LARGE_FLOAT_CONSTANTS
    test_spots(0.0F, "float");
#endif
    test_spots(0.0, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
    test_spots(0.0L, "long double");
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
    test_spots(boost::math::concepts::real_concept(0), "real_concept");
#endif
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif

    return 0;
}
