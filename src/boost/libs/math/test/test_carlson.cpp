//  Copyright 2006 John Maddock
// Copyright Paul A. Bristow 2007.

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/ellint_rf.hpp>
#include <boost/math/special_functions/ellint_rc.hpp>
#include <boost/math/special_functions/ellint_rj.hpp>
#include <boost/math/special_functions/ellint_rd.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/array.hpp>
#include <boost/tr1/random.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the Carlson Elliptic Integrals.  
// There are two sets of tests, spot
// tests which compare our results with the published test values, 
// in Numerical Computation of Real or Complex Elliptic Integrals,
// B. C. Carlson: http://arxiv.org/abs/math.CA/9409227
// However, the bulk of the accuracy tests
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
   // real long doubles:
   //
   if(boost::math::policies::digits<long double, boost::math::policies::policy<> >() > 53)
   {
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         BOOST_PLATFORM,                          // platform
         largest_type,                  // test type(s)
         ".*RJ.*",      // test data group
         ".*", 1000, 50);  // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         BOOST_PLATFORM,                          // platform
         "real_concept",                  // test type(s)
         ".*RJ.*",      // test data group
         ".*", 1000, 50);  // test function
   }
   //
   // Catch all cases come last:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*RJ.*",      // test data group
      ".*", 180, 50);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                  // test type(s)
      ".*RJ.*",      // test data group
      ".*", 180, 50);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",      // test data group
      ".*", 15, 8);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                  // test type(s)
      ".*",      // test data group
      ".*", 15, 8);  // test function
   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <typename T>
void do_test_ellint_rf(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
    value_type (*fp)(value_type, value_type, value_type) = boost::math::ellint_rf<value_type, value_type, value_type>;
#else
    value_type (*fp)(value_type, value_type, value_type) = boost::math::ellint_rf;
#endif
    boost::math::tools::test_result<value_type> result;
 
    result = boost::math::tools::test(
      data, 
      bind_func(fp, 0, 1, 2),
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(), 
      type_name, "boost::math::ellint_rf", test);

   std::cout << std::endl;

}

template <typename T>
void do_test_ellint_rc(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
    value_type (*fp)(value_type, value_type) = boost::math::ellint_rc<value_type, value_type>;
#else
    value_type (*fp)(value_type, value_type) = boost::math::ellint_rc;
#endif
    boost::math::tools::test_result<value_type> result;
 
    result = boost::math::tools::test(
      data, 
      bind_func(fp, 0, 1),
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), 
      type_name, "boost::math::ellint_rc", test);

   std::cout << std::endl;

}

template <typename T>
void do_test_ellint_rj(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
    value_type (*fp)(value_type, value_type, value_type, value_type) = boost::math::ellint_rj<value_type, value_type, value_type, value_type>;
#else
    value_type (*fp)(value_type, value_type, value_type, value_type) = boost::math::ellint_rj;
#endif
    boost::math::tools::test_result<value_type> result;
 
    result = boost::math::tools::test(
      data, 
      bind_func(fp, 0, 1, 2, 3),
      extract_result(4));
   handle_test_result(result, data[result.worst()], result.worst(), 
      type_name, "boost::math::ellint_rf", test);

   std::cout << std::endl;

}

template <typename T>
void do_test_ellint_rd(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
    value_type (*fp)(value_type, value_type, value_type) = boost::math::ellint_rd<value_type, value_type, value_type>;
#else
    value_type (*fp)(value_type, value_type, value_type) = boost::math::ellint_rd;
#endif
    boost::math::tools::test_result<value_type> result;
 
    result = boost::math::tools::test(
      data, 
      bind_func(fp, 0, 1, 2),
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(), 
      type_name, "boost::math::ellint_rd", test);

   std::cout << std::endl;

}

template <typename T>
void test_spots(T, const char* type_name)
{
   using namespace boost::math;
   using namespace std;
   // Spot values from Numerical Computation of Real or Complex 
   // Elliptic Integrals, B. C. Carlson: http://arxiv.org/abs/math.CA/9409227
   // RF:
   T tolerance = (std::max)(T(1e-13f), tools::epsilon<T>() * 5) * 100; // Note 5eps expressed as a persentage!!!
   T eps2 = 2 * tools::epsilon<T>();
   BOOST_CHECK_CLOSE(ellint_rf(T(1), T(2), T(0)), T(1.3110287771461), tolerance);
   BOOST_CHECK_CLOSE(ellint_rf(T(0.5), T(1), T(0)), T(1.8540746773014), tolerance);
   BOOST_CHECK_CLOSE(ellint_rf(T(2), T(3), T(4)), T(0.58408284167715), tolerance);
   // RC:
   BOOST_CHECK_CLOSE_FRACTION(ellint_rc(T(0), T(1)/4), boost::math::constants::pi<T>(), eps2);
   BOOST_CHECK_CLOSE_FRACTION(ellint_rc(T(9)/4, T(2)), log(T(2)), eps2);
   BOOST_CHECK_CLOSE_FRACTION(ellint_rc(T(1)/4, T(-2)), log(T(2))/3, eps2);
   // RJ:
   BOOST_CHECK_CLOSE(ellint_rj(T(0), T(1), T(2), T(3)), T(0.77688623778582), tolerance);
   BOOST_CHECK_CLOSE(ellint_rj(T(2), T(3), T(4), T(5)), T(0.14297579667157), tolerance);
   BOOST_CHECK_CLOSE(ellint_rj(T(2), T(3), T(4), T(-0.5)), T(0.24723819703052), tolerance);
   BOOST_CHECK_CLOSE(ellint_rj(T(2), T(3), T(4), T(-5)), T(-0.12711230042964), tolerance);
   // RD:
   BOOST_CHECK_CLOSE(ellint_rd(T(0), T(2), T(1)), T(1.7972103521034), tolerance);
   BOOST_CHECK_CLOSE(ellint_rd(T(2), T(3), T(4)), T(0.16510527294261), tolerance);

   // Sanity/consistency checks from Numerical Computation of Real or Complex 
   // Elliptic Integrals, B. C. Carlson: http://arxiv.org/abs/math.CA/9409227
   std::tr1::mt19937 ran;
   std::tr1::uniform_real<float> ur(0, 1000);
   T eps40 = 40 * tools::epsilon<T>();

   for(unsigned i = 0; i < 1000; ++i)
   {
      T x = ur(ran);
      T y = ur(ran);
      T z = ur(ran);
      T lambda = ur(ran);
      T mu = x * y / lambda;
      // RF, eq 49:
      T s1 = ellint_rf(x+lambda, y+lambda, lambda) + 
         ellint_rf(x + mu, y + mu, mu);
      T s2 = ellint_rf(x, y, T(0));
      BOOST_CHECK_CLOSE_FRACTION(s1, s2, eps40);
      // RC is degenerate case of RF:
      s1 = ellint_rc(x, y);
      s2 = ellint_rf(x, y, y);
      BOOST_CHECK_CLOSE_FRACTION(s1, s2, eps40);
      // RC, eq 50 (Note have to assume y = x):
      T mu2 = x * x / lambda;
      s1 = ellint_rc(lambda, x+lambda) 
         + ellint_rc(mu2, x + mu2);
      s2 = ellint_rc(T(0), x);
      BOOST_CHECK_CLOSE_FRACTION(s1, s2, eps40);
      /*
      T p = ????; // no closed form for a, b and p???
      s1 = ellint_rj(x+lambda, y+lambda, lambda, p+lambda)
         + ellint_rj(x+mu, y+mu, mu, p+mu);
      s2 = ellint_rj(x, y, T(0), p)
         - 3 * ellint_rc(a, b);
      */
      // RD, eq 53:
      s1 = ellint_rd(lambda, x+lambda, y+lambda)
         + ellint_rd(mu, x+mu, y+mu);
      s2 = ellint_rd(T(0), x, y)
         - 3 / (y * sqrt(x+y+lambda+mu));
      BOOST_CHECK_CLOSE_FRACTION(s1, s2, eps40);
      // RD is degenerate case of RJ:
      s1 = ellint_rd(x, y, z);
      s2 = ellint_rj(x, y, z, z);
      BOOST_CHECK_CLOSE_FRACTION(s1, s2, eps40);
   }

   //
   // Now random spot values:
   //
#include "ellint_rf_data.ipp"

   do_test_ellint_rf(ellint_rf_data, type_name, "RF: Random data");

#include "ellint_rc_data.ipp"

   do_test_ellint_rc(ellint_rc_data, type_name, "RC: Random data");

#include "ellint_rj_data.ipp"

   do_test_ellint_rj(ellint_rj_data, type_name, "RJ: Random data");

#include "ellint_rd_data.ipp"

   do_test_ellint_rd(ellint_rd_data, type_name, "RD: Random data");
}

int test_main(int, char* [])
{
    expected_results();
    BOOST_MATH_CONTROL_FP;

    boost::math::ellint_rj(1.778e-31, 1.407e+18, 10.05, -4.83e-10);

    test_spots(0.0F, "float");
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

/*

test_carlson.cpp
Linking...
Embedding manifest...
Autorun "i:\boost-06-05-03-1300\libs\math\test\Math_test\debug\test_carlson.exe"
Running 1 test case...
Tests run with Microsoft Visual C++ version 8.0, Dinkumware standard library version 405, Win32
Testing: RF: Random data
boost::math::ellint_rf<float> Max = 0 RMS Mean=0
Testing: RC: Random data
boost::math::ellint_rc<float> Max = 0 RMS Mean=0
Testing: RJ: Random data
boost::math::ellint_rf<float> Max = 0 RMS Mean=0
Testing: RD: Random data
boost::math::ellint_rd<float> Max = 0 RMS Mean=0
Testing: RF: Random data
boost::math::ellint_rf<double> Max = 2.949 RMS Mean=0.7498
    worst case at row: 377
    { 3.418e+025, 2.594e-005, 3.264e-012, 6.169e-012 }
Testing: RC: Random data
boost::math::ellint_rc<double> Max = 2.396 RMS Mean=0.6283
    worst case at row: 10
    { 1.97e-029, 3.224e-025, 2.753e+012 }
Testing: RJ: Random data
boost::math::ellint_rf<double> Max = 152.9 RMS Mean=11.15
    worst case at row: 633
    { 1.876e+016, 0.000278, 3.796e-006, -4.412e-005, -1.656e-005 }
Testing: RD: Random data
boost::math::ellint_rd<double> Max = 2.586 RMS Mean=0.8614
    worst case at row: 45
    { 2.111e-020, 8.757e-026, 1.923e-023, 1.004e+033 }
Testing: RF: Random data
boost::math::ellint_rf<long double> Max = 2.949 RMS Mean=0.7498
    worst case at row: 377
    { 3.418e+025, 2.594e-005, 3.264e-012, 6.169e-012 }
Testing: RC: Random data
boost::math::ellint_rc<long double> Max = 2.396 RMS Mean=0.6283
    worst case at row: 10
    { 1.97e-029, 3.224e-025, 2.753e+012 }
Testing: RJ: Random data
boost::math::ellint_rf<long double> Max = 152.9 RMS Mean=11.15
    worst case at row: 633
    { 1.876e+016, 0.000278, 3.796e-006, -4.412e-005, -1.656e-005 }
Testing: RD: Random data
boost::math::ellint_rd<long double> Max = 2.586 RMS Mean=0.8614
    worst case at row: 45
    { 2.111e-020, 8.757e-026, 1.923e-023, 1.004e+033 }
Testing: RF: Random data
boost::math::ellint_rf<real_concept> Max = 2.949 RMS Mean=0.7498
    worst case at row: 377
    { 3.418e+025, 2.594e-005, 3.264e-012, 6.169e-012 }
Testing: RC: Random data
boost::math::ellint_rc<real_concept> Max = 2.396 RMS Mean=0.6283
    worst case at row: 10
    { 1.97e-029, 3.224e-025, 2.753e+012 }
Testing: RJ: Random data
boost::math::ellint_rf<real_concept> Max = 152.9 RMS Mean=11.15
    worst case at row: 633
    { 1.876e+016, 0.000278, 3.796e-006, -4.412e-005, -1.656e-005 }
Testing: RD: Random data
boost::math::ellint_rd<real_concept> Max = 2.586 RMS Mean=0.8614
    worst case at row: 45
    { 2.111e-020, 8.757e-026, 1.923e-023, 1.004e+033 }
*** No errors detected

*/
