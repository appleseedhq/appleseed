//  (C) Copyright John Maddock 2006.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/laguerre.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
#include "test_legendre_hooks.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the Laguerre polynomials.  
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
   // Linux special cases, error rates seem to be much higer here
   // even though the implementation contains nothing but basic
   // arithmetic?
   //
   if((std::numeric_limits<long double>::digits <= 64)
      && (std::numeric_limits<long double>::digits != std::numeric_limits<double>::digits))
   {
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "double",                      // test type(s)
         ".*",                          // test data group
         ".*", 10, 5);                  // test function
#endif
   }
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "linux.*|Mac OS|Sun.*",        // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 40000, 1000);            // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "linux.*|Mac OS|Sun.*",        // platform
      "real_concept",                // test type(s)
      ".*",                          // test data group
      ".*", 40000, 1000);            // test function
   add_expected_result(
      ".*mingw.*",                   // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 40000, 1000);            // test function
   add_expected_result(
      ".*mingw.*",                   // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*",                          // test data group
      ".*", 40000, 1000);            // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "IBM Aix",                     // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 5000, 500);            // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "IBM Aix",                     // platform
      "real_concept",                // test type(s)
      ".*",                          // test data group
      ".*", 5000, 500);            // test function

   //
   // Catch all cases come last:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",      // test data group
      ".*", 4000, 500);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                  // test type(s)
      ".*",      // test data group
      ".*", 4000, 500);  // test function

   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test_laguerre2(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(unsigned, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::laguerre<value_type>;
#else
   pg funcp = boost::math::laguerre;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test laguerre against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func_int1(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::laguerre(n, x)", test_name);

   std::cout << std::endl;
}

template <class T>
void do_test_laguerre3(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(unsigned, unsigned, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::laguerre<unsigned, value_type>;
#else
   pg funcp = boost::math::laguerre;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test laguerre against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func_int2(funcp, 0, 1, 2), 
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::laguerre(n, m, x)", test_name);
   std::cout << std::endl;
}

template <class T>
void test_laguerre(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
   // The contents are as follows, each row of data contains
   // three items, input value a, input value b and erf(a, b):
   // 
#  include "laguerre2.ipp"

   do_test_laguerre2(laguerre2, name, "Laguerre Polynomials");

#  include "laguerre3.ipp"

   do_test_laguerre3(laguerre3, name, "Associated Laguerre Polynomials");
}

template <class T>
void test_spots(T, const char* t)
{
   std::cout << "Testing basic sanity checks for type " << t << std::endl;
   //
   // basic sanity checks, tolerance is 100 epsilon:
   //
   T tolerance = boost::math::tools::epsilon<T>() * 100;
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(1, static_cast<T>(0.5L)), static_cast<T>(0.5L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(4, static_cast<T>(0.5L)), static_cast<T>(-0.3307291666666666666666666666666666666667L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(7, static_cast<T>(0.5L)), static_cast<T>(-0.5183392237103174603174603174603174603175L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(20, static_cast<T>(0.5L)), static_cast<T>(0.3120174870800154148915399248893113634676L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(50, static_cast<T>(0.5L)), static_cast<T>(-0.3181388060269979064951118308575628226834L), tolerance);

   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(1, static_cast<T>(-0.5L)), static_cast<T>(1.5L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(4, static_cast<T>(-0.5L)), static_cast<T>(3.835937500000000000000000000000000000000L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(7, static_cast<T>(-0.5L)), static_cast<T>(7.950934709821428571428571428571428571429L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(20, static_cast<T>(-0.5L)), static_cast<T>(76.12915699869631476833699787070874048223L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(50, static_cast<T>(-0.5L)), static_cast<T>(2307.428631277506570629232863491518399720L), tolerance);

   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(1, static_cast<T>(4.5L)), static_cast<T>(-3.500000000000000000000000000000000000000L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(4, static_cast<T>(4.5L)), static_cast<T>(0.08593750000000000000000000000000000000000L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(7, static_cast<T>(4.5L)), static_cast<T>(-1.036928013392857142857142857142857142857L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(20, static_cast<T>(4.5L)), static_cast<T>(1.437239150257817378525582974722170737587L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(50, static_cast<T>(4.5L)), static_cast<T>(-0.7795068145562651416494321484050019245248L), tolerance);

   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(4, 5, static_cast<T>(0.5L)), static_cast<T>(88.31510416666666666666666666666666666667L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(10, 0, static_cast<T>(2.5L)), static_cast<T>(-0.8802526766660982969576719576719576719577L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(10, 1, static_cast<T>(4.5L)), static_cast<T>(1.564311458042689732142857142857142857143L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(10, 6, static_cast<T>(8.5L)), static_cast<T>(20.51596541066649098875661375661375661376L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(10, 12, static_cast<T>(12.5L)), static_cast<T>(-199.5560968456234671241181657848324514991L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::laguerre(50, 40, static_cast<T>(12.5L)), static_cast<T>(-4.996769495006119488583146995907246595400e16L), tolerance);
}

int test_main(int, char* [])
{
   BOOST_MATH_CONTROL_FP;

#ifndef BOOST_MATH_BUGGY_LARGE_FLOAT_CONSTANTS
   test_spots(0.0F, "float");
#endif
   test_spots(0.0, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_spots(0.0L, "long double");
   test_spots(boost::math::concepts::real_concept(0.1), "real_concept");
#endif

   expected_results();

#ifndef BOOST_MATH_BUGGY_LARGE_FLOAT_CONSTANTS
   test_laguerre(0.1F, "float");
#endif
   test_laguerre(0.1, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_laguerre(0.1L, "long double");
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
   test_laguerre(boost::math::concepts::real_concept(0.1), "real_concept");
#endif
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}



