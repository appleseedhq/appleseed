//  Copyright John Maddock 2006, 2007
//  Copyright Paul A. Bristow 2007

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifdef _MSC_VER
#  pragma warning(disable : 4127) // conditional expression is constant
#  pragma warning(disable : 4512) // assignment operator could not be generated
#  pragma warning(disable : 4756) // overflow in constant arithmetic
// Constants are too big for float case, but this doesn't matter for test.
#endif

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/hermite.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
#include "test_legendre_hooks.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the Hermite polynomials.  
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
      "boost::math::hermite", 10, 5);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                  // test type(s)
      ".*",      // test data group
      "boost::math::hermite", 10, 5);  // test function
   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test_hermite(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(unsigned, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::hermite<value_type>;
#else
   pg funcp = boost::math::hermite;
#endif

   typedef unsigned (*cast_t)(value_type);

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test hermite against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func_int1(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::hermite", test_name);

   std::cout << std::endl;
}

template <class T>
void test_hermite(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
   // The contents are as follows, each row of data contains
   // three items, input value a, input value b and erf(a, b):
   // 
#  include "hermite.ipp"

   do_test_hermite(hermite, name, "Hermite Polynomials");
}

template <class T>
void test_spots(T, const char* t)
{
   std::cout << "Testing basic sanity checks for type " << t << std::endl;
   //
   // basic sanity checks, tolerance is 100 epsilon:
   // These spots were generated by MathCAD, precision is 
   // 14-16 digits.
   //
   T tolerance = (std::max)(boost::math::tools::epsilon<T>() * 100, static_cast<T>(1e-14));
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(0, static_cast<T>(1)), static_cast<T>(1.L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(1, static_cast<T>(1)), static_cast<T>(2.L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(1, static_cast<T>(2)), static_cast<T>(4.L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(1, static_cast<T>(10)), static_cast<T>(20), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(1, static_cast<T>(100)), static_cast<T>(200), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(1, static_cast<T>(1e6)), static_cast<T>(2e6), tolerance);
   if(std::numeric_limits<T>::max_exponent >= std::numeric_limits<double>::max_exponent)
   {
      BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(1, static_cast<T>(1e307)), static_cast<T>(2e307), tolerance);
      BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(99, static_cast<T>(100)), static_cast<T>(4.967223743011310E+227L), tolerance);
   }
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(10, static_cast<T>(30)), static_cast<T>(5.896624628001300E+17L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(10, static_cast<T>(1000)), static_cast<T>(1.023976960161280E+33L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(10, static_cast<T>(10)), static_cast<T>(8.093278209760000E+12L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(10, static_cast<T>(-10)), static_cast<T>(8.093278209760000E+12L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(3, static_cast<T>(-10)), static_cast<T>(-7.880000000000000E+3L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(3, static_cast<T>(-1000)), static_cast<T>(-7.999988000000000E+9L), tolerance);
   BOOST_CHECK_CLOSE_FRACTION(::boost::math::hermite(3, static_cast<T>(-1000000)), static_cast<T>(-7.999999999988000E+18L), tolerance);
}

int test_main(int, char* [])
{
   BOOST_MATH_CONTROL_FP;

   boost::math::hermite(51, 915.0);

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
   test_hermite(0.1F, "float");
#endif
   test_hermite(0.1, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_hermite(0.1L, "long double");
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
   test_hermite(boost::math::concepts::real_concept(0.1), "real_concept");
#endif
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}



