//  (C) Copyright John Maddock 2006.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/beta.hpp>
#include <boost/math/tools/stats.hpp>
#include <boost/math/tools/test.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/type_traits/is_floating_point.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "test_beta_hooks.hpp"
#include "handle_test_result.hpp"

#if !defined(TEST_FLOAT) && !defined(TEST_DOUBLE) && !defined(TEST_LDOUBLE) && !defined(TEST_REAL_CONCEPT)
#  define TEST_FLOAT
#  define TEST_DOUBLE
#  define TEST_LDOUBLE
#  define TEST_REAL_CONCEPT
#endif

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the incomplete beta function inverses 
// ibeta_inv and ibetac_inv. There are three sets of tests:
// 1) Spot tests which compare our results with selected values 
// computed using the online special function calculator at 
// functions.wolfram.com, 
// 2) TODO!!!! Accuracy tests use values generated with NTL::RR at 
// 1000-bit precision and our generic versions of these functions.
// 3) Round trip sanity checks, use the test data for the forward
// functions, and verify that we can get (approximately) back
// where we started.
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
   // Note that permitted max errors are really pretty high
   // at around 10000eps.  The reason for this is that even 
   // if the forward function is off by 1eps, it's enough to
   // throw out the inverse by ~7000eps.  In other words the
   // forward function may flatline, so that many x-values
   // all map to about the same p.  Trying to invert in this
   // region is almost futile.
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

#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   //
   // Linux etc,
   // Extended exponent range of long double
   // causes more extreme test cases to be executed:
   //
   if(std::numeric_limits<long double>::digits == 64)
   {
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                       // platform
         "double",                      // test type(s)
         ".*",                          // test data group
         ".*", 20, 10);            // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                       // platform
         "long double",                      // test type(s)
         ".*",                          // test data group
         ".*", 200000, 100000);            // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "real_concept",                // test type(s)
         ".*",                          // test data group
         ".*", 5000000L, 500000);         // test function
   }
#endif
   //
   // MinGW,
   // Extended exponent range of long double
   // causes more extreme test cases to be executed:
   //
   add_expected_result(
      ".*mingw.*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "double",                // test type(s)
      ".*",                          // test data group
      ".*", 10, 10);         // test function
   add_expected_result(
      ".*mingw.*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                // test type(s)
      ".*",                          // test data group
      ".*", 300000, 20000);         // test function

   //
   // HP-UX and Solaris:
   // Extended exponent range of long double
   // causes more extreme test cases to be executed:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "HP-UX|Sun Solaris",           // platform
      "long double",                 // test type(s)
      ".*",                          // test data group
      ".*", 200000, 100000);         // test function

   //
   // HP Tru64:
   // Extended exponent range of long double
   // causes more extreme test cases to be executed:
   //
   add_expected_result(
      "HP Tru64.*",                  // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "long double",                 // test type(s)
      ".*",                          // test data group
      ".*", 200000, 100000);         // test function

   //
   // Catch all cases come last:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 10000, 1000);            // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*",                          // test data group
      ".*", 500000, 500000);         // test function

   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void test_inverses(const T& data)
{
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   value_type precision = static_cast<value_type>(ldexp(1.0, 1-boost::math::policies::digits<value_type, boost::math::policies::policy<> >()/2)) * 100;
   if(boost::math::policies::digits<value_type, boost::math::policies::policy<> >() < 50)
      precision = 1;   // 1% or two decimal digits, all we can hope for when the input is truncated

   for(unsigned i = 0; i < data.size(); ++i)
   {
      //
      // These inverse tests are thrown off if the output of the
      // incomplete beta is too close to 1: basically there is insuffient
      // information left in the value we're using as input to the inverse
      // to be able to get back to the original value.
      //
      if(data[i][5] == 0)
         BOOST_CHECK_EQUAL(boost::math::ibeta_inv(data[i][0], data[i][1], data[i][5]), value_type(0));
      else if((1 - data[i][5] > 0.001) 
         && (fabs(data[i][5]) > 2 * boost::math::tools::min_value<value_type>()) 
         && (fabs(data[i][5]) > 2 * boost::math::tools::min_value<double>()))
      {
         value_type inv = boost::math::ibeta_inv(data[i][0], data[i][1], data[i][5]);
         BOOST_CHECK_CLOSE(data[i][2], inv, precision);
      }
      else if(1 == data[i][5])
         BOOST_CHECK_EQUAL(boost::math::ibeta_inv(data[i][0], data[i][1], data[i][5]), value_type(1));

      if(data[i][6] == 0)
         BOOST_CHECK_EQUAL(boost::math::ibetac_inv(data[i][0], data[i][1], data[i][6]), value_type(1));
      else if((1 - data[i][6] > 0.001) 
         && (fabs(data[i][6]) > 2 * boost::math::tools::min_value<value_type>()) 
         && (fabs(data[i][6]) > 2 * boost::math::tools::min_value<double>()))
      {
         value_type inv = boost::math::ibetac_inv(data[i][0], data[i][1], data[i][6]);
         BOOST_CHECK_CLOSE(data[i][2], inv, precision);
      }
      else if(data[i][6] == 1)
         BOOST_CHECK_EQUAL(boost::math::ibetac_inv(data[i][0], data[i][1], data[i][6]), value_type(0));
   }
}

template <class T>
void test_inverses2(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::ibeta_inv<value_type, value_type, value_type>;
#else
   pg funcp = boost::math::ibeta_inv;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test ibeta_inv(T, T, T) against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1, 2),
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::ibeta_inv", test_name);
   //
   // test ibetac_inv(T, T, T) against data:
   //
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   funcp = boost::math::ibetac_inv<value_type, value_type, value_type>;
#else
   funcp = boost::math::ibetac_inv;
#endif
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1, 2),
      extract_result(4));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::ibetac_inv", test_name);
}


template <class T>
void test_beta(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
   // The contents are as follows, each row of data contains
   // five items, input value a, input value b, integration limits x, beta(a, b, x) and ibeta(a, b, x):
   //
#if !defined(TEST_DATA) || (TEST_DATA == 1)
#  include "ibeta_small_data.ipp"

   test_inverses(ibeta_small_data);
#endif

#if !defined(TEST_DATA) || (TEST_DATA == 2)
#  include "ibeta_data.ipp"

   test_inverses(ibeta_data);
#endif

#if !defined(TEST_DATA) || (TEST_DATA == 3)
#  include "ibeta_large_data.ipp"

   test_inverses(ibeta_large_data);
#endif

#if !defined(TEST_DATA) || (TEST_DATA == 4)
#  include "ibeta_inv_data.ipp"

   test_inverses2(ibeta_inv_data, name, "Inverse incomplete beta");
#endif
}

template <class T>
void test_spots(T)
{
   //
   // basic sanity checks, tolerance is 100 epsilon expressed as a percentage:
   //
   T tolerance = boost::math::tools::epsilon<T>() * 10000;
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta_inv(
         static_cast<T>(1),
         static_cast<T>(2),
         static_cast<T>(0.5)),
      static_cast<T>(0.29289321881345247559915563789515096071516406231153L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta_inv(
         static_cast<T>(3),
         static_cast<T>(0.5),
         static_cast<T>(0.5)),
      static_cast<T>(0.92096723292382700385142816696980724853063433975470L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta_inv(
         static_cast<T>(20.125),
         static_cast<T>(0.5),
         static_cast<T>(0.5)),
      static_cast<T>(0.98862133312917003480022776106012775747685870929920L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta_inv(
         static_cast<T>(40),
         static_cast<T>(80),
         static_cast<T>(0.5)),
      static_cast<T>(0.33240456430025026300937492802591128972548660643778L), tolerance);
}

int test_main(int, char* [])
{
   BOOST_MATH_CONTROL_FP;
   expected_results();
#ifdef TEST_GSL
   gsl_set_error_handler_off();
#endif
#ifdef TEST_FLOAT
   test_spots(0.0F);
#endif
#ifdef TEST_DOUBLE
   test_spots(0.0);
#endif
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#ifdef TEST_LDOUBLE
   test_spots(0.0L);
#endif
#ifdef TEST_REAL_CONCEPT
   test_spots(boost::math::concepts::real_concept(0.1));
#endif
#endif

#ifdef TEST_FLOAT
   test_beta(0.1F, "float");
#endif
#ifdef TEST_DOUBLE
   test_beta(0.1, "double");
#endif
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#ifdef TEST_LDOUBLE
   test_beta(0.1L, "long double");
#endif
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
#ifdef TEST_REAL_CONCEPT
   test_beta(boost::math::concepts::real_concept(0.1), "real_concept");
#endif
#endif
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}




