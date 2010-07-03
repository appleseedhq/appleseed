//  (C) Copyright John Maddock 2006.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/concepts/real_concept.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/tools/stats.hpp>
#include <boost/math/tools/test.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/type_traits/is_floating_point.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "test_gamma_hooks.hpp"
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
// This file tests the incomplete gamma function inverses 
// gamma_p_inv and gamma_q_inv. There are three sets of tests:
// 1) Spot tests which compare our results with selected values 
// computed using the online special function calculator at 
// functions.wolfram.com, 
// 2) Accuracy tests use values generated with NTL::RR at 
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
   // Large exponent range causes more extreme test cases to be evaluated:
   //
   if(std::numeric_limits<long double>::max_exponent > std::numeric_limits<double>::max_exponent)
   {
      add_expected_result(
         "[^|]*",                          // compiler
         "[^|]*",                          // stdlib
         "[^|]*",                          // platform
         largest_type,                     // test type(s)
         "[^|]*small[^|]*",                    // test data group
         "[^|]*", 200000, 10000);              // test function
      add_expected_result(
         "[^|]*",                          // compiler
         "[^|]*",                          // stdlib
         "[^|]*",                          // platform
         "real_concept",                     // test type(s)
         "[^|]*small[^|]*",                   // test data group
         "[^|]*", 70000, 8000);                  // test function
   }
   //
   // These high error rates are seen on on some Linux
   // architectures:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux.*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*medium[^|]*",                   // test data group
      "[^|]*", 350, 5);                  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux.*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*large[^|]*",                   // test data group
      "[^|]*", 150, 5);                  // test function


   //
   // Catch all cases come last:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*medium[^|]*",                   // test data group
      "[^|]*", 20, 5);                  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*large[^|]*",                    // test data group
      "[^|]*", 5, 2);                   // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*small[^|]*",                    // test data group
      "[^|]*", 2100, 500);              // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "float|double",                   // test type(s)
      "[^|]*small[^|]*",                    // test data group
      "boost::math::gamma_p_inv", 500, 60);   // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "float|double",                   // test type(s)
      "[^|]*",                          // test data group
      "boost::math::gamma_q_inv", 350, 60);   // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "float|double",                   // test type(s)
      "[^|]*",                          // test data group
      "[^|]*", 4, 2);                   // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                     // test type(s)
      "[^|]*medium[^|]*",                   // test data group
      "[^|]*", 20, 5);                  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                     // test type(s)
      "[^|]*large[^|]*",                   // test data group
      "[^|]*", 1000, 500);                  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                     // test type(s)
      "[^|]*small[^|]*",                   // test data group
      "[^|]*", 3700, 500);                  // test function

   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

#define BOOST_CHECK_CLOSE_EX(a, b, prec, i) \
   {\
      unsigned int failures = boost::unit_test::results_collector.results( boost::unit_test::framework::current_test_case().p_id ).p_assertions_failed;\
      BOOST_CHECK_CLOSE(a, b, prec); \
      if(failures != boost::unit_test::results_collector.results( boost::unit_test::framework::current_test_case().p_id ).p_assertions_failed)\
      {\
         std::cerr << "Failure was at row " << i << std::endl;\
         std::cerr << std::setprecision(35); \
         std::cerr << "{ " << data[i][0] << " , " << data[i][1] << " , " << data[i][2];\
         std::cerr << " , " << data[i][3] << " , " << data[i][4] << " , " << data[i][5] << " } " << std::endl;\
      }\
   }

template <class T>
void do_test_gamma_2(const T& data, const char* type_name, const char* test_name)
{
   //
   // test gamma_p_inv(T, T) against data:
   //
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << test_name << " with type " << type_name << std::endl;

   //
   // These sanity checks test for a round trip accuracy of one half
   // of the bits in T, unless T is type float, in which case we check
   // for just one decimal digit.  The problem here is the sensitivity
   // of the functions, not their accuracy.  This test data was generated
   // for the forward functions, which means that when it is used as
   // the input to the inverses then it is necessarily inexact.  This rounding
   // of the input is what makes the data unsuitable for use as an accuracy check,
   // and also demonstrates that you can't in general round-trip these functions.
   // It is however a useful sanity check.
   //
   value_type precision = static_cast<value_type>(ldexp(1.0, 1-boost::math::policies::digits<value_type, boost::math::policies::policy<> >()/2)) * 100;
   if(boost::math::policies::digits<value_type, boost::math::policies::policy<> >() < 50)
      precision = 1;   // 1% or two decimal digits, all we can hope for when the input is truncated to float

   for(unsigned i = 0; i < data.size(); ++i)
   {
      //
      // These inverse tests are thrown off if the output of the
      // incomplete gamma is too close to 1: basically there is insuffient
      // information left in the value we're using as input to the inverse
      // to be able to get back to the original value.
      //
      if(data[i][5] == 0)
         BOOST_CHECK_EQUAL(boost::math::gamma_p_inv(data[i][0], data[i][5]), value_type(0));
      else if((1 - data[i][5] > 0.001) 
         && (fabs(data[i][5]) > 2 * boost::math::tools::min_value<value_type>()) 
         && (fabs(data[i][5]) > 2 * boost::math::tools::min_value<double>()))
      {
         value_type inv = boost::math::gamma_p_inv(data[i][0], data[i][5]);
         BOOST_CHECK_CLOSE_EX(data[i][1], inv, precision, i);
      }
      else if(1 == data[i][5])
         BOOST_CHECK_EQUAL(boost::math::gamma_p_inv(data[i][0], data[i][5]), boost::math::tools::max_value<value_type>());
      else
      {
         // not enough bits in our input to get back to x, but we should be in
         // the same ball park:
         value_type inv = boost::math::gamma_p_inv(data[i][0], data[i][5]);
         BOOST_CHECK_CLOSE_EX(data[i][1], inv, 100000, i);
      }

      if(data[i][3] == 0)
         BOOST_CHECK_EQUAL(boost::math::gamma_q_inv(data[i][0], data[i][3]), boost::math::tools::max_value<value_type>());
      else if((1 - data[i][3] > 0.001) && (fabs(data[i][3]) > 2 * boost::math::tools::min_value<value_type>()))
      {
         value_type inv = boost::math::gamma_q_inv(data[i][0], data[i][3]);
         BOOST_CHECK_CLOSE_EX(data[i][1], inv, precision, i);
      }
      else if(1 == data[i][3])
         BOOST_CHECK_EQUAL(boost::math::gamma_q_inv(data[i][0], data[i][3]), value_type(0));
      else if(fabs(data[i][3]) > 2 * boost::math::tools::min_value<value_type>())
      {
         // not enough bits in our input to get back to x, but we should be in
         // the same ball park:
         value_type inv = boost::math::gamma_q_inv(data[i][0], data[i][3]);
         BOOST_CHECK_CLOSE_EX(data[i][1], inv, 100, i);
      }
   }
   std::cout << std::endl;
}

template <class T>
void do_test_gamma_inv(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::gamma_p_inv<value_type, value_type>;
#else
   pg funcp = boost::math::gamma_p_inv;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test gamma_p_inv(T, T) against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1),
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::gamma_p_inv", test_name);
   //
   // test gamma_q_inv(T, T) against data:
   //
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   funcp = boost::math::gamma_q_inv<value_type, value_type>;
#else
   funcp = boost::math::gamma_q_inv;
#endif
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1),
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::gamma_q_inv", test_name);
}

template <class T>
void test_gamma(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
   // First the data for the incomplete gamma function, each
   // row has the following 6 entries:
   // Parameter a, parameter z,
   // Expected tgamma(a, z), Expected gamma_q(a, z)
   // Expected tgamma_lower(a, z), Expected gamma_p(a, z)
   //
#  include "igamma_med_data.ipp"

   do_test_gamma_2(igamma_med_data, name, "Running round trip sanity checks on incomplete gamma medium sized values");

#  include "igamma_small_data.ipp"

   do_test_gamma_2(igamma_small_data, name, "Running round trip sanity checks on incomplete gamma small values");

#  include "igamma_big_data.ipp"

   do_test_gamma_2(igamma_big_data, name, "Running round trip sanity checks on incomplete gamma large values");

#  include "gamma_inv_data.ipp"

   do_test_gamma_inv(gamma_inv_data, name, "incomplete gamma inverse(a, z) medium values");

#  include "gamma_inv_big_data.ipp"

   do_test_gamma_inv(gamma_inv_big_data, name, "incomplete gamma inverse(a, z) large values");

#  include "gamma_inv_small_data.ipp"

   do_test_gamma_inv(gamma_inv_small_data, name, "incomplete gamma inverse(a, z) small values");
}

template <class T>
void test_spots(T, const char* type_name)
{
   std::cout << "Running spot checks for type " << type_name << std::endl;
   //
   // basic sanity checks, tolerance is 150 epsilon expressed as a percentage:
   //
   T tolerance = boost::math::tools::epsilon<T>() * 15000;
   if(tolerance < 1e-25f)
      tolerance = 1e-25f;  // limit of test data?
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(1)/100, static_cast<T>(1.0/128)), static_cast<T>(0.35767144525455121503672919307647515332256996883787L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(1)/100, static_cast<T>(0.5)), static_cast<T>(4.4655350189103486773248562646452806745879516124613e-31L), tolerance*10);
   //
   // We can't test in this region against Mathworld's data as the results produced
   // by functions.wolfram.com appear to be in error, and do *not* round trip with
   // their own version of gamma_q.  Using our output from the inverse as input to 
   // their version of gamma_q *does* round trip however.  It should be pointed out
   // that the functions in this area are very sensitive with nearly infinite
   // first derivatives, it's also questionable how useful these functions are
   // in this part of the domain.
   //
   //BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(1e-2), static_cast<T>(1.0-1.0/128)), static_cast<T>(3.8106736649978161389878528903698068142257930575497e-181L), tolerance);
   //
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(0.5), static_cast<T>(1.0/128)), static_cast<T>(3.5379794687984498627918583429482809311448951189097L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(0.5), static_cast<T>(1.0/2)), static_cast<T>(0.22746821155978637597125832348982469815821055329511L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(0.5), static_cast<T>(1.0-1.0/128)), static_cast<T>(0.000047938431649305382237483273209405461203600840052182L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(10), static_cast<T>(1.0/128)), static_cast<T>(19.221865946801723949866005318845155649972164294057L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(10), static_cast<T>(1.0/2)), static_cast<T>(9.6687146147141311517500637401166726067778162022664L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(10), static_cast<T>(1.0-1.0/128)), static_cast<T>(3.9754602513640844712089002210120603689809432130520L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(10000), static_cast<T>(1.0/128)), static_cast<T>(10243.369973939134157953734588122880006091919872879L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(10000), static_cast<T>(1.0/2)), static_cast<T>(9999.6666686420474237369661574633153551436435884101L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::gamma_q_inv(static_cast<T>(10000), static_cast<T>(1.0-1.0/128)), static_cast<T>(9759.8597223369324083191194574874497413261589080204L), tolerance);
}

int test_main(int, char* [])
{
   expected_results();
   BOOST_MATH_CONTROL_FP;

#ifndef BOOST_MATH_BUGGY_LARGE_FLOAT_CONSTANTS
#ifdef TEST_FLOAT
   test_spots(0.0F, "float");
#endif
#endif
#ifdef TEST_DOUBLE
   test_spots(0.0, "double");
#endif
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#ifdef TEST_LDOUBLE
   test_spots(0.0L, "long double");
#endif
#if !BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x582))
#ifdef TEST_REAL_CONCEPT
   test_spots(boost::math::concepts::real_concept(0.1), "real_concept");
#endif
#endif
#endif

#ifndef BOOST_MATH_BUGGY_LARGE_FLOAT_CONSTANTS
#ifdef TEST_FLOAT
   test_gamma(0.1F, "float");
#endif
#endif
#ifdef TEST_DOUBLE
   test_gamma(0.1, "double");
#endif
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#ifdef TEST_LDOUBLE
   test_gamma(0.1L, "long double");
#endif
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
#if !BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x582))
#ifdef TEST_REAL_CONCEPT
   test_gamma(boost::math::concepts::real_concept(0.1), "real_concept");
#endif
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



