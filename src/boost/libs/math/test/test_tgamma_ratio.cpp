//  (C) Copyright John Maddock 2006.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_MATH_OVERFLOW_ERROR_POLICY ignore_error

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/math/tools/stats.hpp>
#include <boost/math/tools/test.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the gamma ratio functions tgamma_ratio,
// and tgamma_delta_ratio. The accuracy tests
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
   // HP-UX
   // This is a weird one, HP-UX and Mac OS X show up errors at float
   // precision, that don't show up on other platforms.
   // There appears to be some kind of rounding issue going on (not enough
   // precision in the input to get the answer right):
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "HP-UX|Mac OS|linux|.*(bsd|BSD).*",      // platform
      "float",                          // test type(s)
      "[^|]*",                          // test data group
      "boost::math::tgamma_ratio[^|]*", 35, 8);                 // test function
   //
   // Linux AMD x86em64 has slightly higher rates:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux.*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*",               // test data group
      "boost::math::tgamma_ratio[^|]*", 300, 100);                 // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux.*",                          // platform
      "real_concept",                     // test type(s)
      "[^|]*",               // test data group
      "boost::math::tgamma_ratio[^|]*", 300, 100);                 // test function
   //
   // Catch all cases come last:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*",                          // test data group
      "boost::math::tgamma_delta_ratio[^|]*", 30, 20);                 // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*",               // test data group
      "boost::math::tgamma_ratio[^|]*", 100, 50);                 // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                   // test type(s)
      "[^|]*",                          // test data group
      "boost::math::tgamma_delta_ratio[^|]*", 40, 15);                 // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                   // test type(s)
      "[^|]*",               // test data group
      "boost::math::tgamma_ratio[^|]*", 150, 50);                 // test function

   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

struct negative_tgamma_ratio
{
   template <class Row>
   typename Row::value_type operator()(const Row& row)
   {
      return boost::math::tgamma_delta_ratio(row[0], -row[1]);
   }
};

template <class T>
void do_test_tgamma_delta_ratio(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::tgamma_delta_ratio<value_type, value_type>;
#else
   pg funcp = boost::math::tgamma_delta_ratio;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test tgamma_delta_ratio against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::tgamma_delta_ratio(a, delta)", test_name);
   result = boost::math::tools::test(
      data, 
      negative_tgamma_ratio(), 
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::tgamma_delta_ratio(a -delta)", test_name);
}

template <class T>
void do_test_tgamma_ratio(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::tgamma_ratio<value_type, value_type>;
#else
   pg funcp = boost::math::tgamma_ratio;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test tgamma_ratio against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::tgamma_ratio(a, b)", test_name);
}

template <class T>
void test_tgamma_ratio(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
#  include "tgamma_delta_ratio_data.ipp"

   do_test_tgamma_delta_ratio(tgamma_delta_ratio_data, name, "tgamma + small delta ratios");

#  include "tgamma_delta_ratio_int.ipp"

   do_test_tgamma_delta_ratio(tgamma_delta_ratio_int, name, "tgamma + small integer ratios");

#  include "tgamma_delta_ratio_int2.ipp"

   do_test_tgamma_delta_ratio(tgamma_delta_ratio_int2, name, "integer tgamma ratios");

#  include "tgamma_ratio_data.ipp"

   do_test_tgamma_ratio(tgamma_ratio_data, name, "tgamma ratios");

}

int test_main(int, char* [])
{
   BOOST_MATH_CONTROL_FP;
   expected_results();

#ifndef BOOST_MATH_BUGGY_LARGE_FLOAT_CONSTANTS
   test_tgamma_ratio(0.1F, "float");
#endif
   test_tgamma_ratio(0.1, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_tgamma_ratio(0.1L, "long double");
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
#if !BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x582))
   test_tgamma_ratio(boost::math::concepts::real_concept(0.1), "real_concept");
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


