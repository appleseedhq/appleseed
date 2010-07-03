//  (C) Copyright John Maddock 2005.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_MATH_OVERFLOW_ERROR_POLICY ignore_error

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/log1p.hpp>
#include <boost/math/special_functions/expm1.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the functions log1p and expm1.  The accuracy tests
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

   //
   // Catch all cases come last:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      ".*",                          // test type(s)
      ".*",                          // test data group
      ".*",                          // test function
      4,                             // Max Peek error
      3);                            // Max mean error

   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = &boost::math::log1p<value_type>;
#else
   pg funcp = &boost::math::log1p;
#endif

   boost::math::tools::test_result<value_type> result;
   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
   //
   // test log1p against data:
   //
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   funcp = boost::math::log1p<value_type>;
#else
   funcp = &boost::math::log1p;
#endif
   result = boost::math::tools::test(
      data, 
         bind_func(funcp, 0), 
         extract_result(1));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::log1p", "log1p and expm1");
   std::cout << std::endl;
   //
   // test expm1 against data:
   //
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   funcp = boost::math::expm1<value_type>;
#else
   funcp = boost::math::expm1;
#endif
   result = boost::math::tools::test(
      data, 
      bind_func(funcp, 0), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::expm1", "log1p and expm1");
   std::cout << std::endl;
}

template <class T>
void test(T, const char* type_name)
{
#  include "log1p_expm1_data.ipp"

   do_test(log1p_expm1_data, type_name, "expm1 and log1p");

   //
   // C99 Appendix F special cases:
   static const T zero = 0;
   static const T m_one = -1;
   BOOST_CHECK_EQUAL(boost::math::log1p(zero), zero);
   BOOST_CHECK_EQUAL(boost::math::log1p(-zero), zero);
   BOOST_CHECK_EQUAL(boost::math::expm1(zero), zero);
   if(std::numeric_limits<T>::has_infinity)
   {
      BOOST_CHECK_EQUAL(boost::math::log1p(m_one), -std::numeric_limits<T>::infinity());
      BOOST_CHECK_EQUAL(boost::math::expm1(-std::numeric_limits<T>::infinity()), m_one);
      BOOST_CHECK_EQUAL(boost::math::expm1(std::numeric_limits<T>::infinity()), std::numeric_limits<T>::infinity());
#ifndef __BORLANDC__
      // When building with Borland's compiler, simply the *presence*
      // of these tests cause other unrelated tests to fail!!! :-(
      using namespace boost::math::policies;
      typedef policy<overflow_error<throw_on_error> > pol;
      BOOST_CHECK_THROW(boost::math::log1p(m_one, pol()), std::overflow_error);
      BOOST_CHECK_THROW(boost::math::expm1(std::numeric_limits<T>::infinity(), pol()), std::overflow_error);
#endif
   }
}


int test_main(int, char* [])
{
   expected_results();
   BOOST_MATH_CONTROL_FP;
   test(float(0), "float");
   test(double(0), "double");
   //
   // The long double version of these tests fails on some platforms
   // due to poor std lib support (not enough digits returned from 
   // std::log and std::exp):
   //
#if !defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS)
   test((long double)(0), "long double");
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
   test((boost::math::concepts::real_concept)(0), "real_concept");
#endif
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}

