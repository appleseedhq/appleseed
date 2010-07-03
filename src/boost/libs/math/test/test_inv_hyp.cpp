//  (C) Copyright John Maddock 2006.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/concepts/real_concept.hpp>
#include <boost/math/special_functions/acosh.hpp>
#include <boost/math/special_functions/asinh.hpp>
#include <boost/math/special_functions/atanh.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/tools/stats.hpp>
#include <boost/math/tools/test.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/type_traits/is_floating_point.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the inverse hyperbolic functions. There are two sets of tests:
// 1) Sanity checks: comparison to test values created with the
// online calculator at functions.wolfram.com
// 2) Accuracy tests use values generated with NTL::RR at 
// 1000-bit precision and our generic versions of these functions.
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

   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      "atanh.*",                     // test data group
      ".*", 6, 2);                   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      "atanh.*",                     // test data group
      ".*", 6, 1);                   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*",                          // test data group
      ".*", 4, 2);                   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 4, 1);                   // test function

   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test_asinh(const T& data, const char* type_name, const char* test_name)
{
   //
   // test asinh(T) against data:
   //
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << test_name << " with type " << type_name << std::endl;

   typedef value_type (*pg)(value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::asinh<value_type>;
#else
   pg funcp = boost::math::asinh;
#endif

   boost::math::tools::test_result<value_type> result;
   //
   // test asinh against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0),
      extract_result(1));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::asinh", test_name);
   std::cout << std::endl;
}

template <class T>
void do_test_acosh(const T& data, const char* type_name, const char* test_name)
{
   //
   // test acosh(T) against data:
   //
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << test_name << " with type " << type_name << std::endl;

   typedef value_type (*pg)(value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::acosh<value_type>;
#else
   pg funcp = boost::math::acosh;
#endif

   boost::math::tools::test_result<value_type> result;
   //
   // test acosh against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0),
      extract_result(1));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::acosh", test_name);
   std::cout << std::endl;
}

template <class T>
void do_test_atanh(const T& data, const char* type_name, const char* test_name)
{
   //
   // test atanh(T) against data:
   //
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << test_name << " with type " << type_name << std::endl;

   typedef value_type (*pg)(value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::atanh<value_type>;
#else
   pg funcp = boost::math::atanh;
#endif

   boost::math::tools::test_result<value_type> result;
   //
   // test atanh against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0),
      extract_result(1));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::atanh", test_name);
   std::cout << std::endl;
}

template <class T>
void test_inv_hyperbolics(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
#include "asinh_data.ipp"
   do_test_asinh(asinh_data, name, "asinh");
#include "acosh_data.ipp"
   do_test_acosh(acosh_data, name, "acosh");
#include "atanh_data.ipp"
   do_test_atanh(atanh_data, name, "atanh");
}

extern "C" double zetac(double);

template <class T>
void test_spots(T, const char* t)
{
   std::cout << "Testing basic sanity checks for type " << t << std::endl;
   //
   // Basic sanity checks, tolerance is either 5 or 10 epsilon 
   // expressed as a percentage:
   //
   T tolerance = boost::math::tools::epsilon<T>() * 100 *
      (boost::is_floating_point<T>::value ? 5 : 10);
   BOOST_CHECK_CLOSE(::boost::math::acosh(static_cast<T>(262145)/262144L), static_cast<T>(0.00276213498595136093375633956331651432309750291610866833462649L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::acosh(static_cast<T>(2)), static_cast<T>(1.31695789692481670862504634730796844402698197146751647976847L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::acosh(static_cast<T>(40)), static_cast<T>(4.38187034804006698696313269586603717076961771721038534547948L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::acosh(static_cast<T>(262145L)), static_cast<T>(13.1698002453253126137651962522659827810753786944786303017757L), tolerance);

   BOOST_CHECK_CLOSE(::boost::math::asinh(static_cast<T>(0)), static_cast<T>(0), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::asinh(static_cast<T>(1)/262145L), static_cast<T>(3.81468271375603081996185039385472561751449912305225962381803e-6L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::asinh(static_cast<T>(0.25)), static_cast<T>(0.247466461547263452944781549788359289253766903098567696469117L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::asinh(static_cast<T>(1)), static_cast<T>(0.881373587019543025232609324979792309028160328261635410753296L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::asinh(static_cast<T>(10)), static_cast<T>(2.99822295029796973884659553759645347660705805487730365573446L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::asinh(static_cast<T>(262145L)), static_cast<T>(13.1698002453325885158685460826511173257938039316922010439486L), tolerance);

   BOOST_CHECK_CLOSE(::boost::math::atanh(static_cast<T>(0)), static_cast<T>(0), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::atanh(static_cast<T>(1)/262145L), static_cast<T>(3.81468271378378607794264842456613940280945630999769224301574e-6L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::atanh(static_cast<T>(-1)/262145L), static_cast<T>(-3.81468271378378607794264842456613940280945630999769224301574e-6L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::atanh(static_cast<T>(0.5)), static_cast<T>(0.549306144334054845697622618461262852323745278911374725867347L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::atanh(static_cast<T>(-0.5)), static_cast<T>(-0.549306144334054845697622618461262852323745278911374725867347L), tolerance);
}

int test_main(int, char* [])
{
   expected_results();
   BOOST_MATH_CONTROL_FP;

   test_spots(0.0f, "float");
   test_spots(0.0, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_spots(0.0L, "long double");
   test_spots(boost::math::concepts::real_concept(0.1), "real_concept");
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif

   test_inv_hyperbolics(0.1F, "float");
   test_inv_hyperbolics(0.1, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_inv_hyperbolics(0.1L, "long double");
   test_inv_hyperbolics(boost::math::concepts::real_concept(0.1), "real_concept");
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}



