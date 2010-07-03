//  (C) Copyright John Maddock 2006.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/concepts/real_concept.hpp>
#include <boost/math/special_functions/zeta.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/tools/stats.hpp>
#include <boost/math/tools/test.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/type_traits/is_floating_point.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
#include "test_zeta_hooks.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the zeta function. There are two sets of tests:
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
      largest_type,                  // test type(s)
      ".*Random values less than 1", // test data group
      ".*", 1200, 500);              // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*Random values less than 1", // test data group
      ".*", 1200, 500);              // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 3, 1);                   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*",                          // test data group
      ".*", 10, 5);                  // test function

   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test_zeta(const T& data, const char* type_name, const char* test_name)
{
   //
   // test zeta(T) against data:
   //
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << test_name << " with type " << type_name << std::endl;

   typedef value_type (*pg)(value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::zeta<value_type>;
#else
   pg funcp = boost::math::zeta;
#endif

   boost::math::tools::test_result<value_type> result;
   //
   // test zeta against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0),
      extract_result(1));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::zeta", test_name);
#ifdef TEST_OTHER
   if(boost::is_floating_point<value_type>::value)
   {
      funcp = other::zeta;

      result = boost::math::tools::test(
         data,
         bind_func(funcp, 0),
         extract_result(1));
      handle_test_result(result, data[result.worst()], result.worst(), type_name, "other::zeta", test_name);
   }
#endif
   std::cout << std::endl;
}
template <class T>
void test_zeta(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
#include "zeta_data.ipp"
   do_test_zeta(zeta_data, name, "Zeta: Random values greater than 1");
#include "zeta_neg_data.ipp"
   do_test_zeta(zeta_neg_data, name, "Zeta: Random values less than 1");
#include "zeta_1_up_data.ipp"
   do_test_zeta(zeta_1_up_data, name, "Zeta: Values close to and greater than 1");
#include "zeta_1_below_data.ipp"
   do_test_zeta(zeta_1_below_data, name, "Zeta: Values close to and less than 1");
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
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(0.125)), static_cast<T>(-0.63277562349869525529352526763564627152686379131122L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(1023) / static_cast<T>(1024)), static_cast<T>(-1023.4228554489429786541032870895167448906103303056L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(1025) / static_cast<T>(1024)), static_cast<T>(1024.5772867695045940578681624248887776501597556226L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(0.5)), static_cast<T>(-1.46035450880958681288949915251529801246722933101258149054289L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(1.125)), static_cast<T>(8.5862412945105752999607544082693023591996301183069L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(2)), static_cast<T>(1.6449340668482264364724151666460251892189499012068L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(3.5)), static_cast<T>(1.1267338673170566464278124918549842722219969574036L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(4)), static_cast<T>(1.08232323371113819151600369654116790277475095191872690768298L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(4 + static_cast<T>(1) / 1024), static_cast<T>(1.08225596856391369799036835439238249195298434901488518878804L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(4.5)), static_cast<T>(1.05470751076145426402296728896028011727249383295625173068468L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(6.5)), static_cast<T>(1.01200589988852479610078491680478352908773213619144808841031L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(7.5)), static_cast<T>(1.00582672753652280770224164440459408011782510096320822989663L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(8.125)), static_cast<T>(1.0037305205308161603183307711439385250181080293472L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(16.125)), static_cast<T>(1.0000140128224754088474783648500235958510030511915L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(0)), static_cast<T>(-0.5L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-0.125)), static_cast<T>(-0.39906966894504503550986928301421235400280637468895L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-1)), static_cast<T>(-0.083333333333333333333333333333333333333333333333333L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-2)), static_cast<T>(0L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-2.5)), static_cast<T>(0.0085169287778503305423585670283444869362759902200745L), tolerance * 3);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-3)), static_cast<T>(0.0083333333333333333333333333333333333333333333333333L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-4)), static_cast<T>(0), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-20)), static_cast<T>(0), tolerance * 100);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-21)), static_cast<T>(-281.46014492753623188405797101449275362318840579710L), tolerance * 100);
   BOOST_CHECK_CLOSE(::boost::math::zeta(static_cast<T>(-30.125)), static_cast<T>(2.2762941726834511267740045451463455513839970804578e7L), tolerance * 100);
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

   test_zeta(0.1F, "float");
   test_zeta(0.1, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_zeta(0.1L, "long double");
   test_zeta(boost::math::concepts::real_concept(0.1), "real_concept");
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}



