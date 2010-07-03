//  (C) Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <boost/math/concepts/real_concept.hpp>
#include <boost/math/special_functions/expint.hpp>
#include <boost/math/special_functions/trunc.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/tools/stats.hpp>
#include <boost/math/tools/test.hpp>
#include <boost/math/constants/constants.hpp>
#include <boost/type_traits/is_floating_point.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
#include "test_expint_hooks.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the expint functions. There are two sets of tests:
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

   //
   // On MacOS X erfc has much higher error levels than
   // expected: given that the implementation is basically
   // just a rational function evaluation combined with
   // exponentiation, we conclude that exp and pow are less
   // accurate on this platform, especially when the result 
   // is outside the range of a double.
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                      // platform
      "float|double|long double",    // test type(s)
      ".*E1.*",                      // test data group
      ".*", 30, 10);                   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                      // platform
      "float|double|long double",    // test type(s)
      ".*Ei.*",                      // test data group
      ".*", 300, 200);                   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                      // platform
      ".*",                          // test type(s)
      ".*",                          // test data group
      ".*", 40, 15);                   // test function

   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "float|double|long double",    // test type(s)
      ".*E1.*",                      // test data group
      ".*", 2, 1);                   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "float|double|long double",    // test type(s)
      ".*Ei.*",                      // test data group
      ".*", 6, 3);                   // test function
   if(std::numeric_limits<long double>::digits > 100)
   {
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "real_concept",                // test type(s)
         ".*Ei.*",                      // test data group
         ".*", 150, 50);                // test function
   }
   add_expected_result(
      "Sun.*",                       // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*Ei.*",                      // test data group
      ".*", 150, 50);                // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*Ei.*",                      // test data group
      ".*", 50, 20);                 // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      ".*",                          // test type(s)
      ".*",                          // test data group
      ".*", 25, 5);                   // test function

   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
T expint_wrapper(T n, T z)
{
   return boost::math::expint(
      boost::math::itrunc(n), z);
}

#ifdef TEST_OTHER
template <class T>
T other_expint_wrapper(T n, T z)
{
   return other::expint(
      boost::math::itrunc(n), z);
}
#endif
template <class T>
void do_test_expint(const T& data, const char* type_name, const char* test_name)
{
   //
   // test En(T) against data:
   //
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << test_name << " with type " << type_name << std::endl;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = expint_wrapper<value_type>;
#else
   pg funcp = expint_wrapper;
#endif

   boost::math::tools::test_result<value_type> result;
   //
   // test expint against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1),
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::expint", test_name);
#ifdef TEST_OTHER
   if(boost::is_floating_point<value_type>::value && other::expint(2u, 2.0))
   {
      funcp = other_expint_wrapper;
      //
      // test expint against data:
      //
      result = boost::math::tools::test(
         data,
         bind_func(funcp, 0, 1),
         extract_result(2));
      handle_test_result(result, data[result.worst()], result.worst(), type_name, "other::expint", test_name);
   }
#endif
   std::cout << std::endl;
}

template <class T>
void do_test_expint_Ei(const T& data, const char* type_name, const char* test_name)
{
   //
   // test Ei(T) against data:
   //
   using namespace std;
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << test_name << " with type " << type_name << std::endl;

   typedef value_type (*pg)(value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::expint<value_type>;
#else
   pg funcp = boost::math::expint;
#endif

   boost::math::tools::test_result<value_type> result;
   //
   // test expint against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0),
      extract_result(1));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::expint", test_name);
#ifdef TEST_OTHER
   if(boost::is_floating_point<value_type>::value && other::expint(2.0))
   {
      funcp = other::expint;
      //
      // test expint against data:
      //
      result = boost::math::tools::test(
         data,
         bind_func(funcp, 0),
         extract_result(1));
      handle_test_result(result, data[result.worst()], result.worst(), type_name, "other::expint", test_name);
   }
#endif
}

template <class T>
void test_expint(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
#include "expint_data.ipp"
   do_test_expint(expint_data, name, "Exponential Integral En");
#include "expint_small_data.ipp"
   do_test_expint(expint_small_data, name, "Exponential Integral En: small z values");
#include "expint_1_data.ipp"
   do_test_expint(expint_1_data, name, "Exponential Integral E1");
#include "expinti_data.ipp"
   do_test_expint_Ei(expinti_data, name, "Exponential Integral Ei");

   if(boost::math::tools::log_max_value<T>() > 100)
   {
#include "expinti_data_double.ipp"
      do_test_expint_Ei(expinti_data_double, name, "Exponential Integral Ei: double exponent range");
   }
#if defined(LDBL_MAX_10_EXP) && (LDBL_MAX_10_EXP > 2000)
   if(boost::math::tools::log_max_value<T>() > 1000)
   {
#include "expinti_data_long.ipp"
      do_test_expint_Ei(expinti_data_long, name, "Exponential Integral Ei: long exponent range");
   }
#endif
}

template <class T>
void test_spots(T, const char* t)
{
   std::cout << "Testing basic sanity checks for type " << t << std::endl;
   //
   // Basic sanity checks, tolerance is 100 epsilon 
   // expressed as a percentage:
   //
   T tolerance = boost::math::tools::epsilon<T>() * 100 *
      (boost::is_floating_point<T>::value ? 500 : 500);
   //
   // En:
   //
   BOOST_CHECK_CLOSE(::boost::math::expint(0, static_cast<T>(1)/1024), static_cast<T>(1023.0004881223430781283448725609773366468629307172L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(0, static_cast<T>(0.125F)), static_cast<T>(7.0599752206767632229191371458324058897760385999252L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(0, static_cast<T>(0.5F)), static_cast<T>(1.2130613194252668472075990699823609068838362709744L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(0, static_cast<T>(1.5F)), static_cast<T>(0.14875344009895321928885364717600834756144775290739L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(0, static_cast<T>(4.5F)), static_cast<T>(0.0024686658973871792213651409526512283936753927790603L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(0, static_cast<T>(50.0F)), static_cast<T>(3.8574996959278355660346856330540251495056653024605e-24L), tolerance);

   BOOST_CHECK_CLOSE(::boost::math::expint(1, static_cast<T>(1)/1024), static_cast<T>(6.3552324648310718026144555193580322129376300855378L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(1, static_cast<T>(0.125F)), static_cast<T>(1.6234256405841687914563069246244088736331060573721L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(1, static_cast<T>(0.5F)), static_cast<T>(0.55977359477616081174679593931508523522684689031635L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(1, static_cast<T>(1.5F)), static_cast<T>(0.10001958240663265190190933991166697826173000614035L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(1, static_cast<T>(4.5F)), static_cast<T>(0.0020734007547146144328855938695797884889319725701443L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(1, static_cast<T>(50.0F)), static_cast<T>(3.7832640295504590186989678540212857803028931862511e-24L), tolerance);

   BOOST_CHECK_CLOSE(::boost::math::expint(2, static_cast<T>(1)/1024), static_cast<T>(0.99281763247803906867747111039220635198625517639812L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(2, static_cast<T>(0.125F)), static_cast<T>(0.67956869751157430393285377765099962701786656781914L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(2, static_cast<T>(0.5F)), static_cast<T>(0.32664386232455301773040156533363783582849469032901L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(2, static_cast<T>(1.5F)), static_cast<T>(0.073100786538480851080416460896512053949576620150553L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(2, static_cast<T>(4.5F)), static_cast<T>(0.0017786931420265415481579618738214795713453909401220L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(2, static_cast<T>(50.0F)), static_cast<T>(3.7117833188688273667858889516369684601386058104706e-24L), tolerance);

   BOOST_CHECK_CLOSE(::boost::math::expint(5, static_cast<T>(1)/1024), static_cast<T>(0.24967471743034509414923673526350536071348482068601L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(5, static_cast<T>(0.125F)), static_cast<T>(0.21195078838966585733668853784460504343264488743527L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(5, static_cast<T>(0.5F)), static_cast<T>(0.13097731169586484777931864012654136046214618229236L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(5, static_cast<T>(1.5F)), static_cast<T>(0.038529924425495155395971538166055731456940831714065L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(5, static_cast<T>(4.5F)), static_cast<T>(0.0012311157382296328534406162790653865883418172939975L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(5, static_cast<T>(50.0F)), static_cast<T>(3.5124400631332394056901125740903320085753990490695e-24L), tolerance);

   BOOST_CHECK_CLOSE(::boost::math::expint(22, static_cast<T>(1)/1024), static_cast<T>(0.047570244582119027573000641518739337055625638422014L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(22, static_cast<T>(0.125F)), static_cast<T>(0.041762730174712898120606793100375713866392079558609L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(22, static_cast<T>(0.5F)), static_cast<T>(0.028178840877963713109230354192609362941651630844684L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(22, static_cast<T>(1.5F)), static_cast<T>(0.0098864453561701486668317763826728620676449196215016L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(22, static_cast<T>(4.5F)), static_cast<T>(0.00043257793497205419001613830279995985617548506505159L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(22, static_cast<T>(50.0F)), static_cast<T>(2.6900194251201629500599598206345018300567305625080e-24L), tolerance);
   //
   // Ei:
   //
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(1)/1024), static_cast<T>(-6.35327933972759151358547423727042905862963067106751711596065L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(0.125)), static_cast<T>(-1.37320852494298333781545045921206470808223543321810480716122L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(0.5)), static_cast<T>(0.454219904863173579920523812662802365281405554352642045162818L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(1)), static_cast<T>(1.89511781635593675546652093433163426901706058173270759164623L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(50.5)), static_cast<T>(1.72763195602911805201155668940185673806099654090456049881069e20L), tolerance);

   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(-1)/1024), static_cast<T>(-6.35523246483107180261445551935803221293763008553775821607264L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(-0.125)), static_cast<T>(-1.62342564058416879145630692462440887363310605737209536579267L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(-0.5)), static_cast<T>(-0.559773594776160811746795939315085235226846890316353515248293L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(-1)), static_cast<T>(-0.219383934395520273677163775460121649031047293406908207577979L), tolerance);
   BOOST_CHECK_CLOSE(::boost::math::expint(static_cast<T>(-50.5)), static_cast<T>(-2.27237132932219350440719707268817831250090574830769670186618e-24L), tolerance);
}

int test_main(int, char* [])
{
   expected_results();
   BOOST_MATH_CONTROL_FP;

   boost::math::expint(114.7);

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

   test_expint(0.1F, "float");
   test_expint(0.1, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_expint(0.1L, "long double");
   test_expint(boost::math::concepts::real_concept(0.1), "real_concept");
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}

