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
// This file tests the incomplete beta functions beta, 
// betac, ibeta and ibetac.  There are two sets of tests, spot
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
   // Darwin: just one special case for real_concept:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "Mac OS",                          // platform
      "real_concept",                   // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 400000, 50000);             // test function

   //
   // Linux - results depend quite a bit on the
   // processor type, and how good the std::pow
   // function is for that processor.
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux",                          // platform
      largest_type,                     // test type(s)
      "(?i).*small.*",                  // test data group
      ".*", 350, 100);  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux",                          // platform
      largest_type,                     // test type(s)
      "(?i).*medium.*",                     // test data group
      ".*", 300, 80);  // test function
   //
   // Deficiencies in pow function really kick in here for
   // large arguments.  Note also that the tests here get
   // *very* extreme due to the increased exponent range
   // of 80-bit long doubles.  Also effect Mac OS.
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux|Mac OS",                          // platform
      largest_type,                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 200000, 10000);                 // test function
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux|Mac OS|Sun.*",             // platform
      "double",                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 40, 20);                 // test function
#endif
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "linux|Mac OS",                          // platform
      "real_concept",                   // test type(s)
      "(?i).*medium.*",                 // test data group
      ".*", 350, 100);  // test function

   //
   // HP-UX:
   //
   // Large value tests include some with *very* extreme
   // results, thanks to the large exponent range of
   // 128-bit long doubles.
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "HP-UX",                          // platform
      largest_type,                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 200000, 10000);                 // test function
   //
   // Tru64:
   //
   add_expected_result(
      ".*Tru64.*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 130000, 10000);                 // test function
   //
   // Sun OS:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "Sun.*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 130000, 10000);                 // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "Sun.*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*small.*",                      // test data group
      ".*", 130, 30);                 // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "Sun.*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*medium.*",                 // test data group
      ".*", 200, 40);                   // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "Sun.*",                          // platform
      "real_concept",                   // test type(s)
      "(?i).*medium.*",                 // test data group
      ".*", 200, 40);                   // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "Sun.*",                          // platform
      "real_concept",                     // test type(s)
      "(?i).*small.*",                      // test data group
      ".*", 130, 30);                 // test function
   //
   // MinGW:
   //
   add_expected_result(
      "[^|]*mingw[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      ".*",                          // platform
      "double",                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 20, 10);                 // test function
   add_expected_result(
      "[^|]*mingw[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      ".*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 200000, 10000);                 // test function

#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   //
   // No long doubles:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      BOOST_PLATFORM,                          // platform
      largest_type,                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 13000, 500);                 // test function
#endif
   //
   // Catch all cases come last:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*small.*",                  // test data group
      ".*", 60, 10);  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*medium.*",                     // test data group
      ".*", 150, 50);  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 5000, 500);                 // test function

   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                   // test type(s)
      "(?i).*small.*",                      // test data group
      ".*", 60, 15);  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                   // test type(s)
      "(?i).*medium.*",                     // test data group
      ".*", 200, 50);  // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                   // test type(s)
      "(?i).*large.*",                      // test data group
      ".*", 200000, 50000);             // test function

   // catch all default is 2eps for all types:
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "[^|]*",                          // test type(s)
      "[^|]*",                          // test data group
      ".*", 2, 2);                      // test function
   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test_beta(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::beta<value_type, value_type, value_type>;
#else
   pg funcp = boost::math::beta;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test beta against data:
   //
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1, 2),
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::beta", test_name);

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   funcp = boost::math::betac<value_type, value_type, value_type>;
#else
   funcp = boost::math::betac;
#endif
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1, 2),
      extract_result(4));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::betac", test_name);

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   funcp = boost::math::ibeta<value_type, value_type, value_type>;
#else
   funcp = boost::math::ibeta;
#endif
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1, 2),
      extract_result(5));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::ibeta", test_name);

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   funcp = boost::math::ibetac<value_type, value_type, value_type>;
#else
   funcp = boost::math::ibetac;
#endif
   result = boost::math::tools::test(
      data,
      bind_func(funcp, 0, 1, 2),
      extract_result(6));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::ibetac", test_name);
#ifdef TEST_OTHER
   if(::boost::is_floating_point<value_type>::value){
      funcp = other::ibeta;
      result = boost::math::tools::test(
         data,
         bind_func(funcp, 0, 1, 2),
         extract_result(5));
      print_test_result(result, data[result.worst()], result.worst(), type_name, "other::ibeta");
   }
#endif
   std::cout << std::endl;
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

   do_test_beta(ibeta_small_data, name, "Incomplete Beta Function: Small Values");
#endif

#if !defined(TEST_DATA) || (TEST_DATA == 2)
#  include "ibeta_data.ipp"

   do_test_beta(ibeta_data, name, "Incomplete Beta Function: Medium Values");

#endif
#if !defined(TEST_DATA) || (TEST_DATA == 3)
#  include "ibeta_large_data.ipp"

   do_test_beta(ibeta_large_data, name, "Incomplete Beta Function: Large and Diverse Values");
#endif

#if !defined(TEST_DATA) || (TEST_DATA == 4)
#  include "ibeta_int_data.ipp"

   do_test_beta(ibeta_int_data, name, "Incomplete Beta Function: Small Integer Values");
#endif
}

template <class T>
void test_spots(T)
{
   //
   // basic sanity checks, tolerance is 30 epsilon expressed as a percentage:
   //
   T tolerance = boost::math::tools::epsilon<T>() * 3000;
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(159) / 10000, //(0.015964560210704803L),
         static_cast<T>(1184) / 1000000000L,//(1.1846856068586931e-005L),
         static_cast<T>(6917) / 10000),//(0.69176378846168518L)),
      static_cast<T>(0.000075393541456247525676062058821484095548666733251733L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(4243) / 100,//(42.434902191162109L),
         static_cast<T>(3001) / 10000, //(0.30012050271034241L),
         static_cast<T>(9157) / 10000), //(0.91574394702911377L)),
      static_cast<T>(0.0028387319012616013434124297160711532419664289474798L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(9713) / 1000, //(9.7131776809692383L),
         static_cast<T>(9940) / 100, //(99.406852722167969L),
         static_cast<T>(8391) / 100000), //(0.083912998437881470L)),
      static_cast<T>(0.46116895440368248909937863372410093344466819447476L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(72.5),
         static_cast<T>(1.125),
         static_cast<T>(0.75)),
      static_cast<T>(1.3423066982487051710597194786268004978931316494920e-9L), tolerance*3); // extra tolerance needed on linux X86EM64
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(4985)/1000, //(4.9854421615600586L),
         static_cast<T>(1066)/1000, //(1.0665277242660522L),
         static_cast<T>(7599)/10000), //(0.75997146964073181L)),
      static_cast<T>(0.27533431334486812211032939156910472371928659321347L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(6813)/1000, //(6.8127136230468750L),
         static_cast<T>(1056)/1000, //(1.0562920570373535L),
         static_cast<T>(1741)/10000), //(0.17416560649871826L)),
      static_cast<T>(7.6736128722762245852815040810349072461658078840945e-6L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(4898)/10000, //(0.48983201384544373L),
         static_cast<T>(2251)/10000, //(0.22512593865394592L),
         static_cast<T>(2003)/10000), //(0.20032680034637451L)),
      static_cast<T>(0.17089223868046209692215231702890838878342349377008L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(4049)/1000, //(4.0498137474060059L),
         static_cast<T>(1540)/10000, //(0.15403440594673157L),
         static_cast<T>(6537)/10000), //(0.65370121598243713L)),
      static_cast<T>(0.017273988301528087878279199511703371301647583919670L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(7269)/1000, //(7.2695474624633789L),
         static_cast<T>(1190)/10000, //(0.11902070045471191L),
         static_cast<T>(8003)/10000), //(0.80036874115467072L)),
      static_cast<T>(0.013334694467796052900138431733772122625376753696347L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(2726)/1000, //(2.7266697883605957L),
         static_cast<T>(1151)/100000, //(0.011510574258863926L),
         static_cast<T>(8665)/100000), //(0.086654007434844971L)),
      static_cast<T>(5.8218877068298586420691288375690562915515260230173e-6L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(3431)/10000, //(0.34317314624786377L),
         static_cast<T>(4634)/100000, //0.046342257410287857L),
         static_cast<T>(7582)/10000), //(0.75823287665843964L)),
      static_cast<T>(0.15132819929418661038699397753916091907278005695387L), tolerance);

   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(0.34317314624786377L),
         static_cast<T>(0.046342257410287857L),
         static_cast<T>(0)),
      static_cast<T>(0), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibetac(
         static_cast<T>(0.34317314624786377L),
         static_cast<T>(0.046342257410287857L),
         static_cast<T>(0)),
      static_cast<T>(1), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(0.34317314624786377L),
         static_cast<T>(0.046342257410287857L),
         static_cast<T>(1)),
      static_cast<T>(1), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibetac(
         static_cast<T>(0.34317314624786377L),
         static_cast<T>(0.046342257410287857L),
         static_cast<T>(1)),
      static_cast<T>(0), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(1),
         static_cast<T>(4634)/100000, //(0.046342257410287857L),
         static_cast<T>(32)/100),
      static_cast<T>(0.017712849440718489999419956301675684844663359595318L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(4634)/100000, //(0.046342257410287857L),
         static_cast<T>(1),
         static_cast<T>(32)/100),
      static_cast<T>(0.94856839398626914764591440181367780660208493234722L), tolerance);
   
   // try with some integer arguments:
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(3),
         static_cast<T>(8),
         static_cast<T>(0.25)),
      static_cast<T>(0.474407196044921875000000000000000000000000000000000000000000L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(6),
         static_cast<T>(8),
         static_cast<T>(0.25)),
      static_cast<T>(0.0802125930786132812500000000000000000000000000000000000000000L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(12),
         static_cast<T>(1),
         static_cast<T>(0.25)),
      static_cast<T>(5.96046447753906250000000000000000000000000000000000000000000e-8L), tolerance);
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta(
         static_cast<T>(1),
         static_cast<T>(8),
         static_cast<T>(0.25)),
      static_cast<T>(0.899887084960937500000000000000000000000000000000000000000000L), tolerance);

   // very naive check on derivative:
   using namespace std;  // For ADL of std functions
   tolerance = boost::math::tools::epsilon<T>() * 10000; // 100 eps
   BOOST_CHECK_CLOSE(
      ::boost::math::ibeta_derivative(
         static_cast<T>(2),
         static_cast<T>(3),
         static_cast<T>(0.5)),
         pow(static_cast<T>(0.5), static_cast<T>(2)) * pow(static_cast<T>(0.5), static_cast<T>(1)) / boost::math::beta(static_cast<T>(2), static_cast<T>(3)), tolerance);
}

int test_main(int, char* [])
{
   expected_results();
   BOOST_MATH_CONTROL_FP;
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
#if !BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x582))
#ifdef TEST_REAL_CONCEPT
   test_spots(boost::math::concepts::real_concept(0.1));
#endif
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





