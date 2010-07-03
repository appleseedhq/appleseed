//  (C) Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_MATH_OVERFLOW_ERROR_POLICY ignore_error
#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/bessel.hpp>
#include <boost/type_traits/is_floating_point.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
#include "test_bessel_hooks.hpp"

//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the bessel Y function.  There are two sets of tests, spot
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
      largest_type = "(long\\s+)?double|real_concept";
   }
   else
   {
      largest_type = "long double|real_concept";
   }
#else
   largest_type = "(long\\s+)?double";
#endif

   //
   // HP-UX and Solaris rates are very slightly higher:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "HP-UX|Sun Solaris",                          // platform
      largest_type,                // test type(s)
      ".*(Y[nv]|y).*Random.*",           // test data group
      ".*", 30000, 30000);             // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "HP-UX|Sun Solaris",                          // platform
      largest_type,                  // test type(s)
      ".*Y[01Nv].*",           // test data group
      ".*", 1300, 500);               // test function
   //
   // Tru64:
   //
   add_expected_result(
      ".*Tru64.*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                // test type(s)
      ".*(Y[nv]|y).*Random.*",           // test data group
      ".*", 30000, 30000);             // test function
   add_expected_result(
      ".*Tru64.*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                      // test type(s)
      ".*Y[01Nv].*",           // test data group
      ".*", 400, 200);               // test function

   //
   // Mac OS X rates are very slightly higher:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                          // platform
      largest_type,                // test type(s)
      ".*(Y[nv1]).*",           // test data group
      ".*", 600000, 100000);             // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                          // platform
      "long double|real_concept",        // test type(s)
      ".*Y[0].*",           // test data group
      ".*", 1200, 1000);               // test function

   //
   // Linux:
   //
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         "linux",                          // platform
         largest_type,                  // test type(s)
         ".*Yv.*Random.*",              // test data group
         ".*", 200000, 200000);         // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         "linux",                          // platform
         largest_type,                  // test type(s)
         ".*Y[01v].*",              // test data group
         ".*", 2000, 1000);         // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         "linux",                          // platform
         largest_type,                  // test type(s)
         ".*Yn.*",              // test data group
         ".*", 30000, 30000);         // test function
   //
   // MinGW:
   //
      add_expected_result(
         ".*mingw.*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         largest_type,                  // test type(s)
         ".*Yv.*Random.*",              // test data group
         ".*", 200000, 200000);         // test function
      add_expected_result(
         ".*mingw.*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         largest_type,                  // test type(s)
         ".*Y[01v].*",              // test data group
         ".*", 2000, 1000);         // test function
      add_expected_result(
         ".*mingw.*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         largest_type,                  // test type(s)
         ".*Yn.*",              // test data group
         ".*", 30000, 30000);         // test function
   //
   // Solaris version of long double has it's own error rates,
   // again just a touch higher than msvc's 64-bit double:
   //
   add_expected_result(
      "GNU.*",                          // compiler
      ".*",                          // stdlib
      "Sun.*",                          // platform
      largest_type,                  // test type(s)
      "Y[0N].*Mathworld.*",              // test data group
      ".*", 2000, 2000);         // test function

#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   if((std::numeric_limits<double>::digits != std::numeric_limits<long double>::digits)
      && (std::numeric_limits<long double>::digits < 90))
   {
      // some errors spill over into type double as well:
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "double",                      // test type(s)
         ".*Y[Nn].*",              // test data group
         ".*", 20, 20);         // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "double",                      // test type(s)
         ".*Yv.*",              // test data group
         ".*", 80, 70);         // test function
   }
#endif
   //
   // defaults are based on MSVC-8 on Win32:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*Y0.*Random.*",           // test data group
      ".*", 600, 400);               // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                // test type(s)
      ".*(Y[nv]|y).*Random.*",           // test data group
      ".*", 2000, 2000);             // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*(Y[nv]|y).*Random.*",           // test data group
      ".*", 1500, 1000);               // test function
   //
   // Fallback for sun has to go after the general cases above:
   //
   add_expected_result(
      "GNU.*",                          // compiler
      ".*",                          // stdlib
      "Sun.*",                          // platform
      largest_type,                  // test type(s)
      "Y[0N].*",              // test data group
      ".*", 200, 200);         // test function
   //
   // General fallback:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 60, 40);                 // test function
   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test_cyl_neumann_y(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::cyl_neumann<value_type, value_type>;
#else
   pg funcp = boost::math::cyl_neumann;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test cyl_neumann against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::cyl_neumann", test_name);
   std::cout << std::endl;

#ifdef TEST_OTHER
   if(boost::is_floating_point<value_type>::value)
   {
      funcp = other::cyl_neumann;

      //
      // test other::cyl_neumann against data:
      //
      result = boost::math::tools::test(
         data, 
         bind_func(funcp, 0, 1), 
         extract_result(2));
      handle_test_result(result, data[result.worst()], result.worst(), type_name, "other::cyl_neumann", test_name);
      std::cout << std::endl;
   }
#endif
}

template <class T>
T cyl_neumann_int_wrapper(T v, T x)
{
   return static_cast<T>(boost::math::cyl_neumann(boost::math::itrunc(v), x));
}

template <class T>
void do_test_cyl_neumann_y_int(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = cyl_neumann_int_wrapper<value_type>;
#else
   pg funcp = cyl_neumann_int_wrapper;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test cyl_neumann against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::cyl_neumann", test_name);
   std::cout << std::endl;
}

template <class T>
void do_test_sph_neumann_y(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(unsigned, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::sph_neumann<value_type>;
#else
   pg funcp = boost::math::sph_neumann;
#endif

   typedef int (*cast_t)(value_type);

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test sph_neumann against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func_int1(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::cyl_neumann", test_name);
   std::cout << std::endl;
}

template <class T>
void test_bessel(T, const char* name)
{
   //
   // The actual test data is rather verbose, so it's in a separate file
   //
   // The contents are as follows, each row of data contains
   // three items, input value a, input value b and erf(a, b):
   // 
    // function values calculated on http://functions.wolfram.com/
    #define SC_(x) static_cast<T>(BOOST_JOIN(x, L))
    static const boost::array<boost::array<T, 3>, 9> y0_data = {{
        SC_(0), SC_(1), SC_(0.0882569642156769579829267660235151628278175230906755467110438),
        SC_(0), SC_(2), SC_(0.510375672649745119596606592727157873268139227085846135571839),
        SC_(0), SC_(4), SC_(-0.0169407393250649919036351344471532182404925898980149027169321),
        SC_(0), SC_(8), SC_(0.223521489387566220527323400498620359274814930781423577578334),
        SC_(0), SC_(1e-05), SC_(-7.40316028370197013259676050746759072070960287586102867247159),
        SC_(0), SC_(1e-10), SC_(-14.7325162726972420426916696426209144888762342592762415255386),
        SC_(0), SC_(1e-20), SC_(-29.3912282502857968601858410375186700783698345615477536431464),
        SC_(0), SC_(1e+03), SC_(0.00471591797762281339977326146566525500985900489680197718528000),
        SC_(0), SC_(1e+05), SC_(0.00184676615886506410434074102431546125884886798090392516843524)
    }};
    static const boost::array<boost::array<T, 3>, 9> y1_data = {
        SC_(1), SC_(1), SC_(-0.781212821300288716547150000047964820549906390716444607843833),
        SC_(1), SC_(2), SC_(-0.107032431540937546888370772277476636687480898235053860525795),
        SC_(1), SC_(4), SC_(0.397925710557100005253979972450791852271189181622908340876586),
        SC_(1), SC_(8), SC_(-0.158060461731247494255555266187483550327344049526705737651263),
        SC_(1), SC_(1e-10), SC_(-6.36619772367581343150789184284462611709080831190542841855708e9),
        SC_(1), SC_(1e-20), SC_(-6.36619772367581343075535053490057448139324059868649274367256e19),
        SC_(1), SC_(1e+01), SC_(0.249015424206953883923283474663222803260416543069658461246944),
        SC_(1), SC_(1e+03), SC_(-0.0247843312923517789148623560971412909386318548648705287583490),
        SC_(1), SC_(1e+05), SC_(0.00171921035008825630099494523539897102954509504993494957572726)
    };
    static const boost::array<boost::array<T, 3>, 9> yn_data = {
        SC_(2), SC_(1e-20), SC_(-1.27323954473516268615107010698011489627570899691226996904849e40),
        SC_(5), SC_(10), SC_(0.135403047689362303197029014762241709088405766746419538495983),
        SC_(-5), SC_(1e+06), SC_(0.000331052088322609048503535570014688967096938338061796192422114),
        SC_(10), SC_(10), SC_(-0.359814152183402722051986577343560609358382147846904467526222),
        SC_(10), SC_(1e-10), SC_(-1.18280490494334933900960937719565669877576135140014365217993e108),
        SC_(-10), SC_(1e+06), SC_(0.000725951969295187086245251366365393653610914686201194434805730),
        SC_(1e+02), SC_(5), SC_(-5.08486391602022287993091563093082035595081274976837280338134e115),
        SC_(1e+03), SC_(1e+05), SC_(0.00217254919137684037092834146629212647764581965821326561261181),
        SC_(-1e+03), SC_(7e+02), SC_(-1.88753109980945889960843803284345261796244752396992106755091e77)
    };
    static const boost::array<boost::array<T, 3>, 9> yv_data = {
        //SC_(2.25), SC_(1) / 1024, SC_(-1.01759203636941035147948317764932151601257765988969544340275e7),
        SC_(0.5), SC_(1) / (1024*1024), SC_(-817.033790261762580469303126467917092806755460418223776544122),
        SC_(5.5), SC_(3.125), SC_(-2.61489440328417468776474188539366752698192046890955453259866),
        SC_(-5.5), SC_(3.125), SC_(-0.0274994493896489729948109971802244976377957234563871795364056),
        SC_(-5.5), SC_(1e+04), SC_(-0.00759343502722670361395585198154817047185480147294665270646578),
        SC_(-10486074) / (1024*1024), SC_(1)/1024, SC_(-1.50382374389531766117868938966858995093408410498915220070230e38),
        SC_(-10486074) / (1024*1024), SC_(1e+02), SC_(0.0583041891319026009955779707640455341990844522293730214223545),
        SC_(141.75), SC_(1e+02), SC_(-5.38829231428696507293191118661269920130838607482708483122068e9),
        SC_(141.75), SC_(2e+04), SC_(-0.00376577888677186194728129112270988602876597726657372330194186),
        SC_(-141.75), SC_(1e+02), SC_(-3.81009803444766877495905954105669819951653361036342457919021e9),
    };

    do_test_cyl_neumann_y(y0_data, name, "Y0: Mathworld Data");
    do_test_cyl_neumann_y(y1_data, name, "Y1: Mathworld Data");
    do_test_cyl_neumann_y(yn_data, name, "Yn: Mathworld Data");
    do_test_cyl_neumann_y_int(y0_data, name, "Y0: Mathworld Data (Integer Version)");
    do_test_cyl_neumann_y_int(y1_data, name, "Y1: Mathworld Data (Integer Version)");
    do_test_cyl_neumann_y_int(yn_data, name, "Yn: Mathworld Data (Integer Version)");
    do_test_cyl_neumann_y(yv_data, name, "Yv: Mathworld Data");

#include "bessel_y01_data.ipp"
    do_test_cyl_neumann_y(bessel_y01_data, name, "Y0 and Y1: Random Data");
#include "bessel_yn_data.ipp"
    do_test_cyl_neumann_y(bessel_yn_data, name, "Yn: Random Data");
#include "bessel_yv_data.ipp"
    do_test_cyl_neumann_y(bessel_yv_data, name, "Yv: Random Data");

#include "sph_neumann_data.ipp"
    do_test_sph_neumann_y(sph_neumann_data, name, "y: Random Data");
}

int test_main(int, char* [])
{
#ifdef TEST_GSL
   gsl_set_error_handler_off();
#endif
   expected_results();
   BOOST_MATH_CONTROL_FP;

#ifndef BOOST_MATH_BUGGY_LARGE_FLOAT_CONSTANTS
   test_bessel(0.1F, "float");
#endif
   test_bessel(0.1, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_bessel(0.1L, "long double");
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
   test_bessel(boost::math::concepts::real_concept(0.1), "real_concept");
#endif
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif
   return 0;
}




