//  Copyright Xiaogang Zhang 2006
//  Copyright John Maddock 2006, 2007
//  Copyright Paul A. Bristow 2007

//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifdef _MSC_VER
#  pragma warning(disable : 4756) // overflow in constant arithmetic
// Constants are too big for float case, but this doesn't matter for test.
#endif

#include <boost/math/concepts/real_concept.hpp>
#include <boost/test/included/test_exec_monitor.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/math/special_functions/ellint_3.hpp>
#include <boost/array.hpp>
#include "functor.hpp"

#include "handle_test_result.hpp"
//
// DESCRIPTION:
// ~~~~~~~~~~~~
//
// This file tests the Elliptic Integrals of the third kind.
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
      ".*Large.*",      // test data group
      ".*", 50, 20);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                  // test type(s)
      ".*Large.*",      // test data group
      ".*", 50, 20);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",      // test data group
      ".*", 15, 8);  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "real_concept",                  // test type(s)
      ".*",      // test data group
      ".*", 15, 8);  // test function
   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", "
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <typename T>
void do_test_ellint_pi3(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
    value_type (*fp2)(value_type, value_type, value_type) = boost::math::ellint_3<value_type, value_type, value_type>;
#else
    value_type (*fp2)(value_type, value_type, value_type) = boost::math::ellint_3;
#endif
    boost::math::tools::test_result<value_type> result;

    result = boost::math::tools::test(
      data,
      bind_func(fp2, 2, 0, 1),
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(),
      type_name, "boost::math::ellint_3", test);

   std::cout << std::endl;

}

template <typename T>
void do_test_ellint_pi2(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
    value_type (*fp2)(value_type, value_type) = boost::math::ellint_3<value_type, value_type>;
#else
    value_type (*fp2)(value_type, value_type) = boost::math::ellint_3;
#endif
    boost::math::tools::test_result<value_type> result;

    result = boost::math::tools::test(
      data,
      bind_func(fp2, 1, 0),
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(),
      type_name, "boost::math::ellint_3", test);

   std::cout << std::endl;

}

template <typename T>
void test_spots(T, const char* type_name)
{
    // function values calculated on http://functions.wolfram.com/
    #define SC_(x) static_cast<T>(BOOST_JOIN(x, L))
    static const boost::array<boost::array<T, 4>, 21> data1 = {
        SC_(1), SC_(-1), SC_(0), SC_(-1.557407724654902230506974807458360173087),
        SC_(0), SC_(-4), SC_(0.4), SC_(-4.153623371196831087495427530365430979011),
        SC_(0), SC_(8), SC_(-0.6), SC_(8.935930619078575123490612395578518914416),
        SC_(0), SC_(0.5), SC_(0.25), SC_(0.501246705365439492445236118603525029757890291780157969500480),
        SC_(0), SC_(0.5), SC_(0), SC_(0.5),
        SC_(-2), SC_(0.5), SC_(0), SC_(0.437501067017546278595664813509803743009132067629603474488486),
        SC_(0.25), SC_(0.5), SC_(0), SC_(0.510269830229213412212501938035914557628394166585442994564135),
        SC_(0.75), SC_(0.5), SC_(0), SC_(0.533293253875952645421201146925578536430596894471541312806165),
        SC_(0.75), SC_(0.75), SC_(0), SC_(0.871827580412760575085768367421866079353646112288567703061975),
        SC_(1), SC_(0.25), SC_(0), SC_(0.255341921221036266504482236490473678204201638800822621740476),
        SC_(2), SC_(0.25), SC_(0), SC_(0.261119051639220165094943572468224137699644963125853641716219),
        SC_(1023)/1024, SC_(1.5), SC_(0), SC_(13.2821612239764190363647953338544569682942329604483733197131),
        SC_(0.5), SC_(-1), SC_(0.5), SC_(-1.228014414316220642611298946293865487807),
        SC_(0.5), SC_(1e+10), SC_(0.5), SC_(1.536591003599172091573590441336982730551e+10),
        SC_(-1e+05), SC_(10), SC_(0.75), SC_(0.0347926099493147087821620459290460547131012904008557007934290),
        SC_(-1e+10), SC_(10), SC_(0.875), SC_(0.000109956202759561502329123384755016959364346382187364656768212),
        SC_(-1e+10), SC_(1e+20), SC_(0.875), SC_(1.00000626665567332602765201107198822183913978895904937646809e15),
        SC_(-1e+10), SC_(1608)/1024, SC_(0.875), SC_(0.0000157080616044072676127333183571107873332593142625043567690379),
        1-SC_(1) / 1024, SC_(1e+20), SC_(0.875), SC_(6.43274293944380717581167058274600202023334985100499739678963e21),
        SC_(50), SC_(0.1), SC_(0.25), SC_(0.124573770342749525407523258569507331686458866564082916835900),
        SC_(1.125), SC_(1), SC_(0.25), SC_(1.77299767784815770192352979665283069318388205110727241629752),
        //SC_(1.125), SC_(10), SC_(0.25), SC_(0.662467818678976949597336360256848770217429434745967677192487),
        //SC_(1.125), SC_(3), SC_(0.25), SC_(-0.142697285116693775525461312178015106079842313950476205580178),
    };
    #undef SC_

    do_test_ellint_pi3(data1, type_name, "Elliptic Integral PI: Mathworld Data");

#include "ellint_pi3_data.ipp"

    do_test_ellint_pi3(ellint_pi3_data, type_name, "Elliptic Integral PI: Random Data");

#include "ellint_pi3_large_data.ipp"

    do_test_ellint_pi3(ellint_pi3_large_data, type_name, "Elliptic Integral PI: Large Random Data");

    // function values calculated on http://functions.wolfram.com/
    #define SC_(x) static_cast<T>(BOOST_JOIN(x, L))
    static const boost::array<boost::array<T, 3>, 9> data2 = {
        SC_(0), SC_(0.2), SC_(1.586867847454166237308008033828114192951),
        SC_(0), SC_(0.4), SC_(1.639999865864511206865258329748601457626),
        SC_(0), SC_(0), SC_(1.57079632679489661923132169163975144209858469968755291048747),
        SC_(0.5), SC_(0), SC_(2.221441469079183123507940495030346849307),
        SC_(-4), SC_(0.3), SC_(0.712708870925620061597924858162260293305195624270730660081949),
        SC_(-1e+05), SC_(-0.5), SC_(0.00496944596485066055800109163256108604615568144080386919012831),
        SC_(-1e+10), SC_(-0.75), SC_(0.0000157080225184890546939710019277357161497407143903832703317801),
        SC_(1) / 1024, SC_(-0.875), SC_(2.18674503176462374414944618968850352696579451638002110619287),
        SC_(1023)/1024, SC_(-0.875), SC_(101.045289804941384100960063898569538919135722087486350366997),
    };
    #undef SC_

    do_test_ellint_pi2(data2, type_name, "Complete Elliptic Integral PI: Mathworld Data");

#include "ellint_pi2_data.ipp"

    do_test_ellint_pi2(ellint_pi2_data, type_name, "Complete Elliptic Integral PI: Random Data");

    // Special cases, exceptions etc:
    BOOST_CHECK_THROW(boost::math::ellint_3(T(1.0001), T(-1), T(0)), std::domain_error);
    BOOST_CHECK_THROW(boost::math::ellint_3(T(0.5), T(20), T(1.5)), std::domain_error);
    BOOST_CHECK_THROW(boost::math::ellint_3(T(1.0001), T(-1)), std::domain_error);
    BOOST_CHECK_THROW(boost::math::ellint_3(T(0.5), T(1)), std::domain_error);
    BOOST_CHECK_THROW(boost::math::ellint_3(T(0.5), T(2)), std::domain_error);
}

int test_main(int, char* [])
{
    expected_results();
    BOOST_MATH_CONTROL_FP;
    test_spots(0.0F, "float");
    test_spots(0.0, "double");
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
    test_spots(0.0L, "long double");
#ifndef BOOST_MATH_NO_REAL_CONCEPT_TESTS
    test_spots(boost::math::concepts::real_concept(0), "real_concept");
#endif
#else
   std::cout << "<note>The long double tests have been disabled on this platform "
      "either because the long double overloads of the usual math functions are "
      "not available at all, or because they are too inaccurate for these tests "
      "to pass.</note>" << std::cout;
#endif

    return 0;
}
