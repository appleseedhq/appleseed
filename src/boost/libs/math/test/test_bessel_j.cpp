//  (C) Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

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
// This file tests the bessel functions.  There are two sets of tests, spot
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
   // HP-UX specific rates:
   //
   // Error rate for double precision are limited by the accuracy of
   // the approximations use, which bracket rather than preserve the root.
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "HP-UX",                          // platform
      largest_type,                      // test type(s)
      ".*J0.*Tricky.*",              // test data group
      ".*", 80000000000LL, 80000000000LL);         // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "HP-UX",                          // platform
      largest_type,                      // test type(s)
      ".*J1.*Tricky.*",              // test data group
      ".*", 3000000, 2000000);         // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "HP-UX",                          // platform
      "double",                      // test type(s)
      ".*Tricky.*",              // test data group
      ".*", 100000, 100000);         // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "HP-UX",                          // platform
      largest_type,                      // test type(s)
      ".*J.*Tricky.*",              // test data group
      ".*", 3000, 500);         // test function
   //
   // HP Tru64:
   //
   add_expected_result(
      ".*Tru64.*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      "double",                      // test type(s)
      ".*Tricky.*",              // test data group
      ".*", 100000, 100000);         // test function
   add_expected_result(
      ".*Tru64.*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                      // test type(s)
      ".*Tricky large.*",              // test data group
      ".*", 3000, 1000);         // test function
   //
   // Solaris specific rates:
   //
   // Error rate for double precision are limited by the accuracy of
   // the approximations use, which bracket rather than preserve the root.
   //
   add_expected_result(
      ".*",                              // compiler
      ".*",                              // stdlib
      "Sun Solaris",                     // platform
      largest_type,                      // test type(s)
      "Bessel J: Random Data.*Tricky.*", // test data group
      ".*", 3000, 500);                  // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Sun Solaris",                 // platform
      "double",                      // test type(s)
      ".*Tricky.*",                  // test data group
      ".*", 200000, 100000);         // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Sun Solaris",                 // platform
      largest_type,                  // test type(s)
      ".*J.*tricky.*",               // test data group
      ".*", 400000000, 200000000);    // test function
   //
   // Mac OS X:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                          // platform
      largest_type,                  // test type(s)
      ".*J0.*Tricky.*",              // test data group
      ".*", 400000000, 400000000);   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                          // platform
      largest_type,                  // test type(s)
      ".*J1.*Tricky.*",              // test data group
      ".*", 3000000, 2000000);       // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                          // platform
      largest_type,                      // test type(s)
      "Bessel JN.*",              // test data group
      ".*", 40000, 20000);         // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "Mac OS",                          // platform
      largest_type,                      // test type(s)
      "Bessel J:.*",              // test data group
      ".*", 50000, 20000);         // test function



   //
   // Linux specific results:
   //
   // sin and cos appear to have only double precision for large
   // arguments on some linux distros:
   //
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      "linux",                          // platform
      largest_type,                      // test type(s)
      ".*J:.*",              // test data group
      ".*", 40000, 30000);         // test function


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
         ".*J0.*Tricky.*",              // test data group
         ".*", 400000, 400000);         // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "double",                      // test type(s)
         ".*J1.*Tricky.*",              // test data group
         ".*", 5000, 5000);             // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "double",                      // test type(s)
         ".*(JN|j).*|.*Tricky.*",       // test data group
         ".*", 50, 50);                 // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         "double",                      // test type(s)
         ".*",                          // test data group
         ".*", 30, 30);                 // test function
      //
      // and we have a few cases with higher limits as well:
      //
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         largest_type,                  // test type(s)
         ".*J0.*Tricky.*",              // test data group
         ".*", 400000000, 400000000);   // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         largest_type,                  // test type(s)
         ".*J1.*Tricky.*",              // test data group
         ".*", 5000000, 5000000);       // test function
      add_expected_result(
         ".*",                          // compiler
         ".*",                          // stdlib
         ".*",                          // platform
         largest_type,                  // test type(s)
         ".*(JN|j).*|.*Tricky.*",       // test data group
         ".*", 33000, 20000);               // test function
   }
#endif
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*J0.*Tricky.*",              // test data group
      ".*", 400000000, 400000000);   // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*J1.*Tricky.*",              // test data group
      ".*", 5000000, 5000000);       // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*JN.*Integer.*",              // test data group
      ".*", 30000, 10000);       // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*(JN|j).*|.*Tricky.*",       // test data group
      ".*", 1500, 700);               // test function
   add_expected_result(
      ".*",                          // compiler
      ".*",                          // stdlib
      ".*",                          // platform
      largest_type,                  // test type(s)
      ".*",                          // test data group
      ".*", 40, 20);                 // test function
   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class T>
void do_test_cyl_bessel_j(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::cyl_bessel_j<value_type, value_type>;
#else
   pg funcp = boost::math::cyl_bessel_j;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test cyl_bessel_j against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::cyl_bessel_j", test_name);
   std::cout << std::endl;

#ifdef TEST_OTHER
   if(boost::is_floating_point<value_type>::value)
   {
      funcp = other::cyl_bessel_j;

      //
      // test other::cyl_bessel_j against data:
      //
      result = boost::math::tools::test(
         data, 
         bind_func(funcp, 0, 1), 
         extract_result(2));
      handle_test_result(result, data[result.worst()], result.worst(), type_name, "other::cyl_bessel_j", test_name);
      std::cout << std::endl;
   }
#endif
}

template <class T>
T cyl_bessel_j_int_wrapper(T v, T x)
{
   return static_cast<T>(boost::math::cyl_bessel_j(boost::math::itrunc(v), x));
}


template <class T>
void do_test_cyl_bessel_j_int(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(value_type, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = cyl_bessel_j_int_wrapper<value_type>;
#else
   pg funcp = cyl_bessel_j_int_wrapper;
#endif

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test cyl_bessel_j against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::cyl_bessel_j", test_name);
   std::cout << std::endl;
}

template <class T>
void do_test_sph_bessel_j(const T& data, const char* type_name, const char* test_name)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   typedef value_type (*pg)(unsigned, value_type);
#if defined(BOOST_MATH_NO_DEDUCED_FUNCTION_POINTERS)
   pg funcp = boost::math::sph_bessel<value_type>;
#else
   pg funcp = boost::math::sph_bessel;
#endif

   typedef int (*cast_t)(value_type);

   boost::math::tools::test_result<value_type> result;

   std::cout << "Testing " << test_name << " with type " << type_name
      << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

   //
   // test sph_bessel against data:
   //
   result = boost::math::tools::test(
      data, 
      bind_func_int1(funcp, 0, 1), 
      extract_result(2));
   handle_test_result(result, data[result.worst()], result.worst(), type_name, "boost::math::sph_bessel", test_name);
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
    static const boost::array<boost::array<T, 3>, 8> j0_data = {{
        { SC_(0), SC_(0), SC_(1) },
        { SC_(0), SC_(1), SC_(0.7651976865579665514497175261026632209093) },
        { SC_(0), SC_(-2), SC_(0.2238907791412356680518274546499486258252) },
        { SC_(0), SC_(4), SC_(-0.3971498098638473722865907684516980419756) },
        { SC_(0), SC_(-8), SC_(0.1716508071375539060908694078519720010684) },
        { SC_(0), SC_(1e-05), SC_(0.999999999975000000000156249999999565972) },
        { SC_(0), SC_(1e-10), SC_(0.999999999999999999997500000000000000000) },
        { SC_(0), SC_(-1e+01), SC_(-0.2459357644513483351977608624853287538296) },
    }};
    static const boost::array<boost::array<T, 3>, 6> j0_tricky = {{
        // Big numbers make the accuracy of std::sin the limiting factor:
        { SC_(0), SC_(1e+03), SC_(0.02478668615242017456133073111569370878617) },
        { SC_(0), SC_(1e+05), SC_(-0.001719201116235972192570601477073201747532) },
        // test at the roots:
        { SC_(0), SC_(2521642)/(1024 * 1024), SC_(1.80208819970046790002973759410972422387259992955354630042138e-7) },
        { SC_(0), SC_(5788221)/(1024 * 1024), SC_(-1.37774249380686777043369399806210229535671843632174587432454e-7) },
        { SC_(0), SC_(9074091)/(1024 * 1024), SC_(1.03553057441100845081018471279571355857520645127532785991335e-7) },
        { SC_(0), SC_(12364320)/(1024 * 1024), SC_(-3.53017140778223781420794006033810387155048392363051866610931e-9) }
    }};    

    static const boost::array<boost::array<T, 3>, 8> j1_data = {
        SC_(1), SC_(0), SC_(0),
        SC_(1), SC_(1), SC_(0.4400505857449335159596822037189149131274),
        SC_(1), SC_(-2), SC_(-0.5767248077568733872024482422691370869203),
        SC_(1), SC_(4), SC_(-6.604332802354913614318542080327502872742e-02),
        SC_(1), SC_(-8), SC_(-0.2346363468539146243812766515904546115488),
        SC_(1), SC_(1e-05), SC_(4.999999999937500000000260416666666124132e-06),
        SC_(1), SC_(1e-10), SC_(4.999999999999999999993750000000000000000e-11),
        SC_(1), SC_(-1e+01), SC_(-4.347274616886143666974876802585928830627e-02),
    };
    static const boost::array<boost::array<T, 3>, 5> j1_tricky = {
        // Big numbers make the accuracy of std::sin the limiting factor:
        SC_(1), SC_(1e+03), SC_(4.728311907089523917576071901216916285418e-03),
        SC_(1), SC_(1e+05), SC_(1.846757562882567716362123967114215743694e-03),
        // test zeros:
        SC_(1), SC_(4017834)/(1024*1024), SC_(3.53149033321258645807835062770856949751958513973522222203044e-7),
        SC_(1), SC_(7356375)/(1024*1024), SC_(-2.31227973111067286051984021150135526024117175836722748404342e-7),
        SC_(1), SC_(10667654)/(1024*1024), SC_(1.24591331097191900488116495350277530373473085499043086981229e-7),
    };

    static const boost::array<boost::array<T, 3>, 14> jn_data = {
        SC_(2), SC_(0), SC_(0),
        SC_(2), SC_(1e-02), SC_(1.249989583365885362413250958437642113452e-05),
        SC_(5), SC_(10), SC_(-0.2340615281867936404436949416457777864635),
        SC_(5), SC_(-10), SC_(0.2340615281867936404436949416457777864635),
        SC_(-5), SC_(1e+06), SC_(7.259643842453285052375779970433848914846e-04),
        SC_(5), SC_(1e+06), SC_(-0.000725964384245328505237577997043384891484649290328285235308619),
        SC_(-5), SC_(-1), SC_(2.497577302112344313750655409880451981584e-04),
        SC_(10), SC_(10), SC_(0.2074861066333588576972787235187534280327),
        SC_(10), SC_(-10), SC_(0.2074861066333588576972787235187534280327),
        SC_(10), SC_(-5), SC_(1.467802647310474131107532232606627020895e-03),
        SC_(-10), SC_(1e+06), SC_(-3.310793117604488741264958559035744460210e-04),
        SC_(10), SC_(1e+06), SC_(-0.000331079311760448874126495855903574446020957243277028930713243),
        SC_(1e+02), SC_(8e+01), SC_(4.606553064823477354141298259169874909670e-06),
        SC_(1e+03), SC_(1e+05), SC_(1.283178112502480365195139312635384057363e-03),
    };
    do_test_cyl_bessel_j(j0_data, name, "Bessel J0: Mathworld Data");
    do_test_cyl_bessel_j(j0_tricky, name, "Bessel J0: Mathworld Data (Tricky cases)");
    do_test_cyl_bessel_j(j1_data, name, "Bessel J1: Mathworld Data");
    do_test_cyl_bessel_j(j1_tricky, name, "Bessel J1: Mathworld Data (tricky cases)");
    do_test_cyl_bessel_j(jn_data, name, "Bessel JN: Mathworld Data");

    do_test_cyl_bessel_j_int(j0_data, name, "Bessel J0: Mathworld Data (Integer Version)");
    do_test_cyl_bessel_j_int(j0_tricky, name, "Bessel J0: Mathworld Data (Tricky cases) (Integer Version)");
    do_test_cyl_bessel_j_int(j1_data, name, "Bessel J1: Mathworld Data (Integer Version)");
    do_test_cyl_bessel_j_int(j1_tricky, name, "Bessel J1: Mathworld Data (tricky cases) (Integer Version)");
    do_test_cyl_bessel_j_int(jn_data, name, "Bessel JN: Mathworld Data (Integer Version)");

    static const boost::array<boost::array<T, 3>, 17> jv_data = {
        //SC_(-2.4), SC_(0), std::numeric_limits<T>::infinity(),
        SC_(2457)/1024, SC_(1)/1024, SC_(3.80739920118603335646474073457326714709615200130620574875292e-9),
        SC_(5.5), SC_(3217)/1024, SC_(0.0281933076257506091621579544064767140470089107926550720453038),
        SC_(-5.5), SC_(3217)/1024, SC_(-2.55820064470647911823175836997490971806135336759164272675969),
        SC_(-5.5), SC_(1e+04), SC_(2.449843111985605522111159013846599118397e-03),
        SC_(5.5), SC_(1e+04), SC_(0.00759343502722670361395585198154817047185480147294665270646578),
        SC_(5.5), SC_(1e+06), SC_(-0.000747424248595630177396350688505919533097973148718960064663632),
        SC_(5.125), SC_(1e+06), SC_(-0.000776600124835704280633640911329691642748783663198207360238214),
        SC_(5.875), SC_(1e+06), SC_(-0.000466322721115193071631008581529503095819705088484386434589780),
        SC_(0.5), SC_(101), SC_(0.0358874487875643822020496677692429287863419555699447066226409),
        SC_(-5.5), SC_(1e+04), SC_(0.00244984311198560552211115901384659911839737686676766460822577),
        SC_(-5.5), SC_(1e+06), SC_(0.000279243200433579511095229508894156656558211060453622750659554),
        SC_(-0.5), SC_(101), SC_(0.0708184798097594268482290389188138201440114881159344944791454),
        SC_(-10486074) / (1024*1024), SC_(1)/1024, SC_(1.41474013160494695750009004222225969090304185981836460288562e35),
        SC_(-10486074) / (1024*1024), SC_(15), SC_(-0.0902239288885423309568944543848111461724911781719692852541489),
        SC_(10486074) / (1024*1024), SC_(1e+02), SC_(-0.0547064914615137807616774867984047583596945624129838091326863),
        SC_(10486074) / (1024*1024), SC_(2e+04), SC_(-0.00556783614400875611650958980796060611309029233226596737701688),
        SC_(-10486074) / (1024*1024), SC_(1e+02), SC_(-0.0547613660316806551338637153942604550779513947674222863858713),
    };
    do_test_cyl_bessel_j(jv_data, name, "Bessel J: Mathworld Data");

    #undef SC_

#include "bessel_j_int_data.ipp"
    do_test_cyl_bessel_j(bessel_j_int_data, name, "Bessel JN: Random Data");

#include "bessel_j_data.ipp"
    do_test_cyl_bessel_j(bessel_j_data, name, "Bessel J: Random Data");

#include "bessel_j_large_data.ipp"
    do_test_cyl_bessel_j(bessel_j_large_data, name, "Bessel J: Random Data (Tricky large values)");

#include "sph_bessel_data.ipp"
    do_test_sph_bessel_j(sph_bessel_data, name, "Bessel j: Random Data");
}

int test_main(int, char* [])
{
#ifdef TEST_GSL
   gsl_set_error_handler_off();
#endif
   expected_results();
   BOOST_MATH_CONTROL_FP;

   test_bessel(0.1F, "float");
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




