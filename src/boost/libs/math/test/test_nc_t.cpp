// test_nc_t.cpp

// Copyright John Maddock 2008.

// Use, modification and distribution are subject to the
// Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifdef _MSC_VER
#pragma warning (disable:4127 4512)
#endif

#if !defined(TEST_FLOAT) && !defined(TEST_DOUBLE) && !defined(TEST_LDOUBLE) && !defined(TEST_REAL_CONCEPT)
#  define TEST_FLOAT
#  define TEST_DOUBLE
#  define TEST_LDOUBLE
#  define TEST_REAL_CONCEPT
#endif

#include <boost/math/concepts/real_concept.hpp> // for real_concept
#include <boost/math/distributions/non_central_t.hpp> // for chi_squared_distribution
#include <boost/test/included/test_exec_monitor.hpp> // for test_main
#include <boost/test/floating_point_comparison.hpp> // for BOOST_CHECK_CLOSE

#include "functor.hpp"
#include "handle_test_result.hpp"

#include <iostream>
using std::cout;
using std::endl;
#include <limits>
using std::numeric_limits;

#define BOOST_CHECK_CLOSE_EX(a, b, prec, i) \
   {\
      unsigned int failures = boost::unit_test::results_collector.results( boost::unit_test::framework::current_test_case().p_id ).p_assertions_failed;\
      BOOST_CHECK_CLOSE(a, b, prec); \
      if(failures != boost::unit_test::results_collector.results( boost::unit_test::framework::current_test_case().p_id ).p_assertions_failed)\
      {\
         std::cerr << "Failure was at row " << i << std::endl;\
         std::cerr << std::setprecision(35); \
         std::cerr << "{ " << data[i][0] << " , " << data[i][1] << " , " << data[i][2];\
         std::cerr << " , " << data[i][3] << " , " << data[i][4] << " } " << std::endl;\
      }\
   }

#define BOOST_CHECK_EX(a, i) \
   {\
      unsigned int failures = boost::unit_test::results_collector.results( boost::unit_test::framework::current_test_case().p_id ).p_assertions_failed;\
      BOOST_CHECK(a); \
      if(failures != boost::unit_test::results_collector.results( boost::unit_test::framework::current_test_case().p_id ).p_assertions_failed)\
      {\
         std::cerr << "Failure was at row " << i << std::endl;\
         std::cerr << std::setprecision(35); \
         std::cerr << "{ " << data[i][0] << " , " << data[i][1] << " , " << data[i][2];\
         std::cerr << " , " << data[i][3] << " , " << data[i][4] << " } " << std::endl;\
      }\
   }

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
   largest_type = "(long\\s+)?double|real_concept";
#endif

   //
   // Catch all cases come last:
   //
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      "real_concept",                   // test type(s)
      "[^|]*",                          // test data group
      "[^|]*", 300000, 100000);                // test function
   add_expected_result(
      "[^|]*",                          // compiler
      "[^|]*",                          // stdlib
      "[^|]*",                          // platform
      largest_type,                     // test type(s)
      "[^|]*",                          // test data group
      "[^|]*", 250, 50);                // test function

   //
   // Finish off by printing out the compiler/stdlib/platform names,
   // we do this to make it easier to mark up expected error rates.
   //
   std::cout << "Tests run with " << BOOST_COMPILER << ", " 
      << BOOST_STDLIB << ", " << BOOST_PLATFORM << std::endl;
}

template <class RealType>
RealType naive_pdf(RealType v, RealType delta, RealType x)
{
}

template <class RealType>
RealType naive_mean(RealType v, RealType delta)
{
   using boost::math::tgamma;
   return delta * sqrt(v / 2) * tgamma((v-1)/2) / tgamma(v/2);
}

float naive_mean(float v, float delta)
{
   return (float)naive_mean((double)v, (double)delta);
}

template <class RealType>
RealType naive_variance(RealType v, RealType delta)
{
   using boost::math::tgamma;
   RealType r = tgamma((v-1)/2) / tgamma(v/2);
   r *= r;
   r *= -delta * delta * v / 2;
   r += (1 + delta * delta) * v / (v - 2);
   return r;
}

float naive_variance(float v, float delta)
{
   return (float)naive_variance((double)v, (double)delta);
}

template <class RealType>
RealType naive_skewness(RealType v, RealType delta)
{
   using boost::math::tgamma;
   RealType tgr = tgamma((v-1)/2) / tgamma(v / 2);
   RealType r = delta * sqrt(v) * tgamma((v-1)/2)
      * (v * (-3 + delta * delta + 2 * v) / ((-3 + v) * (-2 + v)) 
         - 2 * ((1 + delta * delta) * v / (-2 + v) - delta * delta * v * tgr * tgr / 2));
   r /= boost::math::constants::root_two<RealType>()
      * pow(((1+delta*delta) * v / (-2+v) - delta*delta*v*tgr*tgr/2), RealType(1.5f))
      * tgamma(v/2);
   return r;
}

float naive_skewness(float v, float delta)
{
   return (float)naive_skewness((double)v, (double)delta);
}

template <class RealType>
RealType naive_kurtosis_excess(RealType v, RealType delta)
{
   using boost::math::tgamma;
   RealType tgr = tgamma((v-1)/2) / tgamma(v / 2);
   RealType r = -delta * delta * v * tgr * tgr / 2;
   r *= v * (delta * delta * (1 + v) + 3 * (-5 + 3 * v)) / ((-3 + v)*(-2+v))
      - 3 * ((1 + delta * delta) * v / (-2 + v) - delta * delta * v * tgr * tgr / 2);
   r += (3 + 6 * delta * delta + delta * delta * delta * delta)* v * v 
      / ((-4+v) * (-2+v));
   r /= (1+delta*delta)*v / (-2+v) - delta*delta*v *tgr*tgr/2;
   r /= (1+delta*delta)*v / (-2+v) - delta*delta*v *tgr*tgr/2;
   return r;
}

float naive_kurtosis_excess(float v, float delta)
{
   return (float)naive_kurtosis_excess((double)v, (double)delta);
}

template <class RealType>
void test_spot(
     RealType df,    // Degrees of freedom
     RealType ncp,   // non-centrality param
     RealType t,     // T statistic
     RealType P,     // CDF
     RealType Q,     // Complement of CDF
     RealType tol)   // Test tolerance
{
   boost::math::non_central_t_distribution<RealType> dist(df, ncp);
   BOOST_CHECK_CLOSE(
      cdf(dist, t), P, tol);
   try{
      BOOST_CHECK_CLOSE(
         mean(dist), naive_mean(df, ncp), tol);
      BOOST_CHECK_CLOSE(
         variance(dist), naive_variance(df, ncp), tol);
      BOOST_CHECK_CLOSE(
         skewness(dist), naive_skewness(df, ncp), tol * 10);
      BOOST_CHECK_CLOSE(
         kurtosis_excess(dist), naive_kurtosis_excess(df, ncp), tol * 50);
      BOOST_CHECK_CLOSE(
         kurtosis(dist), 3 + naive_kurtosis_excess(df, ncp), tol * 50);
   }
   catch(const std::domain_error&)
   {
   }
   /*
   BOOST_CHECK_CLOSE(
      pdf(dist, t), naive_pdf(dist.degrees_of_freedom(), ncp, t), tol * 50);
   */
   if((P < 0.99) && (Q < 0.99))
   {
      //
      // We can only check this if P is not too close to 1,
      // so that we can guarentee Q is reasonably free of error:
      //
      BOOST_CHECK_CLOSE(
         cdf(complement(dist, t)), Q, tol);
      BOOST_CHECK_CLOSE(
            quantile(dist, P), t, tol * 10);
      BOOST_CHECK_CLOSE(
            quantile(complement(dist, Q)), t, tol * 10);
      /*
      BOOST_CHECK_CLOSE(
         dist.find_degrees_of_freedom(ncp, t, P), df, tol * 10);
      BOOST_CHECK_CLOSE(
         dist.find_degrees_of_freedom(boost::math::complement(ncp, t, Q)), df, tol * 10);
      BOOST_CHECK_CLOSE(
         dist.find_non_centrality(df, t, P), ncp, tol * 10);
      BOOST_CHECK_CLOSE(
         dist.find_non_centrality(boost::math::complement(df, t, Q)), ncp, tol * 10);
         */
   }
}

template <class RealType> // Any floating-point type RealType.
void test_spots(RealType)
{
   //
   // Approx limit of test data is 12 digits expressed here as a persentage:
   //
   RealType tolerance = (std::max)(
      boost::math::tools::epsilon<RealType>(),
      (RealType)5e-12f) * 100;
   //
   // At float precision we need to up the tolerance, since 
   // the input values are rounded off to inexact quantities
   // the results get thrown off by a noticeable amount.
   //
   if(boost::math::tools::digits<RealType>() < 50)
      tolerance *= 50;
   if(boost::is_floating_point<RealType>::value != 1)
      tolerance *= 20; // real_concept special functions are less accurate

   cout << "Tolerance = " << tolerance << "%." << endl;

   //
   // Test data is taken from:
   //
   // Computing discrete mixtures of continuous
   // distributions: noncentral chisquare, noncentral t
   // and the distribution of the square of the sample
   // multiple correlation coeficient.
   // Denise Benton, K. Krishnamoorthy.
   // Computational Statistics & Data Analysis 43 (2003) 249 - 267
   //
   test_spot(
      static_cast<RealType>(3),   // degrees of freedom
      static_cast<RealType>(1),   // non centrality
      static_cast<RealType>(2.34),   // T
      static_cast<RealType>(0.801888999613917),       // Probability of result (CDF), P
      static_cast<RealType>(1-0.801888999613917),           // Q = 1 - P
      tolerance);
   test_spot(
      static_cast<RealType>(126),   // degrees of freedom
      static_cast<RealType>(-2),   // non centrality
      static_cast<RealType>(-4.33),   // T
      static_cast<RealType>(1.252846196792878e-2),       // Probability of result (CDF), P
      static_cast<RealType>(1-1.252846196792878e-2),           // Q = 1 - P
      tolerance);
   test_spot(
      static_cast<RealType>(20),   // degrees of freedom
      static_cast<RealType>(23),   // non centrality
      static_cast<RealType>(23),   // T
      static_cast<RealType>(0.460134400391924),       // Probability of result (CDF), P
      static_cast<RealType>(1-0.460134400391924),           // Q = 1 - P
      tolerance);
   test_spot(
      static_cast<RealType>(20),   // degrees of freedom
      static_cast<RealType>(33),   // non centrality
      static_cast<RealType>(34),   // T
      static_cast<RealType>(0.532008386378725),       // Probability of result (CDF), P
      static_cast<RealType>(1-0.532008386378725),           // Q = 1 - P
      tolerance);
   test_spot(
      static_cast<RealType>(12),   // degrees of freedom
      static_cast<RealType>(38),   // non centrality
      static_cast<RealType>(39),   // T
      static_cast<RealType>(0.495868184917805),       // Probability of result (CDF), P
      static_cast<RealType>(1-0.495868184917805),           // Q = 1 - P
      tolerance);
   test_spot(
      static_cast<RealType>(12),   // degrees of freedom
      static_cast<RealType>(39),   // non centrality
      static_cast<RealType>(39),   // T
      static_cast<RealType>(0.446304024668836),       // Probability of result (CDF), P
      static_cast<RealType>(1-0.446304024668836),           // Q = 1 - P
      tolerance);
   test_spot(
      static_cast<RealType>(200),   // degrees of freedom
      static_cast<RealType>(38),   // non centrality
      static_cast<RealType>(39),   // T
      static_cast<RealType>(0.666194209961795),       // Probability of result (CDF), P
      static_cast<RealType>(1-0.666194209961795),           // Q = 1 - P
      tolerance);
   test_spot(
      static_cast<RealType>(200),   // degrees of freedom
      static_cast<RealType>(42),   // non centrality
      static_cast<RealType>(40),   // T
      static_cast<RealType>(0.179292265426085),       // Probability of result (CDF), P
      static_cast<RealType>(1-0.179292265426085),           // Q = 1 - P
      tolerance);

   boost::math::non_central_t_distribution<RealType> dist(static_cast<RealType>(8), static_cast<RealType>(12));
   BOOST_CHECK_CLOSE(pdf(dist, 12), static_cast<RealType>(1.235329715425894935157684607751972713457e-1L), tolerance);
   BOOST_CHECK_CLOSE(pdf(boost::math::non_central_t_distribution<RealType>(126, -2), -4), static_cast<RealType>(5.797932289365814702402873546466798025787e-2L), tolerance);
   BOOST_CHECK_CLOSE(pdf(boost::math::non_central_t_distribution<RealType>(126, 2), 4), static_cast<RealType>(5.797932289365814702402873546466798025787e-2L), tolerance);
   BOOST_CHECK_CLOSE(pdf(boost::math::non_central_t_distribution<RealType>(126, 2), 0), static_cast<RealType>(5.388394890639957139696546086044839573749e-2L), tolerance);
} // template <class RealType>void test_spots(RealType)

template <class T>
T nct_cdf(T df, T nc, T x)
{
   return cdf(boost::math::non_central_t_distribution<T>(df, nc), x);
}

template <class T>
T nct_ccdf(T df, T nc, T x)
{
   return cdf(complement(boost::math::non_central_t_distribution<T>(df, nc), x));
}

template <typename T>
void do_test_nc_t(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   std::cout << "Testing: " << test << std::endl;

   value_type (*fp1)(value_type, value_type, value_type) = nct_cdf;
   boost::math::tools::test_result<value_type> result;

   result = boost::math::tools::test(
      data,
      bind_func(fp1, 0, 1, 2),
      extract_result(3));
   handle_test_result(result, data[result.worst()], result.worst(),
      type_name, "CDF", test);

   fp1 = nct_ccdf;
   result = boost::math::tools::test(
      data,
      bind_func(fp1, 0, 1, 2),
      extract_result(4));
   handle_test_result(result, data[result.worst()], result.worst(),
      type_name, "CCDF", test);

   std::cout << std::endl;

}

template <typename T>
void quantile_sanity_check(T& data, const char* type_name, const char* test)
{
   typedef typename T::value_type row_type;
   typedef typename row_type::value_type value_type;

   //
   // Tests with type real_concept take rather too long to run, so
   // for now we'll disable them:
   //
   if(!boost::is_floating_point<value_type>::value)
      return;

   std::cout << "Testing: " << type_name << " quantile sanity check, with tests " << test << std::endl;

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
      if(data[i][3] == 0)
      {
         BOOST_CHECK(0 == quantile(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]), data[i][3]));
      }
      else if(data[i][3] < 0.9999f)
      {
         value_type p = quantile(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]), data[i][3]);
         value_type pt = data[i][2];
         BOOST_CHECK_CLOSE_EX(pt, p, precision, i);
      }
      if(data[i][4] == 0)
      {
         BOOST_CHECK(0 == quantile(complement(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]), data[i][3])));
      }
      else if(data[i][4] < 0.9999f)
      {
         value_type p = quantile(complement(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]), data[i][4]));
         value_type pt = data[i][2];
         BOOST_CHECK_CLOSE_EX(pt, p, precision, i);
      }
      if(boost::math::tools::digits<value_type>() > 50)
      {
         //
         // Sanity check mode, the accuracy of
         // the mode is at *best* the square root of the accuracy of the PDF:
         //
         try{
            value_type m = mode(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]));
            value_type p = pdf(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]), m);
            BOOST_CHECK_EX(pdf(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]), m * (1 + sqrt(precision) * 100)) <= p, i);
            BOOST_CHECK_EX(pdf(boost::math::non_central_t_distribution<value_type>(data[i][0], data[i][1]), m * (1 - sqrt(precision)) * 100) <= p, i);
         }
         catch(const boost::math::evaluation_error& ) {}
#if 0
         //
         // Sanity check degrees-of-freedom finder, don't bother at float
         // precision though as there's not enough data in the probability
         // values to get back to the correct degrees of freedom or 
         // non-cenrality parameter:
         //
         try{
            if((data[i][3] < 0.99) && (data[i][3] != 0))
            {
               BOOST_CHECK_CLOSE_EX(
                  boost::math::non_central_t_distribution<value_type>::find_degrees_of_freedom(data[i][1], data[i][2], data[i][3]),
                  data[i][0], precision, i);
               BOOST_CHECK_CLOSE_EX(
                  boost::math::non_central_t_distribution<value_type>::find_non_centrality(data[i][0], data[i][2], data[i][3]),
                  data[i][1], precision, i);
            }
            if((data[i][4] < 0.99) && (data[i][4] != 0))
            {
               BOOST_CHECK_CLOSE_EX(
                  boost::math::non_central_t_distribution<value_type>::find_degrees_of_freedom(boost::math::complement(data[i][1], data[i][2], data[i][4])),
                  data[i][0], precision, i);
               BOOST_CHECK_CLOSE_EX(
                  boost::math::non_central_t_distribution<value_type>::find_non_centrality(boost::math::complement(data[i][0], data[i][2], data[i][4])),
                  data[i][1], precision, i);
            }
         }
         catch(const std::exception& e)
         {
            BOOST_ERROR(e.what());
         }
#endif
      }
   }
}

template <typename T>
void test_accuracy(T, const char* type_name)
{
#include "nct.ipp"
    do_test_nc_t(nct, type_name, "Non Central T");
    quantile_sanity_check(nct, type_name, "Non Central T");
}

int test_main(int, char* [])
{
   BOOST_MATH_CONTROL_FP;
   // Basic sanity-check spot values.
   expected_results();

   // (Parameter value, arbitrarily zero, only communicates the floating point type).
#ifdef TEST_FLOAT
   test_spots(0.0F); // Test float.
#endif
#ifdef TEST_DOUBLE
   test_spots(0.0); // Test double.
#endif
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#ifdef TEST_LDOUBLE
   test_spots(0.0L); // Test long double.
#endif
#if !BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x582))
#ifdef TEST_REAL_CONCEPT
   test_spots(boost::math::concepts::real_concept(0.)); // Test real concept.
#endif
#endif
#endif

#ifdef TEST_FLOAT
   test_accuracy(0.0F, "float"); // Test float.
#endif
#ifdef TEST_DOUBLE
   test_accuracy(0.0, "double"); // Test double.
#endif
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#ifdef TEST_LDOUBLE
   test_accuracy(0.0L, "long double"); // Test long double.
#endif
#if !BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x582))
#ifdef TEST_REAL_CONCEPT
   test_accuracy(boost::math::concepts::real_concept(0.), "real_concept"); // Test real concept.
#endif
#endif
#endif
   return 0;
} // int test_main(int, char* [])

