//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include "required_defines.hpp"

#include "performance_measure.hpp"

#include <boost/math/distributions.hpp>

double probabilities[] = {
   1e-5,
   1e-4,
   1e-3,
   1e-2,
   0.05,
   0.1,
   0.2,
   0.3,
   0.4,
   0.5,
   0.6,
   0.7,
   0.8,
   0.9,
   0.95,
   1-1e-5,
   1-1e-4,
   1-1e-3,
   1-1e-2
};

int int_values[] = {
   1,
   2,
   3,
   5,
   10,
   20,
   50,
   100,
   1000,
   10000,
   100000
};

int small_int_values[] = {
   1,
   2,
   3,
   5,
   10,
   15,
   20,
   30,
   50,
   100,
   150
};

double real_values[] = {
   1e-5,
   1e-4,
   1e-2,
   1e-1,
   1,
   10,
   100,
   1000,
   10000,
   100000
};

#define BOOST_MATH_DISTRIBUTION3_TEST(name, param1_table, param2_table, param3_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" BOOST_STRINGIZE(name) "-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(param3_table)/sizeof(param3_table[0]);\
   unsigned d_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            for(unsigned l = 0; l < d_size; ++l)\
            {\
               result += cdf(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i], param2_table[j], param3_table[k]), random_variable_table[l]);\
            }\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size * d_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_pdf_, name), "dist-" BOOST_STRINGIZE(name) "-pdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(param3_table)/sizeof(param3_table[0]);\
   unsigned d_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            for(unsigned l = 0; l < d_size; ++l)\
            {\
               result += pdf(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i], param2_table[j], param3_table[k]), random_variable_table[l]);\
            }\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size * d_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" BOOST_STRINGIZE(name) "-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
      unsigned c_size = sizeof(param3_table)/sizeof(param3_table[0]);\
      unsigned d_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
         for(unsigned j = 0; j < b_size; ++j)\
         {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               for(unsigned l = 0; l < d_size; ++l)\
               {\
                  result += quantile(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i], param2_table[j], param3_table[k]), probability_table[l]);\
               }\
            }\
         }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * b_size * c_size * d_size);\
   }

#define BOOST_MATH_DISTRIBUTION2_TEST(name, param1_table, param2_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" BOOST_STRINGIZE(name) "-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += cdf(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i], param2_table[j]), random_variable_table[k]);\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_pdf_, name), "dist-" BOOST_STRINGIZE(name) "-pdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += pdf(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i], param2_table[j]), random_variable_table[k]);\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" BOOST_STRINGIZE(name) "-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
      unsigned c_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
         for(unsigned j = 0; j < b_size; ++j)\
         {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               result += quantile(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i], param2_table[j]), probability_table[k]);\
            }\
         }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * b_size * c_size);\
   }

#define BOOST_MATH_DISTRIBUTION1_TEST(name, param1_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" BOOST_STRINGIZE(name) "-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += cdf(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i]), random_variable_table[k]);\
         }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_pdf_, name), "dist-" BOOST_STRINGIZE(name) "-pdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += pdf(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i]), random_variable_table[k]);\
         }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" BOOST_STRINGIZE(name) "-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned c_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               result += quantile(boost::math:: BOOST_JOIN(name, _distribution) <>(param1_table[i]), probability_table[k]);\
            }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * c_size);\
   }

BOOST_MATH_DISTRIBUTION2_TEST(beta, probabilities, probabilities, probabilities, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(binomial, int_values, probabilities, int_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(cauchy, int_values, real_values, int_values, probabilities)
BOOST_MATH_DISTRIBUTION1_TEST(chi_squared, int_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION1_TEST(exponential, real_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(fisher_f, int_values, int_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(gamma, real_values, real_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(lognormal, real_values, real_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(negative_binomial, int_values, probabilities, int_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(normal, real_values, real_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION1_TEST(poisson, real_values, int_values, probabilities)
BOOST_MATH_DISTRIBUTION1_TEST(students_t, int_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(weibull, real_values, real_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(non_central_chi_squared, int_values, int_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION3_TEST(non_central_beta, int_values, int_values, real_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION3_TEST(non_central_f, int_values, int_values, real_values, real_values, probabilities)
BOOST_MATH_DISTRIBUTION2_TEST(non_central_t, int_values, small_int_values, real_values, probabilities)

#ifdef TEST_R

#define MATHLIB_STANDALONE 1

extern "C" {
#include "Rmath.h"
/*
double qnchisq(double, double, double, int, int)
{
   return 0;
}
*/
}

#define BOOST_MATH_R_DISTRIBUTION3_TEST(name, param1_table, param2_table, param3_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" #name "-R-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(param3_table)/sizeof(param3_table[0]);\
   unsigned d_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            for(unsigned l = 0; l < d_size; ++l)\
            {\
               result += p##name (random_variable_table[l], param1_table[i], param2_table[j], param3_table[k], 1, 0);\
            }\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size * d_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_pdf_, name), "dist-" #name "-R-pdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(param3_table)/sizeof(param3_table[0]);\
   unsigned d_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            for(unsigned l = 0; l < d_size; ++l)\
            {\
               result += d##name (random_variable_table[l], param1_table[i], param2_table[j], param3_table[k], 0);\
            }\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size * d_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" #name "-R-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
      unsigned c_size = sizeof(param3_table)/sizeof(param3_table[0]);\
      unsigned d_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
         for(unsigned j = 0; j < b_size; ++j)\
         {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               for(unsigned l = 0; l < d_size; ++l)\
               {\
                  result += q##name (probability_table[l], param1_table[i], param2_table[j], param3_table[k], 1, 0);\
               }\
            }\
         }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * b_size * c_size * d_size);\
   }

#define BOOST_MATH_R_DISTRIBUTION2_TEST(name, param1_table, param2_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" #name "-R-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += p##name (random_variable_table[k], param1_table[i], param2_table[j], 1, 0);\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_pdf_, name), "dist-" #name "-R-pdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += d##name (random_variable_table[k], param1_table[i], param2_table[j], 0);\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" #name "-R-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
      unsigned c_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
         for(unsigned j = 0; j < b_size; ++j)\
         {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               result += q##name (probability_table[k], param1_table[i], param2_table[j], 1, 0);\
            }\
         }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * b_size * c_size);\
   }

#define BOOST_MATH_R_DISTRIBUTION1_TEST(name, param1_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" #name "-R-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += p##name (random_variable_table[k], param1_table[i], 1, 0);\
         }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_pdf_, name), "dist-" #name "-R-pdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += d##name (random_variable_table[k], param1_table[i], 0);\
         }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" #name "-R-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned c_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               result += q##name (probability_table[k], param1_table[i], 1, 0);\
            }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * c_size);\
   }

BOOST_MATH_R_DISTRIBUTION2_TEST(beta, probabilities, probabilities, probabilities, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(binom, int_values, probabilities, int_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(cauchy, int_values, real_values, int_values, probabilities)
BOOST_MATH_R_DISTRIBUTION1_TEST(chisq, int_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION1_TEST(exp, real_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(f, int_values, int_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(gamma, real_values, real_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(lnorm, real_values, real_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(nbinom, int_values, probabilities, int_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(norm, real_values, real_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION1_TEST(pois, real_values, int_values, probabilities)
BOOST_MATH_R_DISTRIBUTION1_TEST(t, int_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(weibull, real_values, real_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(nchisq, int_values, int_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION3_TEST(nf, int_values, int_values, real_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION3_TEST(nbeta, int_values, int_values, real_values, real_values, probabilities)
BOOST_MATH_R_DISTRIBUTION2_TEST(nt, int_values, small_int_values, real_values, probabilities)

#endif

#ifdef TEST_CEPHES

extern "C"{

double bdtr(int k, int n, double p);
double bdtri(int k, int n, double p);

double chdtr(double df, double x);
double chdtri(double df, double p);

double fdtr(int k, int n, double p);
double fdtri(int k, int n, double p);

double nbdtr(int k, int n, double p);
double nbdtri(int k, int n, double p);

}

#define BOOST_MATH_CEPHES_DISTRIBUTION2_TEST(name, param1_table, param2_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" #name "-cephes-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
      for(unsigned j = 0; j < b_size; ++j)\
      {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += name##dtr (param1_table[i], param2_table[j], random_variable_table[k]);\
         }\
      }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * b_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" #name "-cephes-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned b_size = sizeof(param2_table)/sizeof(param2_table[0]);\
      unsigned c_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
         for(unsigned j = 0; j < b_size; ++j)\
         {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               result += name##dtri (param1_table[i], param2_table[j], probability_table[k]);\
            }\
         }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * b_size * c_size);\
   }

#define BOOST_MATH_CEPHES_DISTRIBUTION1_TEST(name, param1_table, random_variable_table, probability_table) \
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist, name), "dist-" #name "-cephes-cdf")\
   {\
   double result = 0;\
   unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
   unsigned c_size = sizeof(random_variable_table)/sizeof(random_variable_table[0]);\
   \
   for(unsigned i = 0; i < a_size; ++i)\
   {\
         for(unsigned k = 0; k < c_size; ++k)\
         {\
            result += name##dtr (param1_table[i], random_variable_table[k]);\
         }\
   }\
   \
   consume_result(result);\
   set_call_count(a_size * c_size);\
   }\
   BOOST_MATH_PERFORMANCE_TEST(BOOST_JOIN(dist_quant, name), "dist-" #name "-cephes-quantile")\
   {\
      double result = 0;\
      unsigned a_size = sizeof(param1_table)/sizeof(param1_table[0]);\
      unsigned c_size = sizeof(probability_table)/sizeof(probability_table[0]);\
      \
      for(unsigned i = 0; i < a_size; ++i)\
      {\
            for(unsigned k = 0; k < c_size; ++k)\
            {\
               result += name##dtri (param1_table[i], probability_table[k]);\
            }\
      }\
      \
      consume_result(result);\
      set_call_count(a_size * c_size);\
   }
// Cephes inverse doesn't actually calculate the quantile!!!
// BOOST_MATH_CEPHES_DISTRIBUTION2_TEST(b, int_values, int_values, probabilities, probabilities)
BOOST_MATH_CEPHES_DISTRIBUTION1_TEST(ch, int_values, real_values, probabilities)
BOOST_MATH_CEPHES_DISTRIBUTION2_TEST(f, int_values, int_values, real_values, probabilities)
// Cephes inverse doesn't calculate the quantile!!!
// BOOST_MATH_CEPHES_DISTRIBUTION2_TEST(nb, int_values, int_values, probabilities, probabilities)

#endif

