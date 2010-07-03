//  Copyright John Maddock 2006.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_MATH_ASSERT_UNDEFINED_POLICY false

#include <boost/math/distributions.hpp>
#include <boost/math/concepts/distributions.hpp>


template <class RealType>
void instantiate(RealType)
{
   using namespace boost;
   using namespace boost::math;
   using namespace boost::math::concepts;

   function_requires<DistributionConcept<normal_distribution<RealType> > >();
   function_requires<DistributionConcept<beta_distribution<RealType> > >();
   function_requires<DistributionConcept<binomial_distribution<RealType> > >();
   function_requires<DistributionConcept<cauchy_distribution<RealType> > >();
   function_requires<DistributionConcept<bernoulli_distribution<RealType> > >();
   function_requires<DistributionConcept<chi_squared_distribution<RealType> > >();
   function_requires<DistributionConcept<exponential_distribution<RealType> > >();
   function_requires<DistributionConcept<extreme_value_distribution<RealType> > >();
   function_requires<DistributionConcept<fisher_f_distribution<RealType> > >();
   function_requires<DistributionConcept<gamma_distribution<RealType> > >();
   function_requires<DistributionConcept<students_t_distribution<RealType> > >();
   function_requires<DistributionConcept<pareto_distribution<RealType> > >();
   function_requires<DistributionConcept<poisson_distribution<RealType> > >();
   function_requires<DistributionConcept<rayleigh_distribution<RealType> > >();
   function_requires<DistributionConcept<weibull_distribution<RealType> > >();
   function_requires<DistributionConcept<lognormal_distribution<RealType> > >();
   function_requires<DistributionConcept<triangular_distribution<RealType> > >();
   function_requires<DistributionConcept<uniform_distribution<RealType> > >();
   function_requires<DistributionConcept<negative_binomial_distribution<RealType> > >();
   function_requires<DistributionConcept<non_central_chi_squared_distribution<RealType> > >();
}


int main()
{
   instantiate(float(0));
   instantiate(double(0));
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   instantiate((long double)(0));
#endif
}

