//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[policy_ref_snip7

#include <boost/math/distributions/negative_binomial.hpp>

using namespace boost::math;
using namespace boost::math::policies;

typedef negative_binomial_distribution<
      double, 
      policy<discrete_quantile<integer_round_inwards> > 
   > dist_type;
   
// Lower quantile rounded up:
double x = quantile(dist_type(20, 0.3), 0.05);
// Upper quantile rounded down:
double y = quantile(complement(dist_type(20, 0.3), 0.05));

//]

#include <iostream>

int main()
{
   std::cout << x << " " << y << std::endl;
}
