//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[policy_ref_snip2

#include <boost/math/distributions/normal.hpp>

using namespace boost::math::policies;
using namespace boost::math;

// Define a policy:
typedef policy<
      overflow_error<ignore_error>
      > my_policy;
      
// Define the distribution:
typedef normal_distribution<double, my_policy> my_norm;

// Get a quantile:
double q = quantile(my_norm(), 0.05);

//]

#include <iostream>

int main()
{
   std::cout << q << std::endl;
}
