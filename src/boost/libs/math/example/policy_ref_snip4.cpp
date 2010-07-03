//  Copyright John Maddock 2007.
//  Copyright Paul A. Bristow 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

#ifdef _MSC_VER
# pragma warning (disable : 4305) // 'initializing' : truncation from 'long double' to 'const eval_type'
# pragma warning (disable : 4244) // 'conversion' : truncation from 'long double' to 'const eval_type'
#endif

//[policy_ref_snip4

#include <boost/math/distributions/normal.hpp>

using namespace boost::math::policies;
using namespace boost::math;

// Define a policy:
typedef policy<
      promote_float<false>
      > my_policy;

// Define the distribution:
typedef normal_distribution<float, my_policy> my_norm;

// Get a quantile:
float q = quantile(my_norm(), 0.05f);

//] [policy_ref_snip4]

#include <iostream>

int main()
{
   std::cout << q << std::endl;
}
