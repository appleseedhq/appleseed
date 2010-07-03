//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

double some_value = 2;

//[policy_ref_snip3

#include <boost/math/special_functions/gamma.hpp>

using namespace boost::math::policies;
using namespace boost::math;

// Define a policy:
typedef policy<
      promote_double<false> 
      > my_policy;
      
// Call the function:
double t1 = tgamma(some_value, my_policy());

// Alternatively we could use make_policy and define everything at the call site:
double t2 = tgamma(some_value, make_policy(promote_double<false>()));

//]

#include <iostream>

int main()
{
   std::cout << t1 << " " << t2 << std::endl;
}
