//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[policy_ref_snip12

#include <boost/math/special_functions/gamma.hpp>

namespace myspace{

using namespace boost::math::policies;

// Define a policy that does not throw on overflow:
typedef policy<overflow_error<errno_on_error> > my_policy;

// Define the special functions in this scope to use the policy:   
BOOST_MATH_DECLARE_SPECIAL_FUNCTIONS(my_policy)

}

//
// Now we can use myspace::tgamma etc.
// They will automatically use "my_policy":
//
double t = myspace::tgamma(30.0); // will not throw on overflow

//]

#include <iostream>

int main()
{
   std::cout << t << std::endl;
}
