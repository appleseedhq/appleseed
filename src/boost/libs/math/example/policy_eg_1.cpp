//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <iostream>

//[policy_eg_1

#include <boost/math/special_functions/gamma.hpp>
//
// Define the policy to use:
//
using namespace boost::math::policies;
typedef policy<
   domain_error<errno_on_error>,
   pole_error<errno_on_error>,
   overflow_error<errno_on_error>,
   evaluation_error<errno_on_error> 
> c_policy;
//
// Now use the policy when calling tgamma:
//
int main()
{
   errno = 0;
   std::cout << "Result of tgamma(30000) is: " 
      << boost::math::tgamma(30000, c_policy()) << std::endl;
   std::cout << "errno = " << errno << std::endl;
   std::cout << "Result of tgamma(-10) is: " 
      << boost::math::tgamma(-10, c_policy()) << std::endl;
   std::cout << "errno = " << errno << std::endl;
}

//]