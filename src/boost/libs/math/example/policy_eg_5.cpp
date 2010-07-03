//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

#include <iostream>

//[policy_eg_5

#include <boost/math/special_functions.hpp>

namespace {

using namespace boost::math::policies;

typedef policy<
   domain_error<errno_on_error>,
   pole_error<errno_on_error>,
   overflow_error<errno_on_error>,
   evaluation_error<errno_on_error> 
> c_policy;

BOOST_MATH_DECLARE_SPECIAL_FUNCTIONS(c_policy)

} // close unnamed namespace

int main()
{
   errno = 0;
   std::cout << "Result of tgamma(30000) is: " 
      << tgamma(30000) << std::endl;
   std::cout << "errno = " << errno << std::endl;
   std::cout << "Result of tgamma(-10) is: " 
      << tgamma(-10) << std::endl;
   std::cout << "errno = " << errno << std::endl;
}

//]
