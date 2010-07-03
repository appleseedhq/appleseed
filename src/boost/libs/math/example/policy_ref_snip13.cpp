//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

#ifdef _MSC_VER
#pragma warning (disable : 4189) //  'd' : local variable is initialized but not referenced
#endif

//[policy_ref_snip13

#include <boost/math/distributions/cauchy.hpp>

namespace myspace{

using namespace boost::math::policies;

// Define a policy to use, in this case we want all the distribution
// accessor functions to compile, even if they are mathematically
// undefined:
typedef policy<assert_undefined<false> > my_policy;

BOOST_MATH_DECLARE_DISTRIBUTIONS(double, my_policy)

}

// Now we can use myspace::cauchy etc, which will use policy
// myspace::mypolicy:
//
// This compiles but raises a domain error at runtime:
//
void test_cauchy()
{
   try
   {
      double d = mean(myspace::cauchy());
   }
   catch(const std::domain_error& e)
   {
      std::cout << e.what() << std::endl;
   }
}

//]

#include <iostream>

int main()
{
   test_cauchy();
}
