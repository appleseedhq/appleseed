//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[policy_ref_snip9

#include <boost/math/special_functions/gamma.hpp>

using namespace boost::math;
using namespace boost::math::policies;

typedef policy<digits10<5> > pol;

double t = tgamma(12, pol());

//]

#include <iostream>

int main()
{
   std::cout << t << std::endl;
}
