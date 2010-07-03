//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[policy_ref_snip11

#include <boost/math/distributions/normal.hpp>

using namespace boost::math;
using namespace boost::math::policies;

double q = quantile(
      normal_distribution<double, policy<digits2<25> > >(), 
      0.05);

//]

#include <iostream>

int main()
{
   std::cout << q << std::endl;
}
