//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[policy_ref_snip6

#include <boost/math/distributions/negative_binomial.hpp>

using namespace boost::math;

// Lower quantile rounded down:
double x = quantile(negative_binomial(20, 0.3), 0.05);
// Upper quantile rounded up:
double y = quantile(complement(negative_binomial(20, 0.3), 0.05));

//]

#include <iostream>

int main()
{
   std::cout << x << " " << y << std::endl;
}
