//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

#include <iostream>

//[policy_eg_6

/*`
Suppose we want a set of distributions to behave as follows:

* Return infinity on overflow, rather than throwing an exception.
* Don't perform any promotion from double to long double internally.
* Return the closest integer result from the quantiles of discrete
distributions.

We'll begin by including the needed header:
*/

#include <boost/math/distributions.hpp>

/*`

Open up an appropriate namespace for our distributions, and
define the policy type we want.  Any policies we don't
specify here will inherit the defaults:

*/

namespace my_distributions{

using namespace boost::math::policies;

typedef policy<
   // return infinity and set errno rather than throw:
   overflow_error<errno_on_error>,
   // Don't promote double -> long double internally:
   promote_double<false>,
   // Return the closest integer result for discrete quantiles:
   discrete_quantile<integer_round_nearest>
> my_policy;

/*`

All we need do now is invoke the BOOST_MATH_DECLARE_DISTRIBUTIONS
macro passing the floating point and policy types as arguments:

*/

BOOST_MATH_DECLARE_DISTRIBUTIONS(double, my_policy)

} // close namespace my_namespace

/*`

We now have a set of typedefs defined in namespace my_namespace
that all look something like this:

``
typedef boost::math::normal_distribution<double, my_policy> normal;
typedef boost::math::cauchy_distribution<double, my_policy> cauchy;
typedef boost::math::gamma_distribution<double, my_policy> gamma;
// etc
``

So that when we use my_namespace::normal we really end up using
`boost::math::normal_distribution<double, my_policy>`:

*/


int main()
{
   //
   // Start with something we know will overflow:
   //
   my_distributions::normal norm(10, 2);
   errno = 0;
   std::cout << "Result of quantile(norm, 0) is: " 
      << quantile(norm, 0) << std::endl;
   std::cout << "errno = " << errno << std::endl;
   errno = 0;
   std::cout << "Result of quantile(norm, 1) is: " 
      << quantile(norm, 1) << std::endl;
   std::cout << "errno = " << errno << std::endl;
   //
   // Now try a discrete distribution:
   //
   my_distributions::binomial binom(20, 0.25);
   std::cout << "Result of quantile(binom, 0.05) is: " 
      << quantile(binom, 0.05) << std::endl;
   std::cout << "Result of quantile(complement(binom, 0.05)) is: " 
      << quantile(complement(binom, 0.05)) << std::endl;
}

/*`

Which outputs:

[pre
Result of quantile(norm, 0) is: -1.#INF
errno = 34
Result of quantile(norm, 1) is: 1.#INF
errno = 34
Result of quantile(binom, 0.05) is: 1
Result of quantile(complement(binom, 0.05)) is: 8
]

This mechanism is particularly useful when we want to define a 
project-wide policy, and don't want to modify the Boost source
or set - possibly fragile and easy to forget - project wide 
build macros.

*/ //] ends quickbook imported section

