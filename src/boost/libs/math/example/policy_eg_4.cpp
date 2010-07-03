//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

#include <iostream>

//[policy_eg_4

/*`
Suppose we want `C::foo()` to behave in a C-compatible way and set
`::errno` on error rather than throwing any exceptions.

We'll begin by including the needed header:
*/

#include <boost/math/special_functions.hpp>

/*`

Open up the "C" namespace that we'll use for our functions, and
define the policy type we want: in this case one that sets
::errno rather than throwing exceptions.  Any policies we don't
specify here will inherit the defaults:

*/

namespace C{

using namespace boost::math::policies;

typedef policy<
   domain_error<errno_on_error>,
   pole_error<errno_on_error>,
   overflow_error<errno_on_error>,
   evaluation_error<errno_on_error>
> c_policy;

/*`

All we need do now is invoke the BOOST_MATH_DECLARE_SPECIAL_FUNCTIONS
macro passing our policy type as the single argument:

*/

BOOST_MATH_DECLARE_SPECIAL_FUNCTIONS(c_policy)

} // close namespace C

/*`

We now have a set of forwarding functions defined in namespace C
that all look something like this:

``
template <class RealType>
inline typename boost::math::tools::promote_args<RT>::type
   tgamma(RT z)
{
   return boost::math::tgamma(z, c_policy());
}
``

So that when we call `C::tgamma(z)` we really end up calling
`boost::math::tgamma(z, C::c_policy())`:

*/


int main()
{
   errno = 0;
   std::cout << "Result of tgamma(30000) is: "
      << C::tgamma(30000) << std::endl;
   std::cout << "errno = " << errno << std::endl;
   std::cout << "Result of tgamma(-10) is: "
      << C::tgamma(-10) << std::endl;
   std::cout << "errno = " << errno << std::endl;
}

/*`

Which outputs:

[pre
Result of C::tgamma(30000) is: 1.#INF
errno = 34
Result of C::tgamma(-10) is: 1.#QNAN
errno = 33
]

This mechanism is particularly useful when we want to define a
project-wide policy, and don't want to modify the Boost source
or set - possibly fragile and easy to forget - project wide
build macros.

*/
//]

