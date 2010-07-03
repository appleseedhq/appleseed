//  Copyright John Maddock 2008.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0.  (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#include <boost/math/policies/policy.hpp>
#include <boost/math/special_functions/math_fwd.hpp>

namespace c_policies{

using boost::math::policies::policy;
using boost::math::policies::errno_on_error;
using boost::math::policies::domain_error;
using boost::math::policies::pole_error;
using boost::math::policies::overflow_error;
using boost::math::policies::rounding_error;
using boost::math::policies::evaluation_error;

typedef policy<
   domain_error<errno_on_error>,
   pole_error<errno_on_error>,
   overflow_error<errno_on_error>,
   rounding_error<errno_on_error>,
   evaluation_error<errno_on_error>
> c_policy;

BOOST_MATH_DECLARE_SPECIAL_FUNCTIONS(c_policy)

}
