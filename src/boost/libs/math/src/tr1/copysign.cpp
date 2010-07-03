//  Copyright John Maddock 2008.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0.  (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#define BOOST_MATH_TR1_SOURCE
#include <boost/math/tr1.hpp>
#include <boost/math/special_functions/sign.hpp>
#include "c_policy.hpp"

#if !(defined(__HP_aCC) && (__HP_aCC >= 61400))

extern "C" double BOOST_MATH_TR1_DECL copysign BOOST_PREVENT_MACRO_SUBSTITUTION(double x, double y)
{
   return boost::math::copysign BOOST_PREVENT_MACRO_SUBSTITUTION(x, y);
}

#endif
