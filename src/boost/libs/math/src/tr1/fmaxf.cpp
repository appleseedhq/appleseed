//  Copyright John Maddock 2008.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0.  (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#define BOOST_MATH_TR1_SOURCE
#include <boost/math/tr1.hpp>
#include <boost/math/special_functions/fpclassify.hpp>
#include "c_policy.hpp"

#if !(defined(__HP_aCC) && (__HP_aCC >= 61400))

extern "C" float BOOST_MATH_TR1_DECL fmaxf BOOST_PREVENT_MACRO_SUBSTITUTION(float x, float y)
{
   if((boost::math::isnan)(x))
      return y;
   if((boost::math::isnan)(y))
      return x;
   return (std::max)(x, y);
}

#endif
