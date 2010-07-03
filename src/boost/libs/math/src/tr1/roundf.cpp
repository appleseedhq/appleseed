//  Copyright John Maddock 2008.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0.  (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#define BOOST_MATH_TR1_SOURCE
#include <boost/math/tr1.hpp>
#include <boost/math/special_functions/round.hpp>
#include "c_policy.hpp"

namespace boost{ namespace math{ namespace tr1{

extern "C" float BOOST_MATH_TR1_DECL roundf BOOST_PREVENT_MACRO_SUBSTITUTION(float x)
{
   return c_policies::round BOOST_PREVENT_MACRO_SUBSTITUTION(x);
}

}}}
