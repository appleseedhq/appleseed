//  Copyright John Maddock 2008.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0.  (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#define BOOST_MATH_TR1_SOURCE
#include <boost/math/tr1.hpp>
#include <boost/math/special_functions/spherical_harmonic.hpp>
#include "c_policy.hpp"

extern "C" long double BOOST_MATH_TR1_DECL sph_legendrel BOOST_PREVENT_MACRO_SUBSTITUTION(unsigned n, unsigned m, long double x)
{
   return  (m & 1 ? -1 : 1) * c_policies::spherical_harmonic_r BOOST_PREVENT_MACRO_SUBSTITUTION(n, m, x, 0.0L);
}
