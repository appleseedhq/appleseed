/* boost validate.cpp
 *
 * Copyright Jens Maurer 2000
 * Distributed under the Boost Software License, Version 1.0. (See
 * accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * $Id: validate.cpp 52492 2009-04-19 14:55:57Z steven_watanabe $
 */

#if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
#pragma warning( disable : 4786 )
#endif

#include <iostream>
#include <sstream>
#include <string>
#include <cmath>
#include <iterator>
#include <vector>
#include <boost/random.hpp>
#include <boost/config.hpp>

#include <boost/test/test_tools.hpp>
#include <boost/test/included/test_exec_monitor.hpp>

#ifdef BOOST_NO_STDC_NAMESPACE
  namespace std { using ::abs; using ::fabs; using ::pow; }
#endif


/*
 * General portability note:
 * MSVC mis-compiles explicit function template instantiations.
 * For example, f<A>() and f<B>() are both compiled to call f<A>().
 * BCC is unable to implicitly convert a "const char *" to a std::string
 * when using explicit function template instantiations.
 *
 * Therefore, avoid explicit function template instantiations.
 */

/*
 * Validate correct implementation
 */

// own run
bool check(unsigned long x, const boost::mt11213b&) { return x == 3809585648U; }

// validation by experiment from mt19937.c
bool check(unsigned long x, const boost::mt19937&) { return x == 4123659995U; }

// validation values from the publications
bool check(int x, const boost::minstd_rand0&) { return x == 1043618065; }

// validation values from the publications
bool check(int x, const boost::minstd_rand&) { return x == 399268537; }

#if !defined(BOOST_NO_INT64_T) && !defined(BOOST_NO_INTEGRAL_INT64_T)
// by experiment from lrand48()
bool check(unsigned long x, const boost::rand48&) { return x == 1993516219; }
#endif

// ????
bool check(unsigned long x, const boost::taus88&) { return x == 3535848941U; }

// ????
bool check(int x, const boost::ecuyer1988&) { return x == 2060321752; }

// validation by experiment from Harry Erwin's generator.h (private e-mail)
bool check(unsigned int x, const boost::kreutzer1986&) { return x == 139726; }

bool check(double x, const boost::lagged_fibonacci607&) { return std::abs(x-0.401269) < 1e-5; }

// principal operation validated with CLHEP, values by experiment
bool check(unsigned long x, const boost::ranlux3&) { return x == 5957620; }
bool check(unsigned long x, const boost::ranlux4&) { return x == 8587295; }

bool check(float x, const boost::ranlux3_01&)
{ return std::abs(x-5957620/std::pow(2.0f,24)) < 1e-6; }
bool check(float x, const boost::ranlux4_01&)
{ return std::abs(x-8587295/std::pow(2.0f,24)) < 1e-6; }

bool check(double x, const boost::ranlux64_3_01&)
{ return std::abs(x-0.838413) < 1e-6; }
bool check(double x, const boost::ranlux64_4_01&)
{ return std::abs(x-0.59839) < 1e-6; }

template<class PRNG>
void validate(const std::string & name, const PRNG &)
{
  std::cout << "validating " << name << ": ";
  PRNG rng;  // default ctor
  for(int i = 0; i < 9999; i++)
    rng();
  typename PRNG::result_type val = rng();
  // make sure the validation function is a static member
  bool result = check(val, rng);
  
  // allow for a simple eyeball check for MSVC instantiation brokenness
  // (if the numbers for all generators are the same, it's obviously broken)
  std::cout << val << std::endl;
  BOOST_CHECK(result);
}

void validate_all()
{
  using namespace boost;
#if !defined(BOOST_NO_INT64_T) && !defined(BOOST_NO_INTEGRAL_INT64_T)
  validate("rand48", rand48());
#endif
  validate("minstd_rand", minstd_rand());
  validate("minstd_rand0", minstd_rand0());
  validate("ecuyer combined", ecuyer1988());
  validate("mt11213b", mt11213b());
  validate("mt19937", mt19937());
  validate("kreutzer1986", kreutzer1986());
  validate("ranlux3", ranlux3());
  validate("ranlux4", ranlux4());
  validate("ranlux3_01", ranlux3_01());
  validate("ranlux4_01", ranlux4_01());
  validate("ranlux64_3_01", ranlux64_3_01());
  validate("ranlux64_4_01", ranlux64_4_01());
  validate("taus88", taus88());
  validate("lagged_fibonacci607", lagged_fibonacci607());
}

int test_main(int, char*[])
{
  validate_all();
  return 0;
}
