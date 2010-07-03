
// Copyright 2005-2009 Daniel James.
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include "./config.hpp"

#ifdef TEST_EXTENSIONS
#  ifdef TEST_STD_INCLUDES
#    include <functional>
#  else
#    include <boost/functional/hash.hpp>
#  endif
#endif

#include <boost/detail/lightweight_test.hpp>

#ifdef TEST_EXTENSIONS

#include <complex>
#include <sstream>
#include <boost/limits.hpp>

#if defined(BOOST_MSVC)
#pragma warning(push)
#pragma warning(disable:4244) // conversion from 'unsigned long' to 'unsigned short', possible loss of data
#pragma warning(disable:4512) // assignment operator could not be generated
#endif

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/uniform_real.hpp>
#include <boost/random/variate_generator.hpp>

#if defined(BOOST_MSVC)
#pragma warning(pop)
#endif

template <class T>
void generic_complex_tests(std::complex<T> v)
{
    HASH_NAMESPACE::hash<std::complex<T> > complex_hasher;

    BOOST_TEST(complex_hasher(v) == complex_hasher(v));

    HASH_NAMESPACE::hash<T> real_hasher;
    T real = v.real();
    T imag = v.imag();

    BOOST_TEST(real_hasher(real) == complex_hasher(std::complex<T>(real)));

    if(imag != 0 && real_hasher(real) == complex_hasher(v)) {
        std::ostringstream os;
        os<<"real_hasher("<<real<<") == complex_hasher("
            <<v.real()<<" + "<<v.imag()<<"i) == "
            <<real_hasher(real)<<" (This might not be a bug).";
        BOOST_ERROR(os.str().c_str());
    }
}

template <class Float>
void complex_float_tests(Float*)
{
    boost::mt19937 rng;
    boost::uniform_real<Float> uniform;
    boost::variate_generator<boost::mt19937&, boost::uniform_real<Float> >
        uniform_generator(rng, uniform);

    for(int i = 0; i < 100; ++i)
    {
        std::complex<Float> v(uniform_generator(), uniform_generator());
        generic_complex_tests(v);
    }
}

template <class Integer>
void complex_integral_tests(Integer*)
{
    boost::mt19937 rng;
    boost::uniform_int<Integer> uniform(
            (std::numeric_limits<Integer>::min)(),
            (std::numeric_limits<Integer>::max)());
    boost::variate_generator<boost::mt19937&, boost::uniform_int<Integer> >
        uniform_generator(rng, uniform);

    for(int i = 0; i < 100; ++i)
    {
        std::complex<Integer>v(uniform_generator(), uniform_generator());
        generic_complex_tests(v);
    }
}

int main()
{
    complex_float_tests((float*) 0);
    complex_float_tests((double*) 0);
    complex_float_tests((long double*) 0);
    complex_integral_tests((short*) 0);
    complex_integral_tests((int*) 0);
    complex_integral_tests((long*) 0);
    complex_integral_tests((unsigned short*) 0);
    complex_integral_tests((unsigned int*) 0);
    complex_integral_tests((unsigned long*) 0);

    return boost::report_errors();
}

#endif
