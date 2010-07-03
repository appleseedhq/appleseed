/* boost random_test.cpp various tests
 *
 * Copyright Jens Maurer 2000
 * Distributed under the Boost Software License, Version 1.0. (See
 * accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * $Id: random_test.cpp 52492 2009-04-19 14:55:57Z steven_watanabe $
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
 * A few equidistribution tests
 */

// yet to come...

template<class Generator>
void check_uniform_int(Generator & gen, int iter)
{
  std::cout << "testing uniform_int(" << (gen.min)() << "," << (gen.max)() 
            << ")" << std::endl;
  int range = (gen.max)()-(gen.min)()+1;
  std::vector<int> bucket(range);
  for(int j = 0; j < iter; j++) {
    int result = gen();
    if(result < (gen.min)() || result > (gen.max)())
      std::cerr << "   ... delivers " << result << std::endl;
    else
      bucket[result-(gen.min)()]++;
  }
  int sum = 0;
  // use a different variable name "k", because MSVC has broken "for" scoping
  for(int k = 0; k < range; k++)
    sum += bucket[k];
  double avg = static_cast<double>(sum)/range;
  double threshold = 2*avg/std::sqrt(static_cast<double>(iter));
  for(int i = 0; i < range; i++) {
    if(std::fabs(bucket[i] - avg) > threshold) {
      // 95% confidence interval
      std::cout << "   ... has bucket[" << i << "] = " << bucket[i] 
                << "  (distance " << (bucket[i] - avg) << ")" 
                << std::endl;
    }
  }
}

template<class Generator>
void test_uniform_int(Generator & gen)
{
  typedef boost::uniform_int<int> int_gen;

  // large range => small range (modulo case)
  typedef boost::variate_generator<Generator&, int_gen> level_one;

  level_one uint12(gen, int_gen(1,2));
  BOOST_CHECK((uint12.distribution().min)() == 1);
  BOOST_CHECK((uint12.distribution().max)() == 2);
  check_uniform_int(uint12, 100000);
  level_one uint16(gen, int_gen(1,6));
  check_uniform_int(uint16, 100000);

  // test chaining to get all cases in operator()

  // identity map
  typedef boost::variate_generator<level_one&, int_gen> level_two;
  level_two uint01(uint12, int_gen(0, 1));
  check_uniform_int(uint01, 100000);

  // small range => larger range
  level_two uint05(uint12, int_gen(-3, 2));
  check_uniform_int(uint05, 100000);

  // larger => small range, rejection case
  typedef boost::variate_generator<level_two&, int_gen> level_three;
  level_three uint1_4(uint05, int_gen(1, 4));
  check_uniform_int(uint1_4, 100000);
}

#if defined(BOOST_MSVC) && _MSC_VER < 1300

// These explicit instantiations are necessary, otherwise MSVC does
// not find the <boost/operators.hpp> inline friends.
// We ease the typing with a suitable preprocessor macro.
#define INSTANT(x) \
template class boost::uniform_smallint<x>; \
template class boost::uniform_int<x>; \
template class boost::uniform_real<x>; \
template class boost::bernoulli_distribution<x>; \
template class boost::geometric_distribution<x>; \
template class boost::triangle_distribution<x>; \
template class boost::exponential_distribution<x>; \
template class boost::normal_distribution<x>; \
template class boost::uniform_on_sphere<x>; \
template class boost::lognormal_distribution<x>;

INSTANT(boost::minstd_rand0)
INSTANT(boost::minstd_rand)
INSTANT(boost::ecuyer1988)
INSTANT(boost::kreutzer1986)
INSTANT(boost::hellekalek1995)
INSTANT(boost::mt19937)
INSTANT(boost::mt11213b)

#undef INSTANT
#endif

#if !defined(BOOST_NO_INT64_T) && !defined(BOOST_NO_INTEGRAL_INT64_T)
// testcase by Mario R�tti
class ruetti_gen
{
public:
  typedef boost::uint64_t result_type;
  result_type min BOOST_PREVENT_MACRO_SUBSTITUTION () const { return 0; }
  result_type max BOOST_PREVENT_MACRO_SUBSTITUTION () const { return std::numeric_limits<result_type>::max BOOST_PREVENT_MACRO_SUBSTITUTION (); }
  result_type operator()() { return (max)()-1; }
};

void test_overflow_range()
{
  ruetti_gen gen;
  boost::variate_generator<ruetti_gen, boost::uniform_int<> >
    rng(gen, boost::uniform_int<>(0, 10));
  for (int i=0;i<10;i++)
    (void) rng();
}
#else
void test_overflow_range()
{ }
#endif

template <typename EngineT>
struct rand_for_random_shuffle
{
  explicit rand_for_random_shuffle(EngineT &engine)
    : m_engine(engine)
  { }

  template <typename IntT>
  IntT operator()(IntT upperBound)
  {
    assert(upperBound > 0);

    if (upperBound == 1)
    {
      return 0;
    }

    typedef boost::uniform_int<IntT> distribution_type;
    typedef boost::variate_generator<EngineT &, distribution_type> generator_type;

    return generator_type(m_engine, distribution_type(0, upperBound - 1))();
  }

  EngineT &m_engine;
        
};

// Test that uniform_int<> can be used with std::random_shuffle
// Author: Jos Hickson
void test_random_shuffle()
{
    typedef boost::uniform_int<> distribution_type;
    typedef boost::variate_generator<boost::mt19937 &, distribution_type> generator_type;

    boost::mt19937 engine1(1234);
    boost::mt19937 engine2(1234);

    rand_for_random_shuffle<boost::mt19937> referenceRand(engine1);

    distribution_type dist(0,10);
    generator_type testRand(engine2, dist);

    std::vector<int> referenceVec;

    for (int i = 0; i < 200; ++i)
    {
      referenceVec.push_back(i);
    }

    std::vector<int> testVec(referenceVec);

    std::random_shuffle(referenceVec.begin(), referenceVec.end(), referenceRand);
    std::random_shuffle(testVec.begin(), testVec.end(), testRand);

    typedef std::vector<int>::iterator iter_type;
    iter_type theEnd(referenceVec.end());

    for (iter_type referenceIter(referenceVec.begin()), testIter(testVec.begin());
         referenceIter != theEnd;
         ++referenceIter, ++testIter)
    {
      BOOST_CHECK_EQUAL(*referenceIter, *testIter);
    }
}


int test_main(int, char*[])
{

#if !defined(__INTEL_COMPILER) || !defined(_MSC_VER) || __INTEL_COMPILER > 700 
  boost::mt19937 mt;
  test_uniform_int(mt);

  // bug report from Ken Mahler:  This used to lead to an endless loop.
  typedef boost::uniform_int<unsigned int> uint_dist;
  boost::minstd_rand mr;
  boost::variate_generator<boost::minstd_rand, uint_dist> r2(mr,
                                                            uint_dist(0, 0xffffffff));
  r2();
  r2();

  // bug report from Fernando Cacciola:  This used to lead to an endless loop.
  // also from Douglas Gregor
  boost::variate_generator<boost::minstd_rand, boost::uniform_int<> > x(mr, boost::uniform_int<>(0, 8361));
  (void) x();

  // bug report from Alan Stokes and others: this throws an assertion
  boost::variate_generator<boost::minstd_rand, boost::uniform_int<> > y(mr, boost::uniform_int<>(1,1));
  std::cout << "uniform_int(1,1) " << y() << ", " << y() << ", " << y()
            << std::endl;

  test_overflow_range();
  test_random_shuffle();

  return 0;
#else
  std::cout << "Intel 7.00 on Win32 loops, so the test is disabled\n";
  return 1;
#endif
}
