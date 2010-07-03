/* boost random_speed.cpp performance measurements
 *
 * Copyright Jens Maurer 2000
 * Distributed under the Boost Software License, Version 1.0. (See
 * accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * $Id: random_speed.cpp 24096 2004-07-27 03:43:34Z dgregor $
 */

#include <iostream>
#include <cstdlib>
#include <string>
#include <boost/config.hpp>
#include <boost/random.hpp>
#include <boost/progress.hpp>
#include <boost/shared_ptr.hpp>

/*
 * Configuration Section
 */

// define if your C library supports the non-standard drand48 family
#undef HAVE_DRAND48

// define if you have the original mt19937int.c (with commented out main())
#undef HAVE_MT19937INT_C

// set to your CPU frequency in MHz
static const double cpu_frequency = 200 * 1e6;

/*
 * End of Configuration Section
 */

/*
 * General portability note:
 * MSVC mis-compiles explicit function template instantiations.
 * For example, f<A>() and f<B>() are both compiled to call f<A>().
 * BCC is unable to implicitly convert a "const char *" to a std::string
 * when using explicit function template instantiations.
 *
 * Therefore, avoid explicit function template instantiations.
 */

// provides a run-time configurable linear congruential generator, just
// for comparison
template<class IntType>
class linear_congruential
{
public:
  typedef IntType result_type;

  BOOST_STATIC_CONSTANT(bool, has_fixed_range = false);

  linear_congruential(IntType x0, IntType a, IntType c, IntType m)
    : _x(x0), _a(a), _c(c), _m(m) { }
  // compiler-generated copy ctor and assignment operator are fine
  void seed(IntType x0, IntType a, IntType c, IntType m)
    { _x = x0; _a = a; _c = c; _m = m; }
  void seed(IntType x0) { _x = x0; }
  result_type operator()() { _x = (_a*_x+_c) % _m; return _x; }
  result_type min BOOST_PREVENT_MACRO_SUBSTITUTION () const { return _c == 0 ? 1 : 0; }
  result_type max BOOST_PREVENT_MACRO_SUBSTITUTION () const { return _m -1; }

private:
  IntType _x, _a, _c, _m;
};


// simplest "random" number generator possible, to check on overhead
class counting
{
public:
  typedef int result_type;

  BOOST_STATIC_CONSTANT(bool, has_fixed_range = false);

  counting() : _x(0) { }
  result_type operator()() { return ++_x; }
  result_type min BOOST_PREVENT_MACRO_SUBSTITUTION () const { return 1; }
  result_type max BOOST_PREVENT_MACRO_SUBSTITUTION () const { return (std::numeric_limits<result_type>::max)(); }

private:
  int _x;
};


// decoration of variate_generator to make it runtime-exchangeable
// for speed comparison
template<class Ret>
class RandomGenBase
{
public:
  virtual Ret operator()() = 0;
  virtual ~RandomGenBase() { }
};

template<class URNG, class Dist, class Ret = typename Dist::result_type>
class DynamicRandomGenerator
  : public RandomGenBase<Ret>
{
public:
  DynamicRandomGenerator(URNG& urng, const Dist& d) : _rng(urng, d) { }
  Ret operator()() { return _rng(); }
private:
  boost::variate_generator<URNG&, Dist> _rng;
};

template<class Ret>
class GenericRandomGenerator
{
public:
  typedef Ret result_type;

  GenericRandomGenerator() { };
  void set(boost::shared_ptr<RandomGenBase<Ret> > p) { _p = p; }
  // takes over ownership
  void set(RandomGenBase<Ret> * p) { _p.reset(p); }
  Ret operator()() { return (*_p)(); }
private:
  boost::shared_ptr<RandomGenBase<Ret> > _p;
};


// start implementation of measuring timing

void show_elapsed(double end, int iter, const std::string & name)
{
  double usec = end/iter*1e6;
  double cycles = usec * cpu_frequency/1e6;
  std::cout << name << ": " 
            << usec*1e3 << " nsec/loop = " 
            << cycles << " CPU cycles"
            << std::endl;
}

#if 0
template<class RNG>
void timing(RNG & rng, int iter, const std::string& name)
{
  // make sure we're not optimizing too much
  volatile typename RNG::result_type tmp;
  boost::timer t;
  for(int i = 0; i < iter; i++)
    tmp = rng();
  show_elapsed(t.elapsed(), iter, name);
}
#endif

// overload for using a copy, allows more concise invocation
template<class RNG>
void timing(RNG rng, int iter, const std::string& name)
{
  // make sure we're not optimizing too much
  volatile typename RNG::result_type tmp;
  boost::timer t;
  for(int i = 0; i < iter; i++)
    tmp = rng();
  show_elapsed(t.elapsed(), iter, name);
}

template<class RNG>
void timing_sphere(RNG rng, int iter, const std::string & name)
{
  boost::timer t;
  for(int i = 0; i < iter; i++) {
    // the special return value convention of uniform_on_sphere saves 20% CPU
    const std::vector<double> & tmp = rng();
    (void) tmp[0];
  }
  show_elapsed(t.elapsed(), iter, name);
}

template<class RNG>
void run(int iter, const std::string & name, const RNG &)
{
  std::cout << (RNG::has_fixed_range ? "fixed-range " : "");
  // BCC has trouble with string autoconversion for explicit specializations
  timing(RNG(), iter, std::string(name));
}

#ifdef HAVE_DRAND48
// requires non-standard C library support for srand48/lrand48
void run(int iter, const std::string & name, int)
{
  std::srand48(1);
  timing(&std::lrand48, iter, name);
}
#endif

#ifdef HAVE_MT19937INT_C  // requires the original mt19937int.c
extern "C" void sgenrand(unsigned long);
extern "C" unsigned long genrand();

void run(int iter, const std::string & name, float)
{
  sgenrand(4357);
  timing(genrand, iter, name, 0u);
}
#endif

template<class PRNG, class Dist>
inline boost::variate_generator<PRNG&, Dist> make_gen(PRNG & rng, Dist d)
{
  return boost::variate_generator<PRNG&, Dist>(rng, d);
}

template<class Gen>
void distrib(int iter, const std::string & name, const Gen &)
{
  Gen gen;

  timing(make_gen(gen, boost::uniform_int<>(-2, 4)),
         iter, name + " uniform_int");

  timing(make_gen(gen, boost::geometric_distribution<>(0.5)),
         iter, name + " geometric");

  timing(make_gen(gen, boost::binomial_distribution<int>(4, 0.8)),
         iter, name + " binomial");

  timing(make_gen(gen, boost::poisson_distribution<>(1)),
         iter, name + " poisson");


  timing(make_gen(gen, boost::uniform_real<>(-5.3, 4.8)),
         iter, name + " uniform_real");

  timing(make_gen(gen, boost::triangle_distribution<>(1, 2, 7)),
         iter, name + " triangle");

  timing(make_gen(gen, boost::exponential_distribution<>(3)),
         iter, name + " exponential");

  timing(make_gen(gen, boost::normal_distribution<>()),
                  iter, name + " normal polar");

  timing(make_gen(gen, boost::lognormal_distribution<>()),
         iter, name + " lognormal");

  timing(make_gen(gen, boost::cauchy_distribution<>()),
         iter, name + " cauchy");

  timing(make_gen(gen, boost::cauchy_distribution<>()),
         iter, name + " gamma");

  timing_sphere(make_gen(gen, boost::uniform_on_sphere<>(3)),
                iter/10, name + " uniform_on_sphere");
}


template<class URNG, class Dist>
inline boost::shared_ptr<DynamicRandomGenerator<URNG, Dist> >
make_dynamic(URNG & rng, const Dist& d)
{
  typedef DynamicRandomGenerator<URNG, Dist> type;
  return boost::shared_ptr<type>(new type(rng, d));
}

template<class Gen>
void distrib_runtime(int iter, const std::string & n, const Gen &)
{
  std::string name = n + " virtual function ";
  Gen gen;

  GenericRandomGenerator<int> g_int;

  g_int.set(make_dynamic(gen, boost::uniform_int<>(-2,4)));
  timing(g_int, iter, name + "uniform_int");

  g_int.set(make_dynamic(gen, boost::geometric_distribution<>(0.5)));
  timing(g_int, iter, name + "geometric");

  g_int.set(make_dynamic(gen,  boost::binomial_distribution<>(4, 0.8)));
  timing(g_int, iter, name + "binomial");

  g_int.set(make_dynamic(gen, boost::poisson_distribution<>(1)));
  timing(g_int, iter, name + "poisson");

  GenericRandomGenerator<double> g;

  g.set(make_dynamic(gen, boost::uniform_real<>(-5.3, 4.8)));
  timing(g, iter, name + "uniform_real");

  g.set(make_dynamic(gen, boost::triangle_distribution<>(1, 2, 7)));
  timing(g, iter, name + "triangle");

  g.set(make_dynamic(gen, boost::exponential_distribution<>(3)));
  timing(g, iter, name + "exponential");

  g.set(make_dynamic(gen, boost::normal_distribution<>()));
  timing(g, iter, name + "normal polar");

  g.set(make_dynamic(gen, boost::lognormal_distribution<>()));
  timing(g, iter, name + "lognormal");

  g.set(make_dynamic(gen, boost::cauchy_distribution<>()));
  timing(g, iter, name + "cauchy");

  g.set(make_dynamic(gen, boost::gamma_distribution<>(0.4)));
  timing(g, iter, name + "gamma");
}


int main(int argc, char*argv[])
{
  if(argc != 2) {
    std::cerr << "usage: " << argv[0] << " iterations" << std::endl;
    return 1;
  }

  // okay, it's ugly, but it's only used here
  int iter =
#ifndef BOOST_NO_STDC_NAMESPACE
    std::
#endif
    atoi(argv[1]);

#if !defined(BOOST_NO_INT64_T) && \
    !defined(BOOST_NO_INCLASS_MEMBER_INITIALIZATION)
  run(iter, "rand48", boost::rand48());
  linear_congruential<boost::uint64_t>
    lcg48(boost::uint64_t(1)<<16 | 0x330e,
          boost::uint64_t(0xDEECE66DUL) | (boost::uint64_t(0x5) << 32), 0xB,
          boost::uint64_t(1)<<48);
  timing(lcg48, iter, "lrand48 run-time");
#endif

#ifdef HAVE_DRAND48
  // requires non-standard C library support for srand48/lrand48
  run(iter, "lrand48", 0);   // coded for lrand48()
#endif

  run(iter, "minstd_rand", boost::minstd_rand());
  run(iter, "ecuyer combined", boost::ecuyer1988());
  run(iter, "kreutzer1986", boost::kreutzer1986());

  run(iter, "hellekalek1995 (inversive)", boost::hellekalek1995());

  run(iter, "mt11213b", boost::mt11213b());
  run(iter, "mt19937", boost::mt19937());

  run(iter, "subtract_with_carry", boost::random::ranlux_base());
  run(iter, "subtract_with_carry_01", boost::random::ranlux_base_01());
  run(iter, "ranlux3", boost::ranlux3());
  run(iter, "ranlux4", boost::ranlux4());
  run(iter, "ranlux3_01", boost::ranlux3_01());
  run(iter, "ranlux4_01", boost::ranlux4_01());
  run(iter, "counting", counting());

#ifdef HAVE_MT19937INT_C
  // requires the original mt19937int.c
  run<float>(iter, "mt19937 original");   // coded for sgenrand()/genrand()
#endif

  distrib(iter, "counting", counting());
  distrib_runtime(iter, "counting", counting());

  distrib(iter, "minstd_rand", boost::minstd_rand());

  distrib(iter, "kreutzer1986", boost::kreutzer1986());

  distrib(iter, "mt19937", boost::mt19937());
  distrib_runtime(iter, "mt19937", boost::mt19937());
}
