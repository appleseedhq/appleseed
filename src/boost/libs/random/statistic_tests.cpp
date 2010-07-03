/* statistic_tests.cpp file
 *
 * Copyright Jens Maurer 2000, 2002
 * Distributed under the Boost Software License, Version 1.0. (See
 * accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * $Id: statistic_tests.cpp 24096 2004-07-27 03:43:34Z dgregor $
 *
 * Revision history
 */

/*
 * NOTE: This is not part of the official boost submission.  It exists
 * only as a collection of ideas.
 */

#include <iostream>
#include <iomanip>
#include <string>
#include <functional>
#include <math.h>  // lgamma is not in namespace std
#include <vector>
#include <algorithm>

#include <boost/cstdint.hpp>
#include <boost/random.hpp>

#include "statistic_tests.hpp"
#include "integrate.hpp"


namespace boost {
namespace random {

// Wikramaratna 1989  ACORN
template<class IntType, int k, IntType m, IntType val>
class additive_congruential
{
public:
  typedef IntType result_type;
#ifndef BOOST_NO_INCLASS_MEMBER_INITIALIZATION
  static const bool has_fixed_range = true;
  static const result_type min_value = 0;
  static const result_type max_value = m-1;
#else
  enum {
    has_fixed_range = true,
    min_value = 0,
    max_value = m-1
  };
#endif
  template<class InputIterator>
  explicit additive_congruential(InputIterator start) { seed(start); }
  template<class InputIterator>
  void seed(InputIterator start)
  {
    for(int i = 0; i <= k; ++i, ++start)
      values[i] = *start;
  }
  
  result_type operator()()
  {
    for(int i = 1; i <= k; ++i) {
      IntType tmp = values[i-1] + values[i];
      if(tmp >= m)
        tmp -= m;
      values[i] = tmp;
    }
    return values[k];
  }
  result_type validation() const { return val; }
private:
  IntType values[k+1];
};


template<class IntType, int r, int s, IntType m, IntType val>
class lagged_fibonacci_int
{
public:
  typedef IntType result_type;
#ifndef BOOST_NO_INCLASS_MEMBER_INITIALIZATION
  static const bool has_fixed_range = true;
  static const result_type min_value = 0;
  static const result_type max_value = m-1;
#else
  enum {
    has_fixed_range = true,
    min_value = 0,
    max_value = m-1
  };
#endif
  explicit lagged_fibonacci_int(IntType start) { seed(start); }
  template<class Generator>
  explicit lagged_fibonacci_int(Generator & gen) { seed(gen); }
  void seed(IntType start)
  {
    linear_congruential<uint32_t, 299375077, 0, 0, 0> init;
    seed(init);
  }
  template<class Generator>
  void seed(Generator & gen)
  {
    assert(r > s);
    for(int i = 0; i < 607; ++i)
      values[i] = gen();
    current = 0;
    lag = r-s;
  }
  
  result_type operator()()
  {
    result_type tmp = values[current] + values[lag];
    if(tmp >= m)
      tmp -= m;
    values[current] = tmp;
    ++current;
    if(current >= r)
      current = 0;
    ++lag;
    if(lag >= r)
      lag = 0;
    return tmp;
  }
  result_type validation() const { return val; }
private:
  result_type values[r];
  int current, lag;
};

} // namespace random
} // namespace boost

// distributions from Haertel's dissertation
// (additional parameterizations of the basic templates)
namespace Haertel {
  typedef boost::random::linear_congruential<boost::uint64_t, 45965, 453816691,
    (boost::uint64_t(1)<<31), 0> LCG_Af2;
  typedef boost::random::linear_congruential<boost::uint64_t, 211936855, 0,
    (boost::uint64_t(1)<<29)-3, 0> LCG_Die1;
  typedef boost::random::linear_congruential<boost::uint32_t, 2824527309u, 0,
    0, 0> LCG_Fis;
  typedef boost::random::linear_congruential<boost::uint64_t, 950706376u, 0,
    (boost::uint64_t(1)<<31)-1, 0> LCG_FM;
  typedef boost::random::linear_congruential<boost::int32_t, 51081, 0,
    2147483647, 0> LCG_Hae;
  typedef boost::random::linear_congruential<boost::uint32_t, 69069, 1,
    0, 0> LCG_VAX;
  typedef boost::random::inversive_congruential<boost::int64_t, 240318, 197, 
    1000081, 0> NLG_Inv1;
  typedef boost::random::inversive_congruential<boost::int64_t, 15707262,
    13262967, (1<<24)-17, 0> NLG_Inv2;
  typedef boost::random::inversive_congruential<boost::int32_t, 1, 1,
    2147483647, 0> NLG_Inv4;
  typedef boost::random::inversive_congruential<boost::int32_t, 1, 2,
    1<<30, 0> NLG_Inv5;
  typedef boost::random::additive_congruential<boost::int32_t, 6,
    (1<<30)-35, 0> MRG_Acorn7;
  typedef boost::random::lagged_fibonacci_int<boost::uint32_t, 607, 273,
    0, 0> MRG_Fib2;

  template<class Gen, class T>
  inline void check_validation(Gen & gen, T value, const std::string & name)
  {
    for(int i = 0; i < 100000-1; ++i)
      gen();
    if(value != gen())
      std::cout << name << ": validation failed" << std::endl;
  }

  // we have validation after 100000 steps with Haertel's generators
  template<class Gen, class T>
  void validate(T value, const std::string & name)
  {
    Gen gen(1234567);
    check_validation(gen, value, name);
  }

  void validate_all()
  {
    validate<LCG_Af2>(183269031u, "LCG_Af2");
    validate<LCG_Die1>(522319944u, "LCG_Die1");
    validate<LCG_Fis>(-2065162233u, "LCG_Fis");
    validate<LCG_FM>(581815473u, "LCG_FM");
    validate<LCG_Hae>(28931709, "LCG_Hae");
    validate<LCG_VAX>(1508154087u, "LCG_VAX");
    validate<NLG_Inv2>(6666884, "NLG_Inv2");
    validate<NLG_Inv4>(1521640076, "NLG_Inv4");
    validate<NLG_Inv5>(641840839, "NLG_Inv5");
    static const int acorn7_init[]
      = { 1234567, 7654321, 246810, 108642, 13579, 97531, 555555 };
    MRG_Acorn7 acorn7(acorn7_init);
    check_validation(acorn7, 874294697, "MRG_Acorn7");
    validate<MRG_Fib2>(1234567u, "MRG_Fib2");
  }
} // namespace Haertel




double normal_density(double x)
{
  const double pi = 3.14159265358979323846;
  return 1/std::sqrt(2*pi) * std::exp(-x*x/2);
}

namespace std {
#ifdef _CXXRTCF_H__
  using _CS_swamp::lgamma;
#elif defined __SGI_STL_PORT
  using ::lgamma;
#endif
}


class chi_square_density : public std::unary_function<double, double>
{
public:
  chi_square_density(int freedom)
    : _exponent( static_cast<double>(freedom)/2-1 ),
      _factor(1/(std::pow(2, _exponent+1) * std::exp(lgamma(_exponent+1))))
  { }

  double operator()(double x)
  {
    return _factor*std::pow(x, _exponent)*std::exp(-x/2);
  }
private:
  double _exponent, _factor;
};

// computes F(x) or F(y) - F(x)
class chi_square_probability : public distribution_function<double>
{
public:
  chi_square_probability(int freedom) : dens(freedom) {}
  double operator()(double x) { return operator()(0, x); }
  double operator()(double x, double y)
  { return trapezoid(dens, x, y, 1000); }
private:
  chi_square_density dens;
};

class uniform_distribution : public distribution_function<double>
{
public:
  uniform_distribution(double from, double to) : from(from), to(to) 
  { assert(from < to); }
  double operator()(double x) 
  { 
    if(x < from) 
      return 0;
    else if(x > to)
      return 1;
    else
      return (x-from)/(to-from);
  }
  double operator()(double x, double delta)
  { return operator()(x+delta) - operator()(x); }
private:
  double from, to;
};

class test_environment;

class test_base
{
protected:
  explicit test_base(test_environment & env) : environment(env) { }
  void check(double val) const; 
private:
  test_environment & environment;
};

class equidistribution_test : test_base
{
public:
  equidistribution_test(test_environment & env, unsigned int classes, 
                        unsigned int high_classes)
    : test_base(env), classes(classes),
      test_distrib_chi_square(chi_square_probability(classes-1), high_classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "equidistribution: " << std::flush;
    equidistribution_experiment equi(classes);
    uniform_smallint<RNG> uint_linear(rng, 0, classes-1);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(equi, uint_linear, n1), n2));
    check(run_experiment(test_distrib_chi_square, 
                         experiment_generator(equi, uint_linear, n1), 2*n2));

    std::cout << "  2D: " << std::flush;
    equidistribution_2d_experiment equi_2d(classes);
    unsigned int root = static_cast<unsigned int>(std::sqrt(double(classes)));
    assert(root * root == classes);
    uniform_smallint<RNG> uint_square(rng, 0, root-1);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(equi_2d, uint_square, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(equi_2d, uint_square, n1), 2*n2));
    std::cout << std::endl;
  }
private:
  unsigned int classes;
  distribution_experiment test_distrib_chi_square;
};

class ks_equidistribution_test : test_base
{
public:
  ks_equidistribution_test(test_environment & env, unsigned int classes)
    : test_base(env),
      test_distrib_chi_square(kolmogorov_smirnov_probability(5000), 
                              classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "KS: " << std::flush;
    // generator_reference_t<RNG> gen_ref(rng);
    RNG& gen_ref(rng);
    kolmogorov_experiment ks(n1);
    uniform_distribution ud((rng.min)(), (rng.max)());
    check(run_experiment(test_distrib_chi_square,
                         ks_experiment_generator(ks, gen_ref, ud), n2));
    check(run_experiment(test_distrib_chi_square,
                         ks_experiment_generator(ks, gen_ref, ud), 2*n2));
  }
private:
  distribution_experiment test_distrib_chi_square;
};

class runs_test : test_base
{
public:
  runs_test(test_environment & env, unsigned int classes,
            unsigned int high_classes)
    : test_base(env), classes(classes),
      test_distrib_chi_square(chi_square_probability(classes-1), high_classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "runs: up: " << std::flush;
    runs_experiment<true> r_up(classes);
    // generator_reference_t<RNG> gen_ref(rng);
    RNG& gen_ref(rng);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(r_up, gen_ref, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(r_up, gen_ref, n1), 2*n2));

    std::cout << "  down: " << std::flush;
    runs_experiment<false> r_down(classes);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(r_down, gen_ref, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(r_down, gen_ref, n1), 2*n2));

    std::cout << std::endl;
  }
private:
  unsigned int classes;
  distribution_experiment test_distrib_chi_square;
};

class gap_test : test_base
{
public:
  gap_test(test_environment & env, unsigned int classes,
            unsigned int high_classes)
    : test_base(env), classes(classes),
      test_distrib_chi_square(chi_square_probability(classes-1), high_classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "gaps: " << std::flush;
    gap_experiment gap(classes, 0.2, 0.8);
    // generator_reference_t<RNG> gen_ref(rng);
    RNG& gen_ref(rng);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(gap, gen_ref, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(gap, gen_ref, n1), 2*n2));

    std::cout << std::endl;
  }
private:
  unsigned int classes;
  distribution_experiment test_distrib_chi_square;
};

class poker_test : test_base
{
public:
  poker_test(test_environment & env, unsigned int classes,
             unsigned int high_classes)
    : test_base(env), classes(classes),
      test_distrib_chi_square(chi_square_probability(classes-1), high_classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "poker: " << std::flush;
    poker_experiment poker(8, classes);
    uniform_smallint<RNG> usmall(rng, 0, 7);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(poker, usmall, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(poker, usmall, n1), 2*n2));
    std::cout << std::endl;
  }
private:
  unsigned int classes;
  distribution_experiment test_distrib_chi_square;
};

class coupon_collector_test : test_base
{
public:
  coupon_collector_test(test_environment & env, unsigned int classes,
                        unsigned int high_classes)
    : test_base(env), classes(classes),
      test_distrib_chi_square(chi_square_probability(classes-1), high_classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "coupon collector: " << std::flush;
    coupon_collector_experiment coupon(5, classes);

    uniform_smallint<RNG> usmall(rng, 0, 4);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(coupon, usmall, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(coupon, usmall, n1), 2*n2));
    std::cout << std::endl;
  }
private:
  unsigned int classes;
  distribution_experiment test_distrib_chi_square;
};

class permutation_test : test_base
{
public:
  permutation_test(test_environment & env, unsigned int classes,
                   unsigned int high_classes)
    : test_base(env), classes(classes),
      test_distrib_chi_square(chi_square_probability(fac<int>(classes)-1), 
                              high_classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "permutation: " << std::flush;
    permutation_experiment perm(classes);
    
    // generator_reference_t<RNG> gen_ref(rng);
    RNG& gen_ref(rng);
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(perm, gen_ref, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(perm, gen_ref, n1), 2*n2));
    std::cout << std::endl;
  }
private:
  unsigned int classes;
  distribution_experiment test_distrib_chi_square;
};

class maximum_test : test_base
{
public:
  maximum_test(test_environment & env, unsigned int high_classes)
    : test_base(env),
      test_distrib_chi_square(kolmogorov_smirnov_probability(1000),
                              high_classes)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "maximum-of-t: " << std::flush;
    maximum_experiment<RNG> mx(rng, n1, 5);    
    check(run_experiment(test_distrib_chi_square, mx, n2));
    check(run_experiment(test_distrib_chi_square, mx, 2*n2));
    std::cout << std::endl;
  }
private:
  distribution_experiment test_distrib_chi_square;
};

class birthday_test : test_base
{
public:
  birthday_test(test_environment & env)
    : test_base(env)
  { }

  template<class RNG>
  void run(RNG & rng, int n1, int n2)
  {
    using namespace boost;
    std::cout << "birthday spacing: " << std::flush;
    uniform_int<RNG> uni(rng, 0, (1<<25)-1);
    birthday_spacing_experiment bsp(4, 512, (1<<25));
    std::cout << run_experiment(bsp, uni, n1);
#if 0
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(perm, gen_ref, n1), n2));
    check(run_experiment(test_distrib_chi_square,
                         experiment_generator(perm, gen_ref, n1), 2*n2));
#endif
    std::cout << std::endl;
  }
  
    
};

class test_environment
{
public:
  static const int classes = 20;
  explicit test_environment(double confid) 
    : confidence(confid),
      confidence_chi_square_quantil(quantil(chi_square_density(classes-1), 0, confidence, 1e-4)),
      test_distrib_chi_square6(chi_square_probability(7-1), classes),
      ksequi_test(*this, classes),
      equi_test(*this, 100, classes),
      rns_test(*this, 7, classes),
      gp_test(*this, 7, classes),
      pk_test(*this, 5, classes),
      cpn_test(*this, 15, classes),
      perm_test(*this, 5, classes),
      max_test(*this, classes),
      bday_test(*this)
  {
    std::cout << "Confidence level: " << confid 
              << "; 1-alpha = " << (1-confid)
              << "; chi_square(" << (classes-1) 
              << ", " << confidence_chi_square_quantil
              << ") = " 
              << chi_square_probability(classes-1)(0, confidence_chi_square_quantil)
              << std::endl;
  }
  
  bool check_confidence(double val, double chi_square_conf) const
  {
    std::cout << val;
    bool result = (val <= chi_square_conf);
    if(!result) {
      std::cout << "* [";
      double prob = (val > 10*chi_square_conf ? 1 :
                     chi_square_probability(classes-1)(0, val));
      std::cout << (1-prob) << "]";
    }
    std::cout << " " << std::flush;
    return result;
  }

  bool check(double chi_square_value) const
  {
    return check_confidence(chi_square_value, confidence_chi_square_quantil);
  }

  template<class RNG>
  void run_test(const std::string & name)
  {
    using namespace boost;

    std::cout << "Running tests on " << name << std::endl;

    RNG rng(1234567);
    typedef boost::uniform_01<RNG> UGen;

#if 1
    ksequi_test.run(rng, 5000, 250);
    equi_test.run(rng, 5000, 250);
    rns_test.run(rng, 100000, 250);
    gp_test.run(rng, 10000, 250);
    pk_test.run(rng, 5000, 250);
    cpn_test.run(rng, 500, 250);
    perm_test.run(rng, 1200, 250);
    max_test.run(rng, 1000, 250);
#endif
    bday_test.run(rng, 1000, 150);

    std::cout << std::endl;
  }

private:
  double confidence;
  double confidence_chi_square_quantil;
  distribution_experiment test_distrib_chi_square6;
  ks_equidistribution_test ksequi_test;
  equidistribution_test equi_test;
  runs_test rns_test;
  gap_test gp_test;
  poker_test pk_test;
  coupon_collector_test cpn_test;
  permutation_test perm_test;
  maximum_test max_test;
  birthday_test bday_test;
};

void test_base::check(double val) const
{ 
  environment.check(val);
}

void print_ks_table()
{
  std::cout.setf(std::ios::fixed);
  std::cout.precision(5);
  static const double all_p[] = { 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99 };
  for(int n = 0; n <= 10000;  (n < 55 ? ++n : n *= 10)) {
    std::cout << std::setw(4) << n << " ";
    for(unsigned int i = 0; i < sizeof(all_p)/sizeof(all_p[0]); ++i) {
      std::cout << std::setw(8) 
                << (n == 0 ? all_p[i] : 
                    invert_monotone_inc(kolmogorov_smirnov_probability(n), all_p[i], 0, 10))
                << " ";
    }
    std::cout << std::endl;
  }
}

int main()
{
  //  Haertel::validate_all();
  test_environment env(0.99);
  env.run_test<boost::minstd_rand>("minstd_rand");
  env.run_test<boost::mt19937>("mt19937");
  env.run_test<Haertel::LCG_Af2>("LCG_Af2");
  env.run_test<Haertel::LCG_Die1>("LCG_Die1");
  env.run_test<Haertel::LCG_Fis>("LCG_Fis");
  env.run_test<Haertel::LCG_FM>("LCG_FM");
  env.run_test<Haertel::LCG_Hae>("LCG_Hae");
  env.run_test<Haertel::LCG_VAX>("LCG_VAX");
  env.run_test<Haertel::NLG_Inv1>("NLG_Inv1");
  env.run_test<Haertel::NLG_Inv2>("NLG_Inv2");
  env.run_test<Haertel::NLG_Inv4>("NLG_Inv4");
  env.run_test<Haertel::NLG_Inv5>("NLG_Inv5");
}
