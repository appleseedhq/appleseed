// find_root_example.cpp

// Copyright Paul A. Bristow 2007.

// Use, modification and distribution are subject to the
// Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

// Example of using root finding.

// Note that this file contains Quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[root_find1
/*`
First we need some includes to access the normal distribution
(and some std output of course).
*/

#include <boost/math/tools/roots.hpp> // root finding.

#include <boost/math/distributions/normal.hpp> // for normal_distribution
  using boost::math::normal; // typedef provides default type is double.

#include <iostream>
  using std::cout; using std::endl; using std::left; using std::showpoint; using std::noshowpoint;
#include <iomanip>
  using std::setw; using std::setprecision;
#include <limits>
  using std::numeric_limits;
//] //[/root_find1]

namespace boost{ namespace math { namespace tools
{

template <class F, class T, class Tol>
inline std::pair<T, T> bracket_and_solve_root(F f, // functor 
                                              const T& guess,
                                              const T& factor,
                                              bool rising,
                                              Tol tol, // binary functor specifying termination when tol(min, max) becomes true.
                                              // eps_tolerance most suitable for this continuous function
                                              boost::uintmax_t& max_iter); // explicit (rather than default) max iterations.
// return interval as a pair containing result.

namespace detail
{

  // Functor for finding standard deviation:
  template <class RealType, class Policy>
  struct standard_deviation_functor
  {
     standard_deviation_functor(RealType m, RealType s, RealType d)
        : mean(m), standard_deviation(s)
     {
     }
     RealType operator()(const RealType& sd)
     { // Unary functor - the function whose root is to be found.
        if(sd <= tools::min_value<RealType>())
        { // 
           return 1;
        }
        normal_distribution<RealType, Policy> t(mean, sd);
        RealType qa = quantile(complement(t, alpha));
        RealType qb = quantile(complement(t, beta));
        qa += qb;
        qa *= qa;
        qa *= ratio;
        qa -= (df + 1);
        return qa;
     } // operator()
     RealType mean;
     RealType standard_deviation;
  }; // struct standard_deviation_functor
} // namespace detail

template <class RealType, class Policy>
RealType normal_distribution<RealType, Policy>::find_standard_deviation(
      RealType difference_from_mean,
      RealType mean,
      RealType sd,
      RealType hint) // Best guess available - current sd if none better?
{
   static const char* function = "boost::math::normal_distribution<%1%>::find_standard_deviation";

   // Check for domain errors:
   RealType error_result;
   if(false == detail::check_probability(
      function, sd, &error_result, Policy())
      )
      return error_result;

   if(hint <= 0)
   { // standard deviation can never be negative.
      hint = 1;
   }

   detail::standard_deviation_functor<RealType, Policy> f(mean, sd, difference_from_mean);
   tools::eps_tolerance<RealType> tol(policies::digits<RealType, Policy>());
   boost::uintmax_t max_iter = 100;
   std::pair<RealType, RealType> r = tools::bracket_and_solve_root(f, hint, RealType(2), false, tol, max_iter, Policy());
   RealType result = r.first + (r.second - r.first) / 2;
   if(max_iter == 100)
   {
      policies::raise_evaluation_error<RealType>(function, "Unable to locate solution in a reasonable time:"
         " either there is no answer to how many degrees of freedom are required"
         " or the answer is infinite.  Current best guess is %1%", result, Policy());
   }
   return result;
} // find_standard_deviation
} // namespace tools
} // namespace math
} // namespace boost


int main()
{
  cout << "Example: Normal distribution, root finding.";
  try
  {

//[root_find2

/*`A machine is set to pack 3 kg of ground beef per pack.  
Over a long period of time it is found that the average packed was 3 kg
with a standard deviation of 0.1 kg.  
Assuming the packing is normally distributed,
we can find the fraction (or %) of packages that weigh more than 3.1 kg.
*/

double mean = 3.; // kg
double standard_deviation = 0.1; // kg
normal packs(mean, standard_deviation);

double max_weight = 3.1; // kg
cout << "Percentage of packs > " << max_weight << " is "
<< cdf(complement(packs, max_weight)) << endl; // P(X > 3.1)

double under_weight = 2.9;
cout <<"fraction of packs <= " << under_weight << " with a mean of " << mean 
  << " is " << cdf(complement(packs, under_weight)) << endl;
// fraction of packs <= 2.9 with a mean of 3 is 0.841345
// This is 0.84 - more than the target 0.95
// Want 95% to be over this weight, so what should we set the mean weight to be?
// KK StatCalc says:
double over_mean = 3.0664;
normal xpacks(over_mean, standard_deviation);
cout << "fraction of packs >= " << under_weight
<< " with a mean of " << xpacks.mean() 
  << " is " << cdf(complement(xpacks, under_weight)) << endl;
// fraction of packs >= 2.9 with a mean of 3.06449 is 0.950005
double under_fraction = 0.05;  // so 95% are above the minimum weight mean - sd = 2.9
double low_limit = standard_deviation;
double offset = mean - low_limit - quantile(packs, under_fraction);
double nominal_mean = mean + offset;

normal nominal_packs(nominal_mean, standard_deviation);
cout << "Setting the packer to " << nominal_mean << " will mean that "
  << "fraction of packs >= " << under_weight 
  << " is " << cdf(complement(nominal_packs, under_weight)) << endl;

/*`
Setting the packer to 3.06449 will mean that fraction of packs >= 2.9 is 0.95.

Setting the packer to 3.13263 will mean that fraction of packs >= 2.9 is 0.99,
but will more than double the mean loss from 0.0644 to 0.133.

Alternatively, we could invest in a better (more precise) packer with a lower standard deviation.

To estimate how much better (how much smaller standard deviation) it would have to be,
we need to get the 5% quantile to be located at the under_weight limit, 2.9
*/
double p = 0.05; // wanted p th quantile.
cout << "Quantile of " << p << " = " << quantile(packs, p)
  << ", mean = " << packs.mean() << ", sd = " << packs.standard_deviation() << endl; // 
/*`
Quantile of 0.05 = 2.83551, mean = 3, sd = 0.1

With the current packer (mean = 3, sd = 0.1), the 5% quantile is at 2.8551 kg,
a little below our target of 2.9 kg.
So we know that the standard deviation is going to have to be smaller.

Let's start by guessing that it (now 0.1) needs to be halved, to a standard deviation of 0.05
*/
normal pack05(mean, 0.05); 
cout << "Quantile of " << p << " = " << quantile(pack05, p) 
  << ", mean = " << pack05.mean() << ", sd = " << pack05.standard_deviation() << endl;

cout <<"Fraction of packs >= " << under_weight << " with a mean of " << mean 
  << " and standard deviation of " << pack05.standard_deviation()
  << " is " << cdf(complement(pack05, under_weight)) << endl;
// 
/*`
Fraction of packs >= 2.9 with a mean of 3 and standard deviation of 0.05 is 0.9772

So 0.05 was quite a good guess, but we are a little over the 2.9 target,
so the standard deviation could be a tiny bit more. So we could do some
more guessing to get closer, say by increasing to 0.06
*/

normal pack06(mean, 0.06); 
cout << "Quantile of " << p << " = " << quantile(pack06, p) 
  << ", mean = " << pack06.mean() << ", sd = " << pack06.standard_deviation() << endl;

cout <<"Fraction of packs >= " << under_weight << " with a mean of " << mean 
  << " and standard deviation of " << pack06.standard_deviation()
  << " is " << cdf(complement(pack06, under_weight)) << endl;
/*`
Fraction of packs >= 2.9 with a mean of 3 and standard deviation of 0.06 is 0.9522

Now we are getting really close, but to do the job properly,
we could use root finding method, for example the tools provided, and used elsewhere,
in the Math Toolkit, see
[link math_toolkit.toolkit.internals1.roots2  Root Finding Without Derivatives].

But in this normal distribution case, we could be even smarter and make a direct calculation.
*/
//] [/root_find2]

  }
  catch(const std::exception& e)
  { // Always useful to include try & catch blocks because default policies 
    // are to throw exceptions on arguments that cause errors like underflow, overflow. 
    // Lacking try & catch blocks, the program will abort without a message below,
    // which may give some helpful clues as to the cause of the exception.
    std::cout <<
      "\n""Message from thrown exception was:\n   " << e.what() << std::endl;
  }
  return 0;
}  // int main()

/*

Output is:



*/
