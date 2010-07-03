//  Copyright John Maddock 2007.
//  Use, modification and distribution are subject to the
//  Boost Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

// Note that this file contains quickbook mark-up as well as code
// and comments, don't change any of the special comment mark-ups!

//[policy_eg_10

/*`

To understand how the rounding policies for 
the discrete distributions can be used, we'll
use the 50-sample binomial distribution with a 
success fraction of 0.5 once again, and calculate
all the possible quantiles at 0.05 and 0.95.

Begin by including the needed headers:

*/

#include <iostream>
#include <boost/math/distributions/binomial.hpp>

/*`

Next we'll bring the needed declarations into scope, and
define distribution types for all the available rounding policies:

*/

using namespace boost::math::policies;
using namespace boost::math;

typedef binomial_distribution<
            double, 
            policy<discrete_quantile<integer_round_outwards> > > 
        binom_round_outwards;

typedef binomial_distribution<
            double, 
            policy<discrete_quantile<integer_round_inwards> > > 
        binom_round_inwards;

typedef binomial_distribution<
            double, 
            policy<discrete_quantile<integer_round_down> > > 
        binom_round_down;

typedef binomial_distribution<
            double, 
            policy<discrete_quantile<integer_round_up> > > 
        binom_round_up;

typedef binomial_distribution<
            double, 
            policy<discrete_quantile<integer_round_nearest> > > 
        binom_round_nearest;

typedef binomial_distribution<
            double, 
            policy<discrete_quantile<real> > > 
        binom_real_quantile;

/*`

Now let's set to work calling those quantiles:

*/

int main()
{
   std::cout << 
      "Testing rounding policies for a 50 sample binomial distribution,\n"
      "with a success fraction of 0.5.\n\n"
      "Lower quantiles are calculated at p = 0.05\n\n"
      "Upper quantiles at p = 0.95.\n\n";

   std::cout << std::setw(25) << std::right
      << "Policy"<< std::setw(18) << std::right 
      << "Lower Quantile" << std::setw(18) << std::right 
      << "Upper Quantile" << std::endl;
   
   // Test integer_round_outwards:
   std::cout << std::setw(25) << std::right
      << "integer_round_outwards"
      << std::setw(18) << std::right
      << quantile(binom_round_outwards(50, 0.5), 0.05)
      << std::setw(18) << std::right
      << quantile(binom_round_outwards(50, 0.5), 0.95) 
      << std::endl;
   
   // Test integer_round_inwards:
   std::cout << std::setw(25) << std::right
      << "integer_round_inwards"
      << std::setw(18) << std::right
      << quantile(binom_round_inwards(50, 0.5), 0.05)
      << std::setw(18) << std::right
      << quantile(binom_round_inwards(50, 0.5), 0.95) 
      << std::endl;
   
   // Test integer_round_down:
   std::cout << std::setw(25) << std::right
      << "integer_round_down"
      << std::setw(18) << std::right
      << quantile(binom_round_down(50, 0.5), 0.05)
      << std::setw(18) << std::right
      << quantile(binom_round_down(50, 0.5), 0.95) 
      << std::endl;
   
   // Test integer_round_up:
   std::cout << std::setw(25) << std::right
      << "integer_round_up"
      << std::setw(18) << std::right
      << quantile(binom_round_up(50, 0.5), 0.05)
      << std::setw(18) << std::right
      << quantile(binom_round_up(50, 0.5), 0.95) 
      << std::endl;
   
   // Test integer_round_nearest:
   std::cout << std::setw(25) << std::right
      << "integer_round_nearest"
      << std::setw(18) << std::right
      << quantile(binom_round_nearest(50, 0.5), 0.05)
      << std::setw(18) << std::right
      << quantile(binom_round_nearest(50, 0.5), 0.95) 
      << std::endl;
   
   // Test real:
   std::cout << std::setw(25) << std::right
      << "real"
      << std::setw(18) << std::right
      << quantile(binom_real_quantile(50, 0.5), 0.05)
      << std::setw(18) << std::right
      << quantile(binom_real_quantile(50, 0.5), 0.95) 
      << std::endl;
}

/*`

Which produces the program output:

[pre
Testing rounding policies for a 50 sample binomial distribution,
with a success fraction of 0.5.

Lower quantiles are calculated at p = 0.05

Upper quantiles at p = 0.95.

Testing rounding policies for a 50 sample binomial distribution,
with a success fraction of 0.5.

Lower quantiles are calculated at p = 0.05

Upper quantiles at p = 0.95.

                   Policy    Lower Quantile    Upper Quantile
   integer_round_outwards                18                31
    integer_round_inwards                19                30
       integer_round_down                18                30
         integer_round_up                19                31
    integer_round_nearest                19                30
                     real            18.701            30.299
]

*/

//] ends quickbook import

