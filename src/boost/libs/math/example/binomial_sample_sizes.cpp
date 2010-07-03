// (C) Copyright John Maddock 2006
// Use, modification and distribution are subject to the
// Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifdef _MSC_VER
#  pragma warning(disable: 4512) // assignment operator could not be generated.
#  pragma warning(disable: 4510) // default constructor could not be generated.
#  pragma warning(disable: 4610) // can never be instantiated - user defined constructor required.
#endif

#include <iostream>
#include <iomanip>
#include <boost/math/distributions/binomial.hpp>

void find_max_sample_size(double p, unsigned successes)
{
   //
   // p         = success ratio.
   // successes = Total number of observed successes.
   //
   // Calculate how many trials we can have to ensure the
   // maximum number of successes does not exceed "successes".
   // A typical use would be failure analysis, where you want
   // zero or fewer "successes" with some probability.
   //
   using namespace std;
   using namespace boost::math;

   // Print out general info:
   cout <<
      "________________________\n"
      "Maximum Number of Trials\n"
      "________________________\n\n";
   cout << setprecision(7);
   cout << setw(40) << left << "Success ratio" << "=  " << p << "\n";
   cout << setw(40) << left << "Maximum Number of \"successes\" permitted" << "=  " << successes << "\n";
   //
   // Define a table of confidence intervals:
   //
   double alpha[] = { 0.5, 0.25, 0.1, 0.05, 0.01, 0.001, 0.0001, 0.00001 };
   //
   // Print table header:
   //
   cout << "\n\n"
           "____________________________\n"
           "Confidence        Max Number\n" 
           " Value (%)        Of Trials \n"
           "____________________________\n";
   //
   // Now print out the data for the table rows.
   //
   for(unsigned i = 0; i < sizeof(alpha)/sizeof(alpha[0]); ++i)
   {
      // Confidence value:
      cout << fixed << setprecision(3) << setw(10) << right << 100 * (1-alpha[i]);
      // calculate trials:
      double t = binomial_distribution<>::find_maximum_number_of_trials(successes, p, alpha[i]);
      t = floor(t);
      // Print Trials:
      cout << fixed << setprecision(0) << setw(15) << right << t << endl;
   }
   cout << endl;
}

int main()
{
   find_max_sample_size(1.0/1000, 0);
   find_max_sample_size(1.0/10000, 0);
   find_max_sample_size(1.0/100000, 0);
   find_max_sample_size(1.0/1000000, 0);

   return 0;
}

