// Copyright Paul Bristow 2007.
// Copyright John Maddock 2006.

// Use, modification and distribution are subject to the
// Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

// test_constants.cpp

#include <boost/math/concepts/real_concept.hpp> // for real_concept
#include <boost/test/included/test_exec_monitor.hpp> // Boost.Test
#include <boost/test/floating_point_comparison.hpp>

#include <boost/math/constants/constants.hpp>
#include <boost/math/tools/test.hpp> 

template <class RealType>
void test_spots(RealType)
{
   // Basic sanity checks for constants.

   RealType tolerance = boost::math::tools::epsilon<RealType>() * 2;  // double
   std::cout << "Tolerance for type " << typeid(RealType).name()  << " is " << tolerance << "." << std::endl;

   using namespace boost::math::constants;
   using namespace std; // Help ADL of std exp, log...
   using std::exp;

   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(3.14159265358979323846264338327950288419716939937510L), pi<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(sqrt(3.14159265358979323846264338327950288419716939937510L)), root_pi<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(sqrt(3.14159265358979323846264338327950288419716939937510L/2)), root_half_pi<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(sqrt(3.14159265358979323846264338327950288419716939937510L * 2)), root_two_pi<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(sqrt(log(4.0L))), root_ln_four<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(2.71828182845904523536028747135266249775724709369995L), e<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(0.5), half<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(0.57721566490153286060651209008240243104259335L), euler<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(sqrt(2.0L)), root_two<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(log(2.0L)), ln_two<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(log(log(2.0L))), ln_ln_two<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(1)/3, third<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(2)/3, twothirds<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(0.14159265358979323846264338327950288419716939937510L), pi_minus_three<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(4. - 3.14159265358979323846264338327950288419716939937510L), four_minus_pi<RealType>(), tolerance); 
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(pow((4 - 3.14159265358979323846264338327950288419716939937510L), 1.5L)), pow23_four_minus_pi<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(exp(-0.5L)), exp_minus_half<RealType>(), tolerance); 
#else
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(pow((4 - 3.14159265358979323846264338327950288419716939937510), 1.5)), pow23_four_minus_pi<RealType>(), tolerance); 
   BOOST_CHECK_CLOSE_FRACTION(static_cast<RealType>(exp(-0.5)), exp_minus_half<RealType>(), tolerance); 
#endif

} // template <class RealType>void test_spots(RealType)

int test_main(int, char* [])
{
   // Basic sanity-check spot values.

   // (Parameter value, arbitrarily zero, only communicates the floating point type).
   test_spots(0.0F); // Test float. OK at decdigits = 0 tolerance = 0.0001 %
   test_spots(0.0); // Test double. OK at decdigits 7, tolerance = 1e07 %
#ifndef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
   test_spots(0.0L); // Test long double.
#if !BOOST_WORKAROUND(__BORLANDC__, BOOST_TESTED_AT(0x0582))
   test_spots(boost::math::concepts::real_concept(0.)); // Test real concept.
#endif
#else
  std::cout << "<note>The long double tests have been disabled on this platform "
    "either because the long double overloads of the usual math functions are "
    "not available at all, or because they are too inaccurate for these tests "
    "to pass.</note>" << std::cout;
#endif

  return 0;
} // int test_main(int, char* [])

/*

Output:

Autorun "i:\boost-06-05-03-1300\libs\math\test\Math_test\debug\test_constants.exe"
Running 1 test case...
*** No errors detected

*/






