/*****************************************************************************/
/*                                                                           */
/*  Header: rand.hpp                                                         */
/*                                                                           */
/*  Accompanies STANN Version 0.71 B                                         */
/*  Dec 07, 2009                                                             */
/*                                                                           */
/*  Copyright 2007, 2008                                                     */
/*  Michael Connor and Piyush Kumar                                          */
/*  Florida State University                                                 */
/*  Tallahassee FL, 32306-4532                                               */
/*                                                                           */
/*****************************************************************************/



#ifndef __STANN_RAND__
#define __STANN_RAND__
/*! \file rand.hpp
    \brief Replacements for srand48 and drand48
    
    The standard c++ does not have srand48 and drand48 built into it.
    this header replaces
    them using the standard C++ calls available for random numbers.
*/

#include <cstdlib>

static double __drand48__() 
{
  /* We don't have drand48.  Use rand() to get the bits.  We call
     rand() three times since RAND_MAX it at least 16 bits. */
  double f = 1.0 / (RAND_MAX + 1.0);
  double x = std::rand();
  x = x * f + std::rand();
  x = x * f + std::rand();
  return x * f;
}

static void  __srand48__(unsigned int seedval) 
{
  std::srand(seedval);
  return;
}
  

#endif
