/*****************************************************************************/
/*                                                                           */
/*  Header: assert.hpp                                                       */
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


#ifndef __STANN_ASSERT__
#define __STANN_ASSERT__

#include <iostream>
#include <cstring>
#include <cstdlib>
/*! 
  \file
  \brief Implementation of assert functions
  This file contains an assert function implementation
*/

/*! 
  \brief Assert function
  This function prints error messages along with file and line numbers
  \param b bool to be checked
  \param desc Error message
  \param line Line number
  \param file File name 
  \param exp  Expression tested
  \return True is there is no error
*/
inline bool MyAssertFunction( bool b, 
                       const char* desc, 
                       int line, 
                       const char* file, 
                       const char *exp )
{
	if (b) return true;
	std::cerr << "\n\nAssertion Failure\n";
	std::cerr << "Expression  : " << exp  << std::endl;
	std::cerr << "Description : " << desc << std::endl;
	std::cerr << "Filename    : " << file << std::endl;
	std::cerr << "Line No     : " << line << std::endl;
	std::exit(1);
	return false;
}

// NDEBUG is standard C++ used with assert.h
#if defined( NDEBUG )
#define Assert( exp, description ) MyAssertFunction( (bool)(exp), description, __LINE__, __FILE__, #exp )
#else
#define Assert( exp, description )
#endif


#endif
