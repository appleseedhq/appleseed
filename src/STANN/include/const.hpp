/*****************************************************************************/
/*                                                                           */
/*  Header: const.hpp                                                        */
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


#ifndef __STANN_CONST__
#define __STANN_CONST__

#include <string>

/*! 
  \file const.hpp
  \brief Contains constants
*/
const int SFCNN       = 1;
const int ANN	      = 2;
const int MAX_ALG     = 3;

const std::string ALG_NAME[] = {
		"NULL", "SFCNN" , "ANN", 
		"MAX"
};

#endif
