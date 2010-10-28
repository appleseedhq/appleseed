/*****************************************************************************/
/*                                                                           */
/*  Header: nnBase.hpp                                                       */
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

#ifndef __STANN_NN_BASE_CLASS__
#define __STANN_NN_BASE_CLASS__

#include <vector>

/*! \file nnBase.hpp
\brief Contains the base class for NN algorithms */

template <typename Point, unsigned int Dim, typename NumType>
class nnBase
{
public:
  virtual void ksearch(Point, unsigned int, std::vector<long unsigned int>&, float=0)
  {
  };
  virtual void ksearch(Point, unsigned int, std::vector<long unsigned int>&, std::vector<double>&, float=0)
  {
  };
  virtual ~nnBase(){};
};
#endif
