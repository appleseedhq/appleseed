/*****************************************************************************/
/*                                                                           */
/*  Header: int_partition.hpp                                                */
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

#ifndef __STANN_INT_PARTITION__
#define __STANN_INT_PARTITION__

#include <iostream>
#include <vector>

/*! \file
  \brief Contains union find data structure implementation
*/

/*! Int Partition Class
  \brief Implementation of a union find data structure
*/
class int_partition {
  
  std :: vector<long int> S;
  long int size;
public:
  /*! Constructor
    \brief Default Constructor 
  */
  int_partition(){}

  /*! Initialize function
    \brief Initializes the Union find data structure
    \param n Number of points to be included in the union find
  */
  void initialize(long int n)
  {
    S.resize(n);
#pragma omp parallel for schedule(static)
    for(int i=0;i < (int) S.size();++i)
      S[i]=-1;
    size=n;
  }
  /*! Destructor
    \brief Default Destructor
  */
  ~int_partition()
  { 
  }
  /*! Find
    \brief Finds the root ancestor of a point
    \param x Index of the query point
  */
  long int find(long int x) 
  {
    long int y = S[x];
    
    if (y < 0) 
      return x;
    else
      return (S[x] = find(y));
  }

  /*! Union Blocks
    \brief Joins two blocks of points, and compresses the find path
    \param x Index of first point to be joined
    \param y Index of second point to be joined
  */
  void union_blocks(long int x, long int y)
  { 
    x = find(x);
    y = find(y);
    if (S[y] < S[x])
    S[x] = y;
    else
    {
    if (S[x] == S[y])
      S[x] = S[x] - 1;
    S[y] = x;
    }
  }
};

#endif
