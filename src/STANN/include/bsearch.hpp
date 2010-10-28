/*****************************************************************************/
/*                                                                           */
/*  Header: bsearch.hpp                                                      */
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

#ifndef __STANN_BSEARCH__
#define __STANN_BSEARCH__

#include <vector>
#include <algorithm>
#include <zorder_lt.hpp>

/*! \file
  \brief Binary search functions
  This file contains binary search functions for z-order operations.
*/

//! Binary search function
/*!
  This function executes a binary search on a vector of pointers to points
  \param A Vector of pointers to search
  \param *q Pointer to query point
  \param lt A less_than comparetor
  \return If found: index of point. Otherwise: index of first smaller point
*/
template<typename Point>
long int BinarySearch(std::vector<Point *> &A, Point *q, zorder_lt<Point> lt) 
{
	long int low = 0;
	long int high = A.size()-1;
	long int middle;

	while(low <= high)
	{
	  middle = (low+high)/2;
	  if(q == A[middle])
	    return middle;
	  else if(lt(q, A[middle]))
	    high = middle-1;
	  else
	    low = middle+1;
	}
	return middle;
}

template<typename Point>
unsigned long int BinSearch(std::vector<Point> &A, Point &q, zorder_lt<Point> &lt)
{
  typedef typename std::vector<Point>::iterator MyIt;
  typedef typename std::vector<Point>::size_type size_type;

  MyIt I = upper_bound(A.begin(), A.end(), q, lt);

  return I - A.begin();
}
//! A Binary Search function
/*!
  This function conducts a binary search for two points at the same time.
  \param A Reference to the vector of points being searched
  \param q1 pointer to first point to be searched for
  \param q2 pointer to second point to be searched for
  \param lt less than comparetor
  \param p1 reference to return value for q1 location
  \param p2 reference to return value for q2 location
*/

template<typename Point>
void PairBinarySearch(std::vector<Point> &A, Point q1, 
		      Point q2, zorder_lt<Point> lt, int &p1, int &p2) 
{
  int low_q1=0;
  int low_q2=0;
  int high_q1 = A.size()-1;
  int high_q2 = A.size()-1;
  int middle = 0;
  int middle_store;
  
  p1 = -2;
  p2 = -2;
  
  while((low_q1 == low_q2) && (high_q1 == high_q2) && (p1 == -2) && (p2 == -2))
    {
      middle = (low_q1+high_q1)/2;
      if(q1 == A[middle])
	p1 = middle;
      else if(lt(q1, A[middle]))
	high_q1 = middle-1;
      else
	low_q1 = middle+1;
      if(q2 == A[middle])
	p2 = middle;
      else if(lt(q2, A[middle]))
	high_q2 = middle-1;
      else
	low_q2 = middle+1;
    }
  middle_store = middle;
  while(low_q1 <= high_q1)
    {
      middle = (low_q1+high_q1)/2;
      if(q1 == A[middle])
	break;
      else if(lt(q1, A[middle]))
	high_q1 = middle-1;
      else
	low_q1 = middle+1;
    }
  p1 = middle;
  middle = middle_store;
  while(low_q2 <= high_q2)
    {
      middle = (low_q2+high_q2)/2;
      if(q2 == A[middle])
	break;
      else if(lt(q2, A[middle]))
	high_q2 = middle-1;
      else
	low_q2 = middle+1;
    }
  p2 = middle;
}  

//! A binary search Function.
/*
  This function executes a binary search on a vector of points
  \param A Vector of points to search
  \param q Query point
  \param lt A less_than comparetor
  \return If found: index of point. Otherwise: index of first smaller point
*/
template<typename Point>
long int BinarySearch(std::vector<Point> &A, Point q, zorder_lt<Point> lt) 
{
	long int low = 0;
	long int high = A.size()-1;
	long int middle = 0;

	while(low <= high)
	{
		middle = (low+high)/2;
		if(q == A[middle])
			return middle;
		else if(lt(q, A[middle]))
			high = middle-1;
		else
			low = middle+1;
	}
	return middle;
}

//! A binary search Function.
/*
  This function executes a binary search on an array of points
  \param A pointer to head of array
  \param size size of array
  \param q Query point
  \param lt A less_than comparetor
  \return If found: index of point. Otherwise: index of first smaller point
*/
template<typename Point>
long int BinarySearch(Point *A, long int size, Point q, zorder_lt<Point> lt) 
{
	long int low = 0;
	long int high = size-1;
	long int middle = 0;

	while(low <= high)
	{
		middle = (low+high)/2;
		if(q == A[middle])
			return middle;
		else if(lt(q, A[middle]))
			high = middle-1;
		else
			low = middle+1;
	}
	return middle;
}

template<typename Point>
class BinaryShortSearch
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef typename std::vector<Point>::iterator MyIt;
  typedef typename std::vector<MyIt>::iterator MyPIt;
public:
  std::vector<MyIt> samples;
  zorder_lt<Point> lt;
  
  BinaryShortSearch()
  {
  }

  void init(std::vector<Point> &points, int sample_size=128)
  {
    samples.resize(sample_size);
    srand48(time(0));
    
    for(int i=0;i < sample_size;++i)
      {
	size_type s = drand48() * (double) points.size();
	samples[i] = points.begin() + s;
      }
    sort(samples.begin(), samples.end(), lt);
  }

  unsigned long int BinSearch(std::vector<Point> &A, Point &q, zorder_lt<Point> LT)
  {
    MyPIt U = upper_bound(samples.begin(), samples.end(), q, LT);

    MyIt LI, UI;
    if(U == samples.begin()) LI = A.begin();
    else LI = *(U-1); 
    if(U == samples.end()) UI = A.end();
    else UI = *U;

    MyIt I = upper_bound(LI, UI, q, LT);
    return  I-A.begin();
  }
};
#endif
