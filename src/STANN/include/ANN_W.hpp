/*****************************************************************************/
/*                                                                           */
/*  Header: ANN_W.hpp                                                        */
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

#ifndef __STANN_ANN_W__
#define __STANN_ANN_W__

#include <ANN.h>
#include <ANNx.h>
#include <ANNperf.h>
#include <vector>
#include <nnBase.hpp>
/*! 
  \file ANN_W.hpp
  \brief Implementation of wrapper for ANN library 
*/

/*! 
  \brief ANN_W is a wrapper class for David Mounts ANN library.  
  It uses ANN version 1.1.  
*/
/*! 
  The ANN algorithm is a wrapper class used to 
allow <A HREF="http://www.cs.umd.edu/~mount/ANN/">David Mount's nearest
neighbor library ANN</A> to share the same interface as the other NNCode
implementations. It does not allow dynamic insertions or deletions, nor does
it allow parallel queries. (These were limitations in the original ANN code).
It operates out-of-place, meaning the data set is copied into ANNs internal
data structure. It has a query time of O(ln(N)), a construction time of
O(N), and a space requirement of O(N). ANN is the fastest implementation
for non-parallel operation. If multiple queries can be made simultaneously,
SFCNN is faster given 2 or more processors. <BR> <BR>
To include ANN in an application, use ANN_W.hpp <BR>
In addition, ANN must be compiled via the make utility located in
the ANN directory. The path to the ANN header files is ANN/include/ANN, and 
the path to the ANN object files is ANN/src/ <BR>
For an example of how to compile an application that uses ANN,
see the \link page_compilesample Compiling a Sample Program\endlink section.
*/
template<typename Point, unsigned int Dim, typename NumType>
class ANN_W : public nnBase<Point, Dim, NumType>
{
public:

  /*!
    \param *P A pointer to an array of input points to be searched
    \param N The size of the input point set
  */
  ANN_W(Point *P, long int N);
  ~ANN_W();
  /*!
    Searches for the k nearest neighbors to the point q
    \param *q The query point
    \param k The number of neighbors to return
    \param nn_idx Vector of pointers to points, stores the solution
    \param eps Error tolerence, default of 0.0.  
    Answers will fit in a ball defined by radius*(1.0+eps)
    \param sl Unused, for backwards compatibility
  */
  void ksearch(Point q, unsigned int k, std::vector<long unsigned int> &nn_idx, float eps=0.0);
  void ksearch(Point q, unsigned int k, std::vector<long unsigned int> &nn_idx, std::vector<double> &dist, float eps=0.0);
private:
	
  ANNpointArray PA;
  ANNkd_tree *Tree;
  std::vector<Point *> points;
};

template<typename Point, unsigned int Dim, typename NumType>
ANN_W<Point, Dim, NumType>::ANN_W(Point *P, long int N)
{
  points.resize(N);
  PA = annAllocPts(N, Point::__DIM);
  for(long int i=0;i < N;++i)
    {
      points[i] = &(P[i]);
      for(unsigned long int j=0;j < Point::__DIM;++j)
	PA[i][j] = (float) P[i][j];
    }
	
  Tree = new ANNkd_tree(PA, N, Point::__DIM);
}

template<typename Point, unsigned int Dim, typename NumType>
ANN_W<Point, Dim, NumType>::~ANN_W()
{
  delete Tree;
}
template<typename Point, unsigned int Dim, typename NumType>
void ANN_W<Point, Dim, NumType>::ksearch(Point q, unsigned int k, 
			   std::vector<long unsigned int> &nn_idx,
			   float eps)
{
  std::vector<double> dist;
  ksearch(q,k,nn_idx, dist, eps);
}
template<typename Point, unsigned int Dim, typename NumType>
void ANN_W<Point, Dim, NumType>::ksearch(Point q, unsigned int k, 
			   std::vector<long unsigned int> &nn_idx,
			   std::vector<double> &dist, float eps)
{
  ANNpoint query = new ANNcoord[Point::__DIM];
  ANNidxArray NN_idx = new int[k];
  ANNdistArray Curr_dist = new ANNdist[k];
	
  for(unsigned long int i=0;i < Point::__DIM;++i)
    query[i] = (float)q[i];
		
  Tree->annPkSearch(query, k, NN_idx, Curr_dist, eps);
	
  nn_idx.resize(k);
  dist.resize(k);
  for(unsigned int i=0;i < k;++i)
    {
      nn_idx[i] = NN_idx[i];
      dist[i] = Curr_dist[i];
    }
  delete query;
  delete NN_idx;
  delete Curr_dist;
}

#endif
