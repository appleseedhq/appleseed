/*****************************************************************************/
/*                                                                           */
/*  Header: sfcnn_ip.hpp                                                     */
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


#ifndef __SFCNN_IP__
#define __SFCNN_IP__
#include <cstdlib>
#include <cmath>
#include <climits>
#include <vector>
#include <queue>
#include <algorithm>

#include <pair_iter.hpp>
#include <qknn.hpp>
#include <zorder_lt.hpp>
#include <bsearch.hpp>
/*! 
  \file
  \brief Space filling curve nearest neighbor search
  This file contains the implementation of a space filling curve
  nearest neighbor search data structure
*/

using namespace std;


///////////////////////////////////////////////////////
// Squared Distance of d-dimensional point
///////////////////////////////////////////////////////
template< typename Point, unsigned I > class Distance
{
public:
   static inline double eval( const Point &p, 
                              const Point &q )
   {
     double sum = ( (double) p[I] - (double) q[I] );
     sum = sum * sum;
     return sum + Distance<Point, I-1 >::eval( p,q );
   }
};


// Partial Template Specialization
template <typename Point> class Distance<Point, 0>
{
public:
   static inline double eval( const Point &p, 
                               const Point &q )
   {
     double sum = ((double) p[0] - (double) q[0]);
     return sum * sum;
   }
};


/*! 
  \brief An in-place  space filling curve nearest neighbor class.  
  
  This is the workhorse class for the sfcnn_ip class.
  The In place Space Filling Curve Nearest Neighbor (SFCNN_IP) 
  algorithm sorts the input data set into 2-order Morton ordering. 
  Nearest neighbors are then calculated based on that curve. 
  The algorithm has a runtime of   O(ln(N)) and  
  a construction time of O(Nlog(N)).  This version operates
  on user data instead of a copy.
  The query functions of the algorithm are thread-safe.
*/
template <typename Point, unsigned DIM, typename CType>
class sfcnn_ip 
{
public:
  /*!
    \brief Constructor
    
    Initializes an sfcnn_ip object.
    \param p Pointer to the input data set
    \param N Size of the input data set
    \param numthreads (Optional) Number of threads to use in construction (1)
    \param sorted (Optional) Non-zero indicates data is sorted in Z-order already (0)
  */
  sfcnn_ip(Point *p, long int N, int numthreads, int sorted);
  /*!
    \brief Destructor
  */
  ~sfcnn_ip();

  /*!
    \brief Nearest Neighbor search function
    
    Searches for the k nearest neighbors to the point q.  The answer
    vector returned will contain the indexes to the answer points
    This function is thread-safe.  
    \param q The query point
    \param k The number of neighbors to return
    \param nn_idx Answer vector
    \param eps Error tolerence, default of 0.0.
    \param sl Unused, for backwards compatibility
  */
  void ksearch(Point q, unsigned int k, vector<long unsigned int> &nn_idx, float eps = 0);
  void ksearch(Point q, unsigned int k, vector<long unsigned int> &nn_idx, vector<double> &dist, float eps = 0);

private:
  
  double sqr_dist(const Point &p, const Point &q)
  {
    return Distance<Point, DIM-1>::eval(p, q);
  };
  
  Point *points;
  long int size;
  zorder_lt<Point> lt;



  float eps;
  CType max, min;

  void compute_bounding_box(Point q, Point &q1, Point &q2, double r);
  void sfcnn_ip_init(Point *p, long int N, int num_threads, int sorted);

  void ksearch_common(Point q, unsigned int k, long unsigned int j, qknn &que, float Eps);	
  
  inline void recurse(int s, int n, Point q, 
		      qknn &ans, Point &q1, Point &q2, int bb, int bt);
	
};

template<typename Point, unsigned Dim, typename CType>
sfcnn_ip<Point, Dim, CType>::sfcnn_ip(Point *p, long int N, int numthreads=1, int sorted=0)
{
  sfcnn_ip_init(p,N,numthreads, sorted);
}

template<typename Point, unsigned Dim, typename CType>
void sfcnn_ip<Point, Dim, CType>::sfcnn_ip_init(Point *Points, long int N, int num_threads, int sorted)
{
  max = (numeric_limits<CType>::max)();
  min = (numeric_limits<CType>::min)();
  if(min != 0) min = -(numeric_limits<CType>::max)();

  if(N == 0)
    {
      cerr << "Error:  Input point list has size 0" << endl;
      exit(1);
    }
  points = Points;
  size = N;
  if(sorted == 0)
    sort(points, &points[N], lt);
 }

template<typename Point, unsigned Dim, typename CType>
sfcnn_ip<Point, Dim, CType>::~sfcnn_ip()
{
}

template<typename Point, unsigned Dim, typename CType>
void sfcnn_ip<Point, Dim, CType>::recurse(int s,     // Starting index
			      int n,     // Number of points
			      Point q,  // Query point
			      qknn &ans, // Answer que
			      Point &bound_box_lower_corner,
			      Point &bound_box_upper_corner,
			      int initial_scan_lower_range,
			      int initial_scan_upper_range)
{
  if(n < 4)
    {
      if(n == 0) return;
      
      bool update=false;
      for(int i=0;i < n;++i)
	{
	  if((s+i >= initial_scan_lower_range) 
	     && (s+i < initial_scan_upper_range))
	    continue;
	  update = ans.update(sqr_dist(points[s+i], q), s+i) || update;
	}
      if(update)
	compute_bounding_box(q, bound_box_lower_corner, bound_box_upper_corner, sqrt(ans.topdist()));
      return;
    }
  
  if((s+n/2 >= initial_scan_lower_range) && (s+n/2 < initial_scan_upper_range))
    {
    }
  else if(ans.update(sqr_dist(points[s+n/2], q), s+n/2))
    compute_bounding_box(q, bound_box_lower_corner, bound_box_upper_corner, sqrt(ans.topdist()));
  
  double dsqb = lt.dist_sq_to_quad_box(q,points[s], points[s+n-1]);
  
  //cout << "dsqb: " << dsqb << endl;
  //cout << "dist: " << ans.topdist() << endl;
  //cout << "p1  : " << points[s] << endl;
  //cout << "p2  : " << points[s+n-1] << endl;
  if(dsqb > ans.topdist())
    return;
  
	
  if(lt(q,points[s+n/2]))
    {
      //search_queue.push(pair<int, int>(s, n/2));
      recurse(s, n/2, q, ans, 
	      bound_box_lower_corner, 
	      bound_box_upper_corner, 
	      initial_scan_lower_range, 
	      initial_scan_upper_range);
      if(lt(points[s+n/2],bound_box_upper_corner))
	//search_queue.push(pair<int, int>(s+n/2+1, n-n/2-1));
	recurse(s+n/2+1,n-n/2-1, q, ans, 
		bound_box_lower_corner, 
		bound_box_upper_corner, 
		initial_scan_lower_range, 
		initial_scan_upper_range);
    }
  else
    {
      recurse(s+n/2+1, n-n/2-1, q, ans, 
	      bound_box_lower_corner, 
	      bound_box_upper_corner, 
	      initial_scan_lower_range, 
	      initial_scan_upper_range);
      //search_queue.push(pair<int, int>(s+n/2+1, n-n/2-1));
      if(lt(bound_box_lower_corner,points[s+n/2]))
	//search_queue.push(pair<int, int>(s, n/2));
	recurse(s, n/2, q, ans, 
		bound_box_lower_corner, 
		bound_box_upper_corner, 
		initial_scan_lower_range, 
		initial_scan_upper_range);
    }
}

template<typename Point, unsigned Dim, typename CType>
void sfcnn_ip<Point, Dim, CType>::compute_bounding_box(Point q, Point &q1, Point &q2, double R)
{
  CType radius;
  radius = (CType) ceil(R);
  for(unsigned int i=0;i<Dim;++i)
    {
      if(q[i] < (min+radius)) q1[i] = min;
      else q1[i] = q[i]-radius;

      if(q[i] > (max-radius)) q2[i] = max;
      else q2[i] = q[i]+radius;
    }
}

template<typename Point, unsigned Dim, typename CType>
void sfcnn_ip<Point, Dim, CType>::ksearch_common(Point q, unsigned int k, long unsigned int query_point_index, qknn &que, float Eps)
{
  Point bound_box_lower_corner, bound_box_upper_corner;
  Point low, high;
  
  que.set_size(k);
  eps=1.0+Eps;
  if(query_point_index >= (k)) query_point_index -= (k);
  else query_point_index=0;
  
  int initial_scan_upper_range=query_point_index+2*k+1;
  if(initial_scan_upper_range > (int)size)
    initial_scan_upper_range = size;
  
  low = points[query_point_index];
  high = points[initial_scan_upper_range-1];
  for(int i=query_point_index;i<initial_scan_upper_range;++i)
    {
      que.update(sqr_dist(points[i], q), i);
    }
  compute_bounding_box(q, bound_box_lower_corner, bound_box_upper_corner, sqrt(que.topdist()));
  
  if(lt(bound_box_upper_corner, high) && lt(low,bound_box_lower_corner))
    {
      //cout << "Inital search break!" << endl;
      //cout << "Bb1: " << bound_box_lower_corner << endl;
      //cout << "Bb2: " << bound_box_upper_corner << endl;
      return;
    }
  
  //Recurse through the entire set
  recurse(0, size, q, que, 
	  bound_box_lower_corner, 
	  bound_box_upper_corner,
	  query_point_index,
	  initial_scan_upper_range);
  //cout << "Bb1: " << bound_box_lower_corner << endl;
  //cout << "Bb2: " << bound_box_upper_corner << endl;
}

template<typename Point, unsigned Dim, typename CType>
void sfcnn_ip<Point, Dim, CType>::ksearch(Point q, unsigned int k, 
			      vector<long unsigned int> &nn_idx, float Eps)
{
  long unsigned int query_point_index;  
  qknn que;
  query_point_index = BinarySearch(points, size, q, lt);
  ksearch_common(q, k, query_point_index, que, Eps);
  que.answer(nn_idx);
}

template<typename Point, unsigned Dim, typename CType>
void sfcnn_ip<Point, Dim, CType>::ksearch(Point q, unsigned int k, 
				vector<long unsigned int> &nn_idx, vector<double> &dist, float Eps)
{
  long unsigned int query_point_index;  
  qknn que;
  query_point_index = BinarySearch(points, size, q, lt);
  ksearch_common(q, k, query_point_index, que, Eps);
  que.answer(nn_idx, dist);
}

#endif
