/*****************************************************************************/
/*                                                                           */
/*  Header: predicates.hpp                                                   */
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
#ifndef __STANN_GMST__PREDICATES__
#define __STANN_GMST__PREDICATES__

#include <vector>

/*! \file
  \brief Predicate definitions used in filter_kruskal.hpp
*/

template <typename Point>
class bccpPred
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef std::pair<size_type, size_type> Edge;
  typedef typename std::vector<WSP<Point> >::iterator WspIter;
  typedef std::pair<Edge, double> WEdge;

public:

  bccpPred()
  {
  };

  bool operator()(const WspIter &a, const WspIter &b)
  {
    return (a->get_bccp_dist() < b->get_bccp_dist());
  }
  bool operator()(const WSP<Point> &a, const WSP<Point> &b)
  {
  return (a.get_bccp_dist() < b.get_bccp_dist());
  };
  bool operator()(WEdge &a, WEdge &b)
  {
    return a.second < b.second;
  }
};

template<typename Point>
class calcBccpPred
{
public:
  double *minBccp;
  int THold;
  

  bool operator()(WSP<Point> &a)
  {
    if(a.size() < THold)
      {
	a.bccp();
	return true;
      }
    if(a.get_bccp_dist() < *minBccp)
      {
	if(a.get_bccp_dist() < *minBccp)
	  *minBccp = a.get_bccp_dist();
      }
    return false;
  }
};

template <typename Point>
class unionFindPred
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef std::pair<size_type, size_type> Edge;
  typedef typename std::vector<WSP<Point> >::iterator WspIter;
  typedef std::pair<Edge, double> WEdge;
  typedef comp_quad_tree_node<Point>* CqPtr;
  
public:
  int_partition &Uf;
  comp_quad_tree<Point> &cqtree;
  std::vector<Point> &Points;

  unionFindPred(int_partition &uf, comp_quad_tree<Point> &cq, std::vector<Point> &P) : Uf(uf), cqtree(cq), Points(P)
  {
  };
  
  bool operator()(WSP<Point> &a)
  {
    size_type first, second;
    Point *f,*s,*z;
    
    z = &(*(Points.begin()));    
    if(a.bccp_done())
      {
	f = (Point*) (a.first);
	s = (Point*) (a.second);
	first = f-z;
	second = s-z;
      }
    else
      {
	first = (((CqPtr) a.first)->low)-Points.begin();
	second =(((CqPtr) a.second)->low)-Points.begin();
      }
    return (Uf.find(first) != Uf.find(second));
  };

};

template <typename Point>
class boxLengthPred
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef std::pair<size_type, size_type> Edge;
  typedef typename std::vector<WSP<Point> >::iterator WspIter;
  typedef std::pair<Edge, double> WEdge;
public:
  bool operator()(const WSP<Point> &a, const WSP<Point> &b)
  {
    return (a.length() < b.length());
  }

  bool operator()(const WSP<Point> *a, const WSP<Point> *b)
  {
    return (a->length() < b->length());
  }
};
  
template <typename Point>
class boxDistPredMin
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef std::pair<size_type, size_type> Edge;
  typedef typename std::vector<WSP<Point> >::iterator WspIter;
  typedef typename std::vector<WSP<Point>*>::iterator WspPIter;
  typedef std::pair<Edge, double> WEdge;

public:
  
  //size_type pivot;
  double minBccp;

  boxDistPredMin()
  {
  };

  bool operator()(const WspIter &a, const WspIter &b)
  {
    double minBccp1 = dist_sq_between_quad_boxes((*a).first, (*a).second);
    double minBccp2 = dist_sq_between_quad_boxes((*b).first, (*b).second);
    return minBccp1 < minBccp2;
  };
  bool operator()(const WspIter *a, const WspIter *b)
  {
    double minBccp1 = dist_sq_between_quad_boxes((*a)->first, (*a)->second);
    double minBccp2 = dist_sq_between_quad_boxes((*b)->first, (*b)->second);
    return minBccp1 < minBccp2;
  };
  bool operator()(WSP<Point> &a)
  {
    return (a.get_bccp_dist() < minBccp);
  };

  bool operator()(WSP<Point> *a)
  {
    double maxBccp = max_dist_sq_between_quad_boxes(a->first, a->second);
    return (maxBccp < minBccp);
  };
};

template <typename Point>
class boxDistPredMax
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef std::pair<size_type, size_type> Edge;
  typedef typename std::vector<WSP<Point> >::iterator WspIter;
  typedef std::pair<Edge, double> WEdge;

public:
  
  //size_type pivot;
  double maxBccp;

  boxDistPredMax()
  {
  };
  
  bool operator()(WSP<Point> &a)
  {
    double minBccp = dist_sq_between_quad_boxes(a.first, a.second);
    return (minBccp < maxBccp);
  };

  bool operator()(WSP<Point> *a)
  {
    double minBccp = dist_sq_between_quad_boxes(a->first, a->second);
    return (minBccp < maxBccp);
  };
};

template <typename Point>
class bccpDistPred
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef std::pair<size_type, size_type> Edge;
  typedef typename std::vector<WSP<Point> >::iterator WspIter;
  typedef std::pair<Edge, double> WEdge;

public:
  
  double maxBccp;
  
  bccpDistPred()
  {
  };
  
  bool operator()(WSP<Point> &a)
  {
    return (a.bccp().first < maxBccp);
  };

  bool operator()(WSP<Point> *a)
  {
    return (a->bccp().first < maxBccp);
  };
};
#endif
