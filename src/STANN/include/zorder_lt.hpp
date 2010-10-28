/*****************************************************************************/
/*                                                                           */
/*  Header: zorder_lt.hpp                                                    */
/*                                                                           */
/*  Accompanies STANN Version 0.6 Beta                                       */
/*  Aug 27, 2009                                                             */
/*                                                                           */
/*  Copyright 2007, 2008                                                     */
/*  Michael Connor and Piyush Kumar                                          */
/*  Florida State University                                                 */
/*  Tallahassee FL, 32306-4532                                               */
/*                                                                           */
/*****************************************************************************/



#ifndef __STANN_ZORDER_LT__
#define __STANN_ZORDER_LT__

#include <dpoint.hpp>
#include <cmath>
#include <climits>
#include <limits>
#include <iostream>
#include <vector>
#include <zorder_type_traits.hpp>
#include <sep_float.hpp>
#include <pair_iter.hpp>
/*! \file
  \brief Contains implementation of various z-order functions for defined types
*/

template<typename Point>
class zorder_lt;
template<typename Point, typename CType, typename sign_trait, typename integral_trait, typename sep_trait>
class zorder_lt_worker;

//Z Order spec for unsigned integral types
/*! Z-Order work class (Unsigned Integral)
  \brief Z-Order work class specialized for unsigned integer types

  This class computes Z-order less than operations and the distance
  to a quadtree box for points with coordinate types that are
  unsigned integers (unsigned short, unsigned int, unsigned long).
  It is designed to be called by the zorder_lt class.
*/
template<typename Point, typename CType>
class zorder_lt_worker <Point, CType, zorder_f, zorder_t, zorder_f>
{
public:
  /*! 
    Constructor
    \brief Default constructor
   */
  zorder_lt_worker()
  {
    offset=0;
  }
  /*! Destructor
    \brief Default destructor
  */
  ~zorder_lt_worker(){;}
    /*! Function Object Operator
    \brief Calls less than operator

    This function determines whether one point preceeds the other in
    a Z-ordering.
    \param p First point
    \param q Second point
    \return True if p comes before q in the Z-ordering of the points
  */
  bool operator()(const Point &p, const Point &q)
  {
    return lt_func(p,q);
  }
  void set_offset(CType off)
  {
    offset=off;
  }
    /*! Distance (Squared) to Quadtree Box
    \brief Computes the distance from a query point to the smallest quadtree 
    box containing two other points.

    This function first computes the minimum enclosing quadtree box for two 
    points, then computes the square distance from the query point to that 
    quadtree box.  If the query point lies within the quadtree box, the distance
    returned will be 0.
    \param q Query point
    \param p1 First quadtree box defining point
    \param p2 Second quadtree box defining point
    \return Squared distance from q to quadtree box.  0 if q lies within box
  */
  double dist_sq_to_quad_box(const Point &q, const Point &p1, const Point &p2)
  {
    unsigned int j;
    int i;
    CType x,y;
    double z;
    z=0;
    x=0;
    for(j=0;j < Point::__DIM;++j)
      {
	y = (CType) (p1[j]^p2[j]);
	if(less_msb(x, y)) 
	  {
	    x = y;
	  }
      }
    frexp((float)x, &i);
    for(j=0;j < Point::__DIM;++j)
      {
	x = (CType) ((((p1)[j])>>i)<<i);
	y = (CType) ((int) x+(1 << i));
	if(q[j] < x) 
	  z+= pow(((double) q[j]-(double) x), 2.0);
	else if(q[j] > y) 
	  z+= pow(((double) q[j]-(double) y), 2.0);
      }
    return z;
  }
  
/*! Quadtree Box Length
    \brief Computes the side length of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \return The side length of the smallest quadtree box containing p1 and p2
  */
  double quad_box_length(const Point &p1, const Point &p2)
  {
    unsigned int j;
    int i;
    CType x,y;
    double length;

    x = 0;
    if(p1 == p2)
      return 0;
    for(j=0;j < Point::__DIM;++j)
      {
	if((p1[j] < 0) != (p2[j] < 0))
	  {
	    return 0;
	  }
	y = p1[j]^p2[j];
	if(less_msb(x,y)) x=y;
      }
    length = pow((CType) 2.0,x);
    return length;
  };
  /*! Minimum Enclosing Quadtree Box
    \brief Computes the lower and upper corners of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \param lcorner Return value, lower corner
    \param ucorner Return value, upper corner
  */
  void min_quad_box(const Point &p1, const Point &p2, Point &lcorner, Point &ucorner)
  {
    double length = quad_box_length(p1, p2);
    if(length==0)
      {
	lcorner=p1;
	ucorner=p1;
	return;
      }
    else if(length == (std::numeric_limits<CType>::max)())
      {
	for(unsigned int j=0;j <  Point::__DIM;++j)
	  {
	    lcorner[j] = -(std::numeric_limits<CType>::max)();
	    ucorner[j] = (std::numeric_limits<CType>::max)();
	  }
      }
    else 
      for(unsigned int j=0;j < Point::__DIM;++j)
	{
	  lcorner[j]  = floor(p1[j] / length) * length;
	  ucorner[j]  = lcorner[j]+length;
	}
  }  
/*! Distance (Squared) between two Quadtree Boxs
    \brief Computes the distance between two quadtree boxes defined by two sets of two points
    
    This function first computes the minimum enclosing quadtree boxs for two 
    sets of two points, then computes the minimum distance between them.  If the boxes overlap, 
    the distance will be 0.
    \param q1 First point defining first box
    \param q2 Second point defining first box
    \param p1 First point defining second box
    \param p2 Second point defining second box
    \param dist Return value, squared distance between quadtree boxes, 0 if overlap
    \param radius1 Return value, side length of the first quadtree box
    \param radius2 Return value, side length of the second quadtree box
  */
  void dist_sq_between_quad_boxes(const Point &q1, const Point &q2, const Point &p1, const Point &p2, double &dist, double &radius1, double &radius2)
  {
    unsigned int j;
    int x,y;
    double box_edge_q1, box_edge_q2;
    double box_edge_p1, box_edge_p2;
    double z;
    double box_dist1, box_dist2;

    z = 0;

    radius1 = quad_box_length(q1, q2);
    
    radius2 = quad_box_length(p1, p2);

    dist = 0;

    if((radius1 == 0) && (radius2 == 0))
      {
	dist = p1.sqr_dist(q2);
      }
    else if(radius1 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(q1[j] < box_edge_p1) 
	      dist+=(q1[j]-box_edge_p1)*(q1[j]-box_edge_p1);
	    else if(q1[j] > box_edge_p2) 
	      dist+= (q1[j]-box_edge_p2)*(q1[j]-box_edge_p2);
	  }
      }
    else if(radius2 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    
	    if(p1[j] < box_edge_q1) 
	      dist+=(p1[j]-box_edge_q1)*(p1[j]-box_edge_q1);
	    else if(p1[j] > box_edge_q2) 
	      dist+= (p1[j]-box_edge_q2)*(p1[j]-box_edge_q2);
	  }
      }
    else
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(box_edge_q2 < box_edge_p1)
	      z+=(box_edge_p1-box_edge_q2)*(box_edge_p1-box_edge_q2);
	    else if (box_edge_p2 < box_edge_q1)
	      z+=(box_edge_q1-box_edge_p2)*(box_edge_q1-box_edge_p2);
	  }
	dist=z;
      }
    return;
  }
private:
  CType offset;
  bool lt_func(const Point &p, const Point &q)
  {
    CType x,y;
    int k;
    int j;
    x=0;
    for(j=k=0;k < (int) Point::__DIM;++k)
      {
	y = (CType) ((p[k]+offset)^(q[k]+offset));
	if(less_msb(x,y))
	  {
	    j=k;
	    x=y;
	  }
      }
    return p[j] < q[j];
  }

  bool less_msb(CType x, CType y)
  {
    return (x < y) && (x < (x^y));
  }
};


//Z Order spec for signed integral types
/*! Z-Order work class (Signed Integral)
  \brief Z-Order work class specialized for integer types

  This class computes Z-order less than operations and the distance
  to a quadtree box for points with coordinate types that are
  signed integers (short, int, long).
  It is designed to be called by the zorder_lt class.
*/
template<typename Point, typename CType>
class zorder_lt_worker <Point, CType, zorder_t, zorder_t, zorder_f>
{
public:
  /*! 
    Constructor
    \brief Default constructor
   */
  zorder_lt_worker()
  {
    offset=0;
  }
  /*! Destructor
    \brief Default destructor
  */
  ~zorder_lt_worker(){;}
  /*! Function Object Operator
    \brief Calls less than operator

    This function determines whether one point preceeds the other in
    a Z-ordering
    \param p First point
    \param q Second point
    \return True if p comes before q in the Z-ordering of the points
  */
  bool operator()(const Point &p, const Point &q)
  {
    return lt_func(p,q);
  }
  void set_offset(CType off)
  {
    offset=off;
  }
  /*! Distance (Squared) to Quadtree Box
    \brief Computes the distance from a query point to the smallest quadtree 
    box containing two other points.

    This function first computes the minimum enclosing quadtree box for two 
    points, then computes the square distance from the query point to that 
    quadtree box.  If the query point lies within the quadtree box, the distance
    returned will be 0.
    \param q Query point
    \param p1 First quadtree box defining point
    \param p2 Second quadtree box defining point
    \return Squared distance from q to quadtree box.  0 if q lies within box
  */
  double dist_sq_to_quad_box(const Point &q, const Point &p1, const Point &p2)
  {
    unsigned int j;
    int i;
    CType x,y;
    double z, X, Y;
    
    x = (std::numeric_limits<CType>::min)();
    for(j=0;j < Point::__DIM;++j)
      {
	if((p1[j] < 0) != (p2[j] < 0))
	  {
	    return 0;
	  }
	y = (CType) (p1[j]^p2[j]);
	if(less_msb(x,y)) x = y;
      }
    frexp((double)x, &i);
    for(z=j=0;j < Point::__DIM;++j)
      {
	X = (double) ((p1[j]>>i)<<i);
	Y = (double) (1 << i);
	Y = Y+X;
	if(q[j] < X) 
	  z+=pow(((double) X - (double) q[j]), 2.0);
	else if(q[j] > Y) 
	  z+=pow(((double) q[j] - (double) Y), 2.0);
      }
    return z;
  }
/*! Quadtree Box Length
    \brief Computes the side length of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \return The side length of the smallest quadtree box containing p1 and p2
  */
  double quad_box_length(const Point &p1, const Point &p2)
  {
    unsigned int j;
    int i;
    CType x,y;

    x = -(std::numeric_limits<CType>::max)();
    if(p1 == p2)
      return 0;
    for(j=0;j < Point::__DIM;++j)
      {
	if((p1[j] < 0) != (p2[j] < 0))
	  {
	    return 0;
	  }
	y = p1[j]^p2[j];
	if(less_msb(x,y)) x=y;
      }
    x = pow((CType) 2.0,x);
    return x;
  };
  /*! Minimum Enclosing Quadtree Box
    \brief Computes the lower and upper corners of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \param lcorner Return value, lower corner
    \param ucorner Return value, upper corner
  */
  void min_quad_box(const Point &p1, const Point &p2, Point &lcorner, Point &ucorner)
  {
    double length = quad_box_length(p1, p2);
    if(length==0)
      {
	lcorner=p1;
	ucorner=p1;
	return;
      }
    else if(length == (std::numeric_limits<CType>::max)())
      {
	for(unsigned int j=0;j <  Point::__DIM;++j)
	  {
	    lcorner[j] = -(std::numeric_limits<CType>::max)();
	    ucorner[j] = (std::numeric_limits<CType>::max)();
	  }
      }
    else 
      for(unsigned int j=0;j < Point::__DIM;++j)
	{
	  lcorner[j]  = floor(p1[j] / length) * length;
	  ucorner[j]  = lcorner[j]+length;
	}
  }  
/*! Distance (Squared) between two Quadtree Boxs
    \brief Computes the distance between two quadtree boxes defined by two sets of two points
    
    This function first computes the minimum enclosing quadtree boxs for two 
    sets of two points, then computes the minimum distance between them.  If the boxes overlap, 
    the distance will be 0.
    \param q1 First point defining first box
    \param q2 Second point defining first box
    \param p1 First point defining second box
    \param p2 Second point defining second box
    \param dist Return value, squared distance between quadtree boxes, 0 if overlap
    \param radius1 Return value, side length of the first quadtree box
    \param radius2 Return value, side length of the second quadtree box
  */
  void dist_sq_between_quad_boxes(const Point &q1, const Point &q2, const Point &p1, const Point &p2, double &dist, double &radius1, double &radius2)
  {
    unsigned int j;
    int x,y;
    double box_edge_q1, box_edge_q2;
    double box_edge_p1, box_edge_p2;
    double z;
    double box_dist1, box_dist2;

    z = 0;

    radius1 = quad_box_length(q1, q2);
    
    radius2 = quad_box_length(p1, p2);

    dist = 0;

    if((radius1 == 0) && (radius2 == 0))
      {
	dist = p1.sqr_dist(q2);
      }
    else if(radius1 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(q1[j] < box_edge_p1) 
	      dist+=(q1[j]-box_edge_p1)*(q1[j]-box_edge_p1);
	    else if(q1[j] > box_edge_p2) 
	      dist+= (q1[j]-box_edge_p2)*(q1[j]-box_edge_p2);
	  }
      }
    else if(radius2 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    
	    if(p1[j] < box_edge_q1) 
	      dist+=(p1[j]-box_edge_q1)*(p1[j]-box_edge_q1);
	    else if(p1[j] > box_edge_q2) 
	      dist+= (p1[j]-box_edge_q2)*(p1[j]-box_edge_q2);
	  }
      }
    else
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(box_edge_q2 < box_edge_p1)
	      z+=(box_edge_p1-box_edge_q2)*(box_edge_p1-box_edge_q2);
	    else if (box_edge_p2 < box_edge_q1)
	      z+=(box_edge_q1-box_edge_p2)*(box_edge_q1-box_edge_p2);
	  }
	dist=z;
      }
    return;
  }
private:
  CType offset;
  bool lt_func(const Point &p, const Point &q)
  {
    CType x,y;
    int j=0;
    unsigned int k;
    x = (CType) (-(std::numeric_limits<CType>::max)());
    for(k=0;k < Point::__DIM;++k)
      {
	if(((p[k]+offset) < 0) != ((q[k]+offset) < 0))
	  return (p[k]+offset) < (q[k]+offset);
	y = (CType) ((p[k]+offset)^(q[k]+offset));
	if(less_msb(x,y))
	  {
	    j=k;
	    x=y;
	  }
      }
    return p[j] < q[j];
  }
  
  bool less_msb(CType x, CType y)
  {
    return (x < y) && (x < (x^y));
  }

};

//Z Order spec for floating point types
/*! Z-Order work class (Floating Point)
  \brief Z-Order work class specialized for Floating Point types

  This class computes Z-order less than operations and the distance
  to a quadtree box for points with coordinate types that are
  floating point types.  
  zorder_lt_worker is designed to be called by the zorder_lt class.
*/
template<typename Point, typename CType>
class zorder_lt_worker <Point, CType, zorder_t, zorder_f, zorder_f>
{
public: 
  /*! 
    Constructor
    \brief Default constructor
  */
  zorder_lt_worker()
  {
    offset=0;
  }
  /*! Destructor
    \brief Default destructor
  */
  ~zorder_lt_worker(){;}
  void set_offset(CType off)
  {
    offset = off;
  }
  /*! Function Object Operator
    \brief Calls less than operator

    This function determines whether one point preceeds the other in
    a Z-ordering
    \param p First point
    \param q Second point
    \return True if p comes before q in the Z-ordering of the points
  */
  bool operator()(const Point &p, const Point &q)
  {
    return lt_func(p,q);
  }
  /*! Quadtree Box Length
    \brief Computes the side length of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \return The side length of the smallest quadtree box containing p1 and p2
  */
  double quad_box_length(const Point &p1, const Point &p2)
  {
    sep_float<CType> p1c, p2c;
    CType x,y;
    
    x = -(std::numeric_limits<CType>::max)();
    if(p1 == p2)
      return 0;
    for(unsigned int j=0;j < Point::__DIM;++j)
      {
	if((p1[j] < 0) != (p2[j] < 0))
	  {
	    return (double) (std::numeric_limits<CType>::max)();
	  }
	p1c = p1[j];
	p2c = p2[j];
	y = msdb(p1c, p2c);
	if(y > x)
	  {
	    x = y;
	  }
      }
    x = pow((CType) 2.0,x);
    return x;
  };
  /*! Minimum Enclosing Quadtree Box
    \brief Computes the lower and upper corners of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \param lcorner Return value, lower corner
    \param ucorner Return value, upper corner
  */
  void min_quad_box(const Point &p1, const Point &p2, Point &lcorner, Point &ucorner)
  {
    CType length = (CType) quad_box_length(p1, p2);
    if(length==0)
      {
	lcorner=p1;
	ucorner=p1;
	return;
      }
    else if(length == (std::numeric_limits<CType>::max)())
      {
	for(unsigned int j=0;j <  Point::__DIM;++j)
	  {
	    lcorner[j] = -(std::numeric_limits<CType>::max)();
	    ucorner[j] = (std::numeric_limits<CType>::max)();
	  }
      }
    else 
      for(unsigned int j=0;j < Point::__DIM;++j)
	{
	  lcorner[j]  = floor(p1[j] / length) * length;
	  ucorner[j]  = lcorner[j]+length;
	}
  }
  /*! Distance (Squared) to Quadtree Box
    \brief Computes the distance from a query point to the smallest quadtree 
    box containing two other points.
    
    This function first computes the minimum enclosing quadtree box for two 
    points, then computes the square distance from the query point to that 
    quadtree box.  If the query point lies within the quadtree box, the distance
    returned will be 0.
    \param q Query point
    \param p1 First quadtree box defining point
    \param p2 Second quadtree box defining point
    \return Squared distance from q to quadtree box.  0 if q lies within box
  */
  double dist_sq_to_quad_box(const Point &q, const Point &p1, const Point &p2)
  {
    unsigned int j;
    int x,y;
    double box_edge_1, box_edge_2;
    double z;
    CType box_dist;
    sep_float<CType> p1c, p2c;
    z = 0;
    x = -(std::numeric_limits<int>::max)();
    
    for(j=0;j < Point::__DIM;++j)
      {
	if((p1[j] < 0) != (p2[j] < 0))
	  return 0;
	p1c = p1[j];
	p2c = p2[j];
	y = msdb(p1c, p2c);
	if(y > x)
	  {
	    x = y;
	  }
      }
    box_dist = pow(2.0,x);
    for(j=0;j < Point::__DIM;++j)
      {
	box_edge_1 = floor(p1[j] / box_dist) * box_dist;
	box_edge_2 = box_edge_1+box_dist;
	
	if(q[j] < box_edge_1) 
	  z+=(q[j]-box_edge_1)*(q[j]-box_edge_1);
	else if(q[j] > box_edge_2) 
	  z+= (q[j]-box_edge_2)*(q[j]-box_edge_2);
      }
    return z;
  }
/*! Distance (Squared) between two Quadtree Boxs
    \brief Computes the distance between two quadtree boxes defined by two sets of two points
    
    This function first computes the minimum enclosing quadtree boxs for two 
    sets of two points, then computes the minimum distance between them.  If the boxes overlap, 
    the distance will be 0.
    \param q1 First point defining first box
    \param q2 Second point defining first box
    \param p1 First point defining second box
    \param p2 Second point defining second box
    \param dist Return value, squared distance between quadtree boxes, 0 if overlap
    \param radius1 Return value, side length of the first quadtree box
    \param radius2 Return value, side length of the second quadtree box
  */
  void dist_sq_between_quad_boxes(const Point &q1, const Point &q2, const Point &p1, const Point &p2, double &dist, double &radius1, double &radius2)
  {
    unsigned int j;
    int x,y;
    double box_edge_q1, box_edge_q2;
    double box_edge_p1, box_edge_p2;
    double z;
    CType box_dist1, box_dist2;
    sep_float<CType> p1c, p2c, q1c, q2c;

    z = 0;

    radius1 = quad_box_length(q1, q2);
    
    radius2 = quad_box_length(p1, p2);
    dist = 0;

    if((radius1 == 0) && (radius2 == 0))
      {
	dist = p1.sqr_dist(q2);
      }
    else if(radius1 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(q1[j] < box_edge_p1) 
	      dist+=(q1[j]-box_edge_p1)*(q1[j]-box_edge_p1);
	    else if(q1[j] > box_edge_p2) 
	      dist+= (q1[j]-box_edge_p2)*(q1[j]-box_edge_p2);
	  }
      }
    else if(radius2 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    
	    if(p1[j] < box_edge_q1) 
	      dist+=(p1[j]-box_edge_q1)*(p1[j]-box_edge_q1);
	    else if(p1[j] > box_edge_q2) 
	      dist+= (p1[j]-box_edge_q2)*(p1[j]-box_edge_q2);
	  }
      }
    else
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(box_edge_q2 < box_edge_p1)
	      z+=(box_edge_p1-box_edge_q2)*(box_edge_p1-box_edge_q2);
	    else if (box_edge_p2 < box_edge_q1)
	      z+=(box_edge_q1-box_edge_p2)*(box_edge_q1-box_edge_p2);
	  }
	dist=z;
      }
    return;
  }
private:
  bool lt_func(const Point &p, const Point &q)
  {
    int y,x;
    unsigned int k,j;
    sep_float<CType> pc, qc;
    j = 0;
    x = -(std::numeric_limits<int>::max)();
    for(k=0;k < Point::__DIM;++k)
      {
	if((p[k] < 0) != (q[k] < 0))
	  return p[k] < q[k];
	pc = p[k]+offset;
	qc = q[k]+offset;
	y = msdb(pc, qc);
	if(x < y)
	  {
	    j = k;
	    x = y;
	  }
      }
    return p[j] < q[j];
  }
  CType offset;
};

//Z Order spec for seperated floating point types
/*! Z-Order work class (Seperated Floating Point)
  \brief Z-Order work class specialized for Seperated Floating Point types

  This class computes Z-order less than operations and the distance
  to a quadtree box for points with coordinate types that are
  seperated floating point types.  The sfcnn algorithm automatically
  converts float, double and long double into the matching seperated
  type at construction, to save time on calculations.
  zorder_lt_worker is designed to be called by the zorder_lt class.
*/
template<typename Point, typename CType>
class zorder_lt_worker <Point, CType, zorder_t, zorder_f, zorder_t>
{
public: 
  /*! 
    Constructor
    \brief Default constructor
  */
  zorder_lt_worker()
  {
    offset=0;
  }
  /*! Destructor
    \brief Default destructor
  */
  ~zorder_lt_worker(){;}
  void set_offset(CType off)
  {
    offset=off;
  }
  /*! Function Object Operator
    \brief Calls less than operator

    This function determines whether one point preceeds the other in
    a Z-ordering
    \param p First point
    \param q Second point
    \return True if p comes before q in the Z-ordering of the points
  */
  bool operator()(const Point &p, const Point &q)
  {
    return lt_func(p,q);
  }
  /*! Distance (Squared) to Quadtree Box
    \brief Computes the distance from a query point to the smallest quadtree 
    box containing two other points.
    
    This function first computes the minimum enclosing quadtree box for two 
    points, then computes the square distance from the query point to that 
    quadtree box.  If the query point lies within the quadtree box, the distance
    returned will be 0.
    \param q Query point
    \param p1 First quadtree box defining point
    \param p2 Second quadtree box defining point
    \return Squared distance from q to quadtree box.  0 if q lies within box
  */
  double dist_sq_to_quad_box(const Point &q, const Point &p1, const Point &p2)
  {
    unsigned int j;
    int x,y;
    double box_edge_1, box_edge_2;
    double z;
    typename CType::flt_type box_dist;

    z = 0;
    x = -(std::numeric_limits<int>::max)();
    for(j=0;j < Point::__DIM;++j)
      {
	if((p1[j].get_flt() < 0) != (p2[j].get_flt() < 0))
	  return 0;
	y = msdb(p1[j], p2[j]);
	if(y > x)
	  {
	    x = y;
	  }
      }
    box_dist = (typename CType::flt_type) pow((typename CType::flt_type) 2.0,x);
    for(j=0;j < Point::__DIM;++j)
      {
	box_edge_1 = floor(p1[j] / box_dist) * box_dist;
	box_edge_2 = box_edge_1+box_dist;
	
	if(q[j].get_flt() < box_edge_1) 
	  z+=(q[j].get_flt()-box_edge_1)*(q[j].get_flt()-box_edge_1);
	else if(q[j].get_flt() > box_edge_2) 
	  z+= (q[j].get_flt()-box_edge_2)*(q[j].get_flt()-box_edge_2);
      }
    return z;
  }
/*! Quadtree Box Length
    \brief Computes the side length of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \return The side length of the smallest quadtree box containing p1 and p2
  */
  double quad_box_length(const Point &p1, const Point &p2)
  {
    double x,y;
    
    x = -(std::numeric_limits<double>::max)();
    if(p1 == p2)
      return 0;
    for(unsigned int j=0;j < Point::__DIM;++j)
      {
	if((p1[j].get_flt() < 0) != (p2[j].get_flt < 0))
	  {
	    return (double) (std::numeric_limits<double>::max)();
	  }
	y = msdb(p1[j], p2[j]);
	if(y > x)
	  {
	    x = y;
	  }
      }
    x = pow(2.0,x);
    return x;
  };
  /*! Minimum Enclosing Quadtree Box
    \brief Computes the lower and upper corners of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \param lcorner Return value, lower corner
    \param ucorner Return value, upper corner
  */
  void min_quad_box(const Point &p1, const Point &p2, Point &lcorner, Point &ucorner)
  {
    double length = quad_box_length(p1, p2);
    if(length==0)
      {
	lcorner=p1;
	ucorner=p1;
	return;
      }
    else if(length == (std::numeric_limits<CType>::max)())
      {
	for(unsigned int j=0;j <  Point::__DIM;++j)
	  {
	    lcorner[j] = -(std::numeric_limits<CType>::max)();
	    ucorner[j] = (std::numeric_limits<CType>::max)();
	  }
      }
    else 
      for(unsigned int j=0;j < Point::__DIM;++j)
	{
	  lcorner[j]  = floor(p1[j] / length) * length;
	  ucorner[j]  = lcorner[j]+length;
	}
  }
/*! Distance (Squared) between two Quadtree Boxs
    \brief Computes the distance between two quadtree boxes defined by two sets of two points
    
    This function first computes the minimum enclosing quadtree boxs for two 
    sets of two points, then computes the minimum distance between them.  If the boxes overlap, 
    the distance will be 0.
    \param q1 First point defining first box
    \param q2 Second point defining first box
    \param p1 First point defining second box
    \param p2 Second point defining second box
    \param dist Return value, squared distance between quadtree boxes, 0 if overlap
    \param radius1 Return value, side length of the first quadtree box
    \param radius2 Return value, side length of the second quadtree box
  */
  void dist_sq_between_quad_boxes(const Point &q1, const Point &q2, const Point &p1, const Point &p2, double &dist, double &radius1, double &radius2)
  {
    unsigned int j;
    int x,y;
    double box_edge_q1, box_edge_q2;
    double box_edge_p1, box_edge_p2;
    double z;
    double box_dist1, box_dist2;

    z = 0;

    radius1 = quad_box_length(q1, q2);
    
    radius2 = quad_box_length(p1, p2);

    dist = 0;

    if((radius1 == 0) && (radius2 == 0))
      {
	dist = p1.sqr_dist(q2);
      }
    else if(radius1 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(q1[j] < box_edge_p1) 
	      dist+=(q1[j]-box_edge_p1)*(q1[j]-box_edge_p1);
	    else if(q1[j] > box_edge_p2) 
	      dist+= (q1[j]-box_edge_p2)*(q1[j]-box_edge_p2);
	  }
      }
    else if(radius2 == 0)
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    
	    if(p1[j] < box_edge_q1) 
	      dist+=(p1[j]-box_edge_q1)*(p1[j]-box_edge_q1);
	    else if(p1[j] > box_edge_q2) 
	      dist+= (p1[j]-box_edge_q2)*(p1[j]-box_edge_q2);
	  }
      }
    else
      {
	for(j=0;j < Point::__DIM;++j)
	  {
	    box_edge_q1 = floor(q1[j] / radius1) * radius1;
	    box_edge_q2 = box_edge_q1+radius1;
	    box_edge_p1 = floor(p1[j] / radius2) * radius2;
	    box_edge_p2 = box_edge_p1+radius2;
	    
	    if(box_edge_q2 < box_edge_p1)
	      z+=(box_edge_p1-box_edge_q2)*(box_edge_p1-box_edge_q2);
	    else if (box_edge_p2 < box_edge_q1)
	      z+=(box_edge_q1-box_edge_p2)*(box_edge_q1-box_edge_p2);
	  }
	dist=z;
      }
    return;
  }
private:
  bool lt_func(const Point &p, const Point &q)
  {
    int y,x;
    unsigned int k,j;
    j = 0;
    x = -(std::numeric_limits<int>::max)();
    for(k=0;k < Point::__DIM;++k)
      {
	if((p[k].get_flt() < 0) != (q[k].get_flt() < 0))
	  return p[k].get_flt() < q[k].get_flt();
	y = msdb(p[k], q[k]);
	if(x < y)
	  {
	    j = k;
	    x = y;
	  }
      }
    return p[j] < q[j];
  }
  CType offset;
};

/*! Z-order Less Than Operator
  \brief Wrapper class for various Z-Order operations
  
  This class provides an interface with several specialized worker classes
  which control some functions based on the Z-order of multi-dimensional 
  points.  The actual work is done by the zorder_lt_worker class,
  which is specialized based on the type of coordinates being used.
  \param Point The point type being used.
*/
  
template<typename Point>
class zorder_lt
{
public:
  /*! Constructor
    \brief Default constructor. 
    
    The constructor will check that the type of coordinate used in the
    point type has had the appropriate traits defined in zorder_type_traits.hpp
  */
  zorder_lt()
  {
    zorder_traits<typename Point::__NumType>::check_type();
  }
  /*! Destructor
    \brief Default Destructor
  */
  ~zorder_lt(){;}
  
  void set_offset(typename Point::__NumType offset)
  {
    lt.set_offset(offset);
  }
  /*! Function Object Operator
    \brief Calls less than operator

    This function determines whether one point preceeds the other in
    a Z-ordering by calling the appropriate function in the 
    specialized z_order_worker class
    \param p First point
    \param q Second point
    \return True if p comes before q in the Z-ordering of the points
  */
  bool operator()(const Point &p, const Point &q)
  {
    return lt(p, q);
  }
/*! Function Object Operator
    \brief Calls less than operator

    This function determines whether one point preceeds the other in
    a Z-ordering.  In this instance of the function, the points are
    taken from a Mypair of iterator types.  This is used to sort
    two vectors simultaneously.  See pair_iter.hpp.
    \param p First point
    \param q Second point
    \return True if p comes before q in the Z-ordering of the points
  */
  bool operator()(const Mypair<typename std::vector<Point>::iterator,typename std::vector<long unsigned int>::iterator> &p, const Mypair<typename std::vector<Point>::iterator, typename std::vector<long unsigned int>::iterator> &q)
  {
    return lt(p.val1, q.val1);
  }
  /*! Distance (Squared) to Quadtree Box
    \brief Computes the distance from a query point to the smallest quadtree 
    box containing two other points.

    This function first computes the minimum enclosing quadtree box for two 
    points, then computes the square distance from the query point to that 
    quadtree box.  If the query point lies within the quadtree box, the distance
    returned will be 0.  This function calls the appropriate function
    in the specialized z_order_worker class.
    \param q Query point
    \param p1 First quadtree box defining point
    \param p2 Second quadtree box defining point
    \return Squared distance from q to quadtree box.  0 if q lies within box
  */
  double dist_sq_to_quad_box(const Point &q, const Point &p1, const Point &p2)
  {
    return lt.dist_sq_to_quad_box(q,p1,p2);
  }
  /*! Distance (Squared) between two Quadtree Boxs
    \brief Computes the distance between two quadtree boxes defined by two sets of two points
    
    This function first computes the minimum enclosing quadtree boxs for two 
    sets of two points, then computes the minimum distance between them.  If the boxes overlap, 
    the distance will be 0.
    \param q1 First point defining first box
    \param q2 Second point defining first box
    \param p1 First point defining second box
    \param p2 Second point defining second box
    \param dist Return value, squared distance between quadtree boxes, 0 if overlap
    \param radius1 Return value, side length of the first quadtree box
    \param radius2 Return value, side length of the second quadtree box
  */
  void dist_sq_between_quad_boxes(const Point &q1, const Point &q2, const Point &p1, const Point &p2, double &dist, double &radius1, double &radius2)
  {
    lt.dist_sq_between_quad_boxes(q1, q2, p1, p2, dist, radius1, radius2);
  }
 /*! Minimum Enclosing Quadtree Box
    \brief Computes the lower and upper corners of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \param lcorner Return value, lower corner
    \param ucorner Return value, upper corner
  */
  void min_quad_box(const Point &p1, const Point &p2, Point &lcorner, Point &ucorner)
  {
    lt.min_quad_box(p1,p2, lcorner, ucorner);
  }
  /*! Quadtree Box Length
    \brief Computes the side length of the smallest quadtree box containing two points
    \param p1 First point
    \param p2 Second point
    \return The side length of the smallest quadtree box containing p1 and p2
  */
  double quad_box_length(const Point &p1, const Point &p2)
  {
    return lt.quad_box_length(p1, p2);
  };
private:
  zorder_lt_worker<Point, typename Point::__NumType, typename zorder_traits<typename Point::__NumType>::is_signed, typename zorder_traits<typename Point::__NumType>::is_integral, typename zorder_traits<typename Point::__NumType>::is_seperated> lt;
};
#endif
