/*****************************************************************************/
/*                                                                           */
/*  Header: compute_bounding_box.hpp                                         */
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

#ifndef __STANN_COMPUTE_BOUNDING_BOX__
#define __STANN_COMPUTE_BOUNDING_BOX__

#include <sep_float.hpp>

template<typename Point, typename Ptype>
class cbb_work
{
public:
  
  static inline void eval(Point q, Point &q1, Point &q2, double R, Ptype max, Ptype min)
  {
    Ptype radius;
    R = ceil(R);

    radius = (Ptype) R;

    for(unsigned int i=0;i<Point::__DIM;++i)
    {
      if(q[i] < (min+radius)) q1[i] = min;
      else q1[i] = (Ptype) (q[i]-radius);
      
      if(q[i] > (max-radius)) q2[i] = max;
      else q2[i] = (Ptype) (q[i]+radius);
    }
  }
};

template<typename Point>
class cbb_work<Point, float>
{
public:
  
  static inline void eval(Point q, Point &q1, Point &q2, double R, float max, float min)
  {
    float radius;
    radius = (float) R;

    for(unsigned int i=0;i<Point::__DIM;++i)
    {
      if(q[i] < (min+radius)) q1[i] = min;
      else q1[i] = (q[i]-radius);
      
      if(q[i] > (max-radius)) q2[i] = max;
      else q2[i] = (q[i]+radius);
    }
  }
};

template<typename Point>
class cbb_work<Point, sep_float<float> >
{
public:
  
  static inline void eval(Point q, Point &q1, Point &q2, double R, sep_float<float> max, sep_float<float> min)
  {
    sep_float<float> radius = (float) R;
    for(unsigned int i=0;i<Point::__DIM;++i)
    {
      if(q[i] < (min+radius)) q1[i] = min;
      else q1[i] = (q[i]-radius);
      
      if(q[i] > (max-radius)) q2[i] = max;
      else q2[i] = (q[i]+radius);
    }
  }
};

template<typename Point>
class cbb_work<Point, double>
{
public:
  
  static inline void eval(Point q, Point &q1, Point &q2, double R, double max, double min)
  {
    for(unsigned int i=0;i<Point::__DIM;++i)
    {
      if(q[i] < (min+R)) q1[i] = min;
      else q1[i] = (q[i]-R);
      
      if(q[i] > (max-R)) q2[i] = max;
      else q2[i] = (q[i]+R);
    }
  }
};

template<typename Point>
class cbb_work<Point, sep_float<double> >
{
public:
  
  static inline void eval(Point q, Point &q1, Point &q2, double R, sep_float<double> max, sep_float<double> min)
  {
    sep_float<double> radius = R;
    for(unsigned int i=0;i<Point::__DIM;++i)
    {
      if(q[i] < (min+radius)) q1[i] = min;
      else q1[i] = (q[i]-radius);
      
      if(q[i] > (max-radius)) q2[i] = max;
      else q2[i] = (q[i]+radius);
    }
  }
};



#endif
