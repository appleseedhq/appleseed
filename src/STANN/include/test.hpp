/*****************************************************************************/
/*                                                                           */
/*  Header: test.hpp                                                         */
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



#ifndef __STANN_TEST__
#define __STANN_TEST__

#include <algorithm>
#include <iostream>
#include <vector>
#include <limits>

#include <bruteNN.hpp>
#include <dpoint.hpp>
#include <rand.hpp>
#include <sfcnn.hpp>
#include <sfcnn_knng.hpp>
#include <gmst.hpp>
#include <zorder_lt.hpp>

/*! \file test.hpp
\brief Header file for the test.cpp program.*/

template<typename Point, typename T>
Point newRandomPoint(T Min, T Max)
{
  double d;
  double e;
  double f;
  Point a;
  double max, min;
  max = (double) Max;
  min = (double) Min;
  for(unsigned int i=0;i < Point::__DIM;++i)
    {
      d = drand48();
      if(min > 0)
	{
	  e = (d*max)-(d*min);
	  f = min;
	}
      else
	{
	  e = (d*max)+min;
	  f = -(d*min);
	}
      
      a[i] = (T) (e+f);
    }
  return a;
}

template<typename T, unsigned DIM>
bool testNN(unsigned int Size, unsigned int k, T min, T max)
{
  typedef reviver::dpoint<T, DIM> Point;
  std::vector<Point> data;
  std::vector<Point> query;
  std::vector<long unsigned int> sfcnn_ans;
  std::vector<long unsigned int> bf_ans;

  data.resize(Size);
  query.resize(Size);
  
  for(unsigned int i=0;i < data.size();++i)
    {
      data[i]  = newRandomPoint<Point, T>(min, max);
      query[i] = newRandomPoint<Point, T>(min, max);
    }

  bruteNN<Point> BF(&data[0], data.size());
  sfcnn<Point, DIM, T> SFC(&data[0], data.size());

  for(unsigned int i=0;i < data.size();++i)
    {
      BF.ksearch(query[i], k, bf_ans);
      SFC.ksearch(query[i], k, sfcnn_ans); 
      
      for(unsigned int j=0;j < Point::__DIM;++j)
	{
	  if(bf_ans[j] != sfcnn_ans[j])
	    return false;
	}
    }
  return true;
}

template<unsigned DIM>
bool testGMST(unsigned int Size)
{
  typedef reviver::dpoint<double, DIM> Point;
  typedef std::pair<typename std::vector<Point>::size_type, typename std::vector<Point>::size_type> Edge;

  std::vector<Point> data;
  std::vector<Edge> ans;
  std::vector<Edge> bfans;

  data.resize(Size);
  for(int i=0;(unsigned int) i < data.size();++i)
    {
      data[i]  = newRandomPoint<Point, double>(0, 1);
    }

  gmst(data, ans);
  bfgmst(data, bfans);
  
  double dist1=0, dist2=0;
  for(int i=0;i < (int) ans.size();++i)
    {
      dist1+= data[ans[i].first].sqr_dist(data[ans[i].second]);
      dist2+= data[bfans[i].first].sqr_dist(data[bfans[i].second]);
    }
  return dist1==dist2;
  
}
template<typename T, unsigned DIM>
bool testKNNG(unsigned int Size, unsigned int k, T min, T max, int num_threads)
{
  typedef reviver::dpoint<T, DIM> Point;
  std::vector<Point> data;
  std::vector<long unsigned int> bf_ans;

  data.resize(Size);
  
  for(unsigned int i=0;i < data.size();++i)
    {
      data[i]  = newRandomPoint<Point, T>(min, max);
    }

  bruteNN<Point> BF(&data[0], data.size());
  sfcnn_knng<Point, DIM, T> SFC(&data[0], data.size(), k, num_threads);

  for(unsigned int i=0;i < data.size();++i)
    {
      BF.ksearch(data[i], k+1, bf_ans);
      for(unsigned int j=1;j < k+1;++j)
	{
	  if(bf_ans[j] != SFC[i][j-1])
	    return false;
	}
    }
  return true;
}

template<typename T, unsigned DIM>
bool testZLT(T Min, T Max)
{
  typedef reviver::dpoint<T, DIM> Point;

  Point min, max, random;
  zorder_lt<Point> lt;

  for(unsigned i = 0; i < DIM;++i)
    {
      min[i]=Min;
      max[i]=Max;
    }
  random = newRandomPoint<Point, T>(Min, Max);
  while((random == min) || (random == max))
    {
      random = newRandomPoint<Point, T>(Min, Max);
    }

  if(lt(max, min)) return false;
  if(lt(random, min)) return false;
  if(lt(max, random)) return false;
  if(!lt(min, max)) return false;
  if(!lt(min, random)) return false;
  if(!lt(random, max)) return false;
  return true;
}

template<typename T, unsigned DIM>
bool testSORT(unsigned int Size, T Min, T Max)
{
  typedef reviver::dpoint<T, DIM> Point;
  std::vector<Point> points;
  zorder_lt<Point> lt;

  for(unsigned int i=0;i < Size;++i)
    {
      points.push_back(newRandomPoint<Point, T>(Min, Max));
    }
  sort(points.begin(), points.end(), lt);

  for(unsigned int i=0;i < Size-1;++i)
    {
      if(!lt(points[i], points[i+1])) return false;
    }
  return true;
}
#endif
