/*****************************************************************************/
/*                                                                           */
/*  Header: filter_kruskal.hpp                                               */
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

#ifndef __STANN_FILTER_KRUSKAL__
#define __STANN_FILTER_KRUSKAL__

#include <vector>
#include <algorithm>
#include <limits>
#include <dpoint.hpp>
#include <int_partition.hpp>
#include <pwspd.hpp>
#include <rand.hpp>
#include <timer.hpp>
#include <predicates.hpp>

/*! \file
  \brief Contains the geometric filter kruskal class
*/

/*! Geometric Filter Kruskal class
  \brief Implements Geometric Filter Kruskal on a well-seperated pair decomposition
  
  This class takes a well seperated pair decomposition as input and outputs and edge set representation of 
  the geometric minimum spanning tree.  Currently, only the L2 metric is supported.
*/
template <typename Point>
class FilterKruskal
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef std::pair<size_type, size_type> Edge;
  typedef typename std::vector<WSP<Point> >::iterator WspIter;
  typedef typename std::vector<WSP<Point>*>::iterator WspPIter;
  typedef std::pair<Edge, float> WEdge;
  typedef comp_quad_tree_node<Point>* CqPtr;
  
public:
  
  /*! run
    \brief Finds the geometric minimum spanning tree for points in the range [begin:end)
    \param begin Random access iterator indicating the first well-seperated pair
    \param end Random access iterator indicating the end of the well-seperated pairs
  */
  void run(WspIter begin, WspIter end, int THold=1)
  {
    pivot_index = (1000*wspd.Points.size())/(end-begin);
    double thold = floor(log((double)wspd.Points.size()));
    if(THold != 0) thold = THold;
    recurse_filter_kruskal3(begin, end, (int) thold);
  };

  /*! Constructor
    \brief Constructor for filter kruskal object
    \param w Reference to well-seperated pair decomposition
    \param uf Reference to union find data structure
    \param mst Reference to output vector
  */
  FilterKruskal(pwspd<Point> &w, int_partition &uf, 
		std::vector<Edge> &mst, size_type t=20) : wspd(w), 
						     ufpred(uf,w.cqtree, w.Points),
						     Uf(uf), 
						     Mst(mst),
						     threshold(t)
  {
    bccpTotal=0;
    samples.resize(1000);
  };

private:
  pwspd<Point> &wspd;
  bccpPred<Point> bccp;
  boxLengthPred<Point> blpred;
  unionFindPred<Point> ufpred;
  boxDistPredMin<Point> bdpredmin;
  boxDistPredMax<Point> bdpredmax;
  bccpDistPred<Point> bccppred;
  calcBccpPred<Point> calcbccppred;
  int_partition &Uf;
  std::vector<Edge> &Mst;
  size_type threshold;
  size_type pivot_index;
  int bccpTotal;
  int wspd_size;
  std::vector<WspIter> samples;

  WspIter findPivot(WspIter begin, WspIter end)
  {
    double range = (double) (end-begin);
    
    for(unsigned int i=0;i < samples.size();++i)
      {
	samples[i] = (begin+((size_type) (__drand48__()*range)));
      }
    
    sort(samples.begin(), samples.end(), bccp);
    return samples[pivot_index];
  };
  
  void recurse_filter_kruskal3(WspIter begin, WspIter end, int Thold)
  {
    if(end-begin == 0) return;

    //Walk through the loop
    //for every element of
    //size > threshold,
    //calculate the BCCP
    //and maintain the minimum
    //non-computed bccp
    WspIter I;
    double minBccp = (std::numeric_limits<double>::max)();
    calcbccppred.minBccp = &minBccp;
    calcbccppred.THold = Thold;
#ifdef _GLIBCXX_PARALLEL
    I = partition(begin, end, calcbccppred, __gnu_parallel::sequential_tag());
#else
    I = partition(begin, end, calcbccppred);
#endif
    //Partition the computed BCCP into 
    //everything smaller and larger than the
    //smallest non-computed BCCP
    bdpredmin.minBccp = minBccp;
    WspIter cpart = partition(begin, I, bdpredmin);
    //Partition and process every computed
    //BCCP smaller than the smallest non_computed
    //BCCP
    part_and_process(begin, cpart);
    if(Mst.size() >= wspd.Points.size())
      return;
    
    
    //Filter everything that's left
    WspIter fpart = partition(cpart, end, ufpred);
    //Repeat with double the threshold
    recurse_filter_kruskal3(cpart, fpart, Thold*2);
    
  };
  
  void part_and_process(WspIter begin, WspIter end)
  {
    if((unsigned int) (end-begin) < (wspd.Points.size()*10))
      {
	Kruskal(begin, end);
      }
    else
      {
	WspIter I = findPivot(begin, end);
	bdpredmin.minBccp = I->get_bccp_dist();
	I = partition(begin, end, bdpredmin);
	Kruskal(begin, I);
	if(Mst.size() >= wspd.Points.size())
	  return;
	WspIter fpart = partition(I, end, ufpred);
	part_and_process(I, fpart);
      }
  }

  void Kruskal(WspIter begin, WspIter end)
  {
    size_type first, second;
    Point *f,*s,*z;

    if(end-begin==0) return;

    sort(begin, end, bccp);
    //Find the address of the first element in
    //The z-order sorted vector of points
    z = &(*(wspd.Points.begin()));
    
    for(WspIter i=begin;i != end;++i)
      {
	//Find the index of the bccp points
	//from the pointer to them
	f = (Point*) (i->first);
	s = (Point*) (i->second);
	first = f-z;
	second = s-z;

	//Check the Union Find data structure
	//if the bccp points are not allready
	//connected, join them in the UF and add
	//them to the MST
	if(Uf.find(first) != Uf.find(second))
	{
	  Mst.push_back(Edge(first, second));
	  Uf.union_blocks(first, second);
	  if(Mst.size() >= wspd.Points.size())
	    return;
	}
      }
  };
  
  void recurse_filter_kruskal(WspIter begin, WspIter end, int THold)
  {
    
    WspIter part,fpart, part1, part2;
        
    if(end-begin < 1000)
      {
	Kruskal(begin, end);
      }
    else
      {
	findPivot(begin, end, bdpredmin, bdpredmax);

	do
	  {
	    MyTimer A;
	    part1 = partition(begin, end, bdpredmin);
	    std::cout << "Part1 size: " << end-begin << " Time: " << A << std::endl;
	    if(part1==begin) bdpredmin.minBccp*=2;
	  }
	while(part1==begin);

	bdpredmax.maxBccp = bdpredmin.minBccp;
	part2 = partition(part1, end, bdpredmax);
	do_bccp(part1, part2);
	bccppred.maxBccp = bdpredmin.minBccp;
	part  = partition(part1, part2, bccppred);
	Kruskal(begin, part);
	fpart = partition(part, end, ufpred);
	recurse_filter_kruskal(part, fpart, THold);
      }
  }

  void do_bccp(WspIter begin, WspIter end)
  {
    for(WspIter i=begin;i != end;++i)
      (*i).bccp();
  }
  
void recurse_filter_kruskal2(WspIter begin, WspIter end, int Thold)
{
  std::cout << Thold << " Range: " << end-begin << std::endl;
  if(end-begin == 0) return;

  //Walk through the loop
  //for every element of
  //size > threshold,
  //calculate the BCCP
  WspIter I;
  for(I=begin;I != end;++I)
    {
      if(I->size()<=Thold)
	{
	  //std::cout << I->size() << std::endl;
	  I->bccp();
	}
    }

  //Sort all valid WSPs based on
  //BCCP value (Note: in this case
  //BCCP is either the actual BCCP value
  //or the minimum possible BCCP value)
  std::cout << "sorting" << std::endl;
  sort(begin, end, bccp);

  //Find the first WSP for which the BCCP
  //Has not yet been calculated
  I=begin;
  while(I->bccp_done())
    {
      ++I;
    }
  std::cout << "Minimum non-computed bccp: " << I->get_bccp_dist() << std::endl;
  std::cout << "\tTo Kruskal: " << I-begin <<  " " << begin->size() << " " << end-begin << std::endl;

  //Send the first block of WSPs
  //That have had their BCCP calulated
  //To be tested/added to the MST
  Kruskal2(begin, I);

  //Partition the remaining WSPs to filter
  //out any pairs that have allready been
  //connected
  std::cout << "Filtering" << std::endl;
  WspIter fpart = partition(I, end, ufpred);
  std::cout << "filtered : " << end-fpart << std::endl;

  //Double the threshold value and recurse on any elements
  //that are left
  recurse_filter_kruskal2(I, fpart, Thold*2);
}

  void Kruskal2(WspIter begin, WspIter end)
  {
    size_type first, second;
    Point *f,*s,*z;

    if(end-begin==0) return;

    //Find the address of the first element in
    //The z-order sorted vector of points
    z = &(*(wspd.Points.begin()));
    
    for(WspIter i=begin;i != end;++i)
      {
	//Find the index of the bccp points
	//from the pointer to them
	f = (Point*) (i->first);
	s = (Point*) (i->second);
	first = f-z;
	second = s-z;

	//Check the Union Find data structure
	//if the bccp points are not allready
	//connected, join them in the UF and add
	//them to the MST
	if(Uf.find(first) != Uf.find(second))
	{
	  Mst.push_back(Edge(first, second));
	  Uf.union_blocks(first, second);
	}
      }
  };
};
#endif
