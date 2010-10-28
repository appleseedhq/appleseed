/*****************************************************************************/
/*                                                                           */
/*  Header: pwspd.hpp                                                        */
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

#ifndef __STANN_PWSPD__
#define __STANN_PWSPD__

#include <cstring>
#include <limits>
#include <vector>
#include <comp_quad_tree.hpp>
#include <zorder_lt.hpp>

#ifdef _OPENMP
#include <omp.h>
#endif
/*! \file
  \brief Implementation of a parallel well seperated pair decomposition for a compressed quad tree
*/

/*! Well Seperated Pair
  \brief Implements functions needed by a Well Seperated Pair object
*/
template<typename Point>
class WSP
{
  typedef typename std::vector<Point>::size_type size_type;
  typedef typename std::vector<Point>::iterator PItr;
  typedef comp_quad_tree_node<Point>* index;

public:

  //Before the BCCP is computed, first and last point
  //To the quad-tree boxes that make up the WSP
  //After BCCP is computed, they point to the two points
  //that make up the BCCP
  /*! first
    \brief Indicates either the first quadtree box, or point
    Initially, this pointer indcates the comp_quad_tree_node that
    is the first half of the well-seperated pair.  After the BCCP is
    calculated, this because a pointer to a point, indicating it is
    half of the BCCP
  */
  void* first;
  /*! second
    \brief Indicates either the second quadtree box, or point
    Initially, this pointer indcates the comp_quad_tree_node that
    is the second half of the well-seperated pair.  After the BCCP is
    calculated, this because a pointer to a point, indicating it is
    half of the BCCP
  */
  void* second;
  /*! bccp_dist
    \brief Indicates either the BCCP distance, or the marginal BCCP distance
    If this value is negative, it indicates the current minimum possible BCCP distance known.  If it is positive, it indicates the actual BCCP distance
  */
  double bccp_dist;
  /*! Constructor
    \brief Default constructor
  */
  WSP()
  {    bccp_dist = -1;
  };

  /*! Constructor
    \brief Construcs a WSP consisting of two comp_quad_tree_node elements
    \param f First comp_quad_tree_node
    \param s Second comp_quad_tree_node
  */
  WSP(index f, index s) : first(f), second(s)
  {

    //We store the marginal distance as a negative to indicate
    //That we haven't computed the actual BCCP yet
    bccp_dist = -dist_sq_between_quad_boxes(f,s);
    if(bccp_dist == 0)
      {
	std::cout << std::endl << "Error in WSP construction!" << std::endl;
	std::cout << (*(f->low))[0] << "," << (*(f->low))[1]  << std::endl;
	std::cout << (*(s->low))[0] << "," << (*(s->low))[1] << std::endl;
	if(f==s)
	  std::cout << "F==S!" << std::endl;
	exit(0);
      }
  };
  
  /*! size
    \brief returns the number of points in the WSP
  */
  long int size()
  {
    if(bccp_done())
      return 0;
    //std::cout << "bccp_dist: " << bccp_dist << std::endl;
    return (((index)first)->hig-((index)first)->low)+(((index) second)->hig-((index) second)->low);
  };

  /*! get bccp distance
    \brief returns the currently computed BCCP distance
  */
  double get_bccp_dist() const
  {
    return (double) fabs(bccp_dist);
  }

  /*! BCCP done
    \brief returns true if the actual BCCP has been computed, false otherwise
  */
  bool bccp_done() const
  {
    return bccp_dist >= 0;
  }

  /*! Assignment Operator
    \brief Assignment operator
  */
  WSP<Point>& operator=(const WSP<Point> b)
  {
    first = b.first;
    second = b.second;
    bccp_dist = b.bccp_dist;
    return *this;
  }

  /*! BCCP
    \brief computes the BCCP if it has not been
    For WSPs of small size (<32) this computes the BCCP by brute force.
    Otherwise it calls a recursive algorithm.
  */
  void bccp()
  {
    if(bccp_done())
      return;    
    //Note: After BCCP is computed, the void* first and second
    //point to Points, not comp_quad_tree_nodes
    
    //For small sized quad-tree boxes, 
    //Brute force computation of BCCP is faster
    if(size() < 32)
      {
	brute_force_bccp((index)first, (index)second);
	return;
      }

    //For small sized boxes, compute using 
    //recursive algorithm
    do_bccp();
    return;
  };
private:
  //Compute the BCCP by brute force
  //Note: This works well because
  //the points in the quad-tree boxes
  //are already in a contiguous chunk the the
  //z-order sorted point list
  void brute_force_bccp(index l, index r)
  {
    double dist;
    bccp_dist = (std::numeric_limits<double>::max)();
    for(PItr i=l->low;i != l->hig;i+=1)
      {
	for(PItr j=r->low;j != r->hig;j+=1)
	  {
	    dist = i->sqr_dist(*j);
	    if(dist < bccp_dist)
	      {
		bccp_dist=dist;
		first=&*i;
		second=&*j;
	      }
	  }
      }
  };
  
  //Recursive BCCP computation.
  //Decends the compressed quadtree and 
  //computes the bccp
  void bccp_recurse(index r, index l)
  {
    double d1, d2;
    index t;
    if((r->length==0) && (l->length==0))
      {
	d1 = dist_sq_between_quad_boxes(r, l);
	if(d1 < bccp_dist)
	  {
	    bccp_dist=d1;
	    first=&*(r->low);
	    second=&*(l->low);
	  }
	return;
      }
    if(r->length < l->length)
      {
	t=l;
	l=r;
	r=t;
      }

    d1 = dist_sq_between_quad_boxes(r->left, l);
    d2 = dist_sq_between_quad_boxes(r->right, l);

    if(d1 < d2)
      {
	if(d1 < bccp_dist)
	  {
	    bccp_recurse(r->left, l);
	  }
	if(d2 < bccp_dist)
	  {
	    bccp_recurse(r->right, l);
	  }
      }
    else
      {
	if(d2 < bccp_dist)
	  {
	    bccp_recurse(r->right, l);
	  }
	if(d1 < bccp_dist)
	  {
	    bccp_recurse(r->left, l);
	  }
      }
  }

  //compute the bi-chromatic closest pair from two well seperated ranges
  void do_bccp()
  {
    bccp_dist = ((index)first)->low->sqr_dist(*(((index)second)->low));
    index f,s;
    f = (index) first;
    s = (index) second;
    first = &*(f->low);
    second = &*(s->low);
    bccp_recurse(f,s);
  }
};

/*! Parallel Well Seperated Pair Decomposition Class
  \brief Implements a prallel well-seperated pair decomposition of a point set
*/
template<typename Point>
class pwspd
{
public:
  typedef typename std::vector<Point>::size_type size_type;
  typedef comp_quad_tree_node<Point>* Node;
  typedef typename std::vector<Point>::iterator PItr;
  typedef std::pair<PItr, PItr> Edge;
  
  /*! Constructor
    \brief Constructor
    \param P vector of points for which the WSPD is to be constructed.
    \param S seperation factor for the WSPD (boxes will be considered well separated if they have a minimum distance greater than S times the maximum corner to corner distance of the boxes)
  */
  pwspd(std::vector<Point> &P, double S=2) : Points(P), cqtree(P)
  {
    setSpread(S);
  }
  /*! Destructor
    \brief Destructor
  */
  ~pwspd()
  {
  };
  
  /*! Set Spread
    \brief sets the spread of the WSPD
    \param S the spread factor
    Note, this will only effect the WSPD if set before the run command is issued
  */
  void setSpread(double S)
  {
    spread=S*S;
  };

  /* Get Spread
     \brief returns the spread of the WSPD
  */
  double getSpread()
  {
    return sqrt(spread);
  };
  
  /*! Run
    \brief Generates the WSPD
    \param Wsp Vector of WSPs storing the output
    \param num_threads Number of threads to use (requires OPENMP)
  */
  void run(std::vector<WSP<Point> > &Wsp, int num_threads=1)
  {
    int i;
    int tid=0;
    std::vector<size_type> Wsp_size;
    std::vector<std::vector<WSP<Point> > > wsp;
    wsp.resize(num_threads);
#ifdef _OPENMP
    omp_set_num_threads(num_threads);
#pragma omp parallel private(i, tid) shared(wsp)
    {
      tid = omp_get_thread_num();
#pragma omp for schedule(static)
#endif
      for(i=1;i < (int) cqtree.size()/2;++i)
      	{
	  decomp(&cqtree[i], 
		 wsp[tid]);
	}
#ifdef _OPENMP
#pragma omp barrier
#pragma omp single
      {
#endif
	Wsp_size.resize(num_threads);
	Wsp_size[0]=0;
	for(i=1;i < num_threads;++i)
	  {
	    Wsp_size[i] = wsp[i-1].size()+Wsp_size[i-1];
	  }
	Wsp.resize(Wsp_size[i-1]+wsp[i-1].size());
#ifdef _OPENMP
      } //End single section
#pragma omp for schedule(static,1)
#endif
      for(i=0;i < num_threads;++i)
	{
	  memcpy(&(Wsp[Wsp_size[i]]), &(wsp[i][0]),
		 sizeof(WSP<Point>)*wsp[i].size());
	}
#ifdef _OPENMP
    } //End parallel section
#endif
  };
  
  /*! Points
    \brief Reference to the points the WSPD is built on
  */
  std::vector<Point> &Points;  
  /*! cqtree
    \brief Reference to the compressed quadtree built for the WSPD
  */
  comp_quad_tree<Point> cqtree;
private:
  double spread;
  zorder_lt<Point> lt;
  void decomp(const Node node,
	      std::vector<WSP<Point> > &wsp)
  {
    
    if(node->left->length == 0)
      {
	if(node->right->length == 0)
	  {
	    if(node->left->low->sqr_dist(*(node->right->low)) > 0)
	      {
		WSP<Point> c(node->left, node->right);
		wsp.push_back(c);
	      }
	  }
	else
	  {
	    recursive_decomp_p(node->left, node->right, wsp);
	  }
      }
    else
      {
	if(node->right->length == 0)
	  {
	    recursive_decomp_p(node->right, node->left, wsp);
	  }
	else
	  {
	    recursive_decomp(node->left, node->right, wsp);
	  }
      }
  };
  void recursive_decomp(const Node left, const Node right,
			std::vector<WSP<Point> > &wsp)
  {
    double max_radius, dist, radius_l, radius_r;

    radius_l=left->length;
    radius_r=right->length;
    dist = dist_sq_between_quad_boxes(left,
				      right);

    max_radius = (radius_l>radius_r)?radius_l:radius_r;
    if(dist > (spread*max_radius))
      {
	WSP<Point> c(left, right);
	wsp.push_back(c);
	return;
      }
    
    if(radius_l == max_radius)
      {
	if(left->left->length == 0)
	  {
	    recursive_decomp_p(left->left, right, wsp);
	  }
	else
	  recursive_decomp(left->left, right, wsp);

	if(left->right->length == 0)
	  {
	    recursive_decomp_p(left->right, right, wsp);
	  }
	else
	  recursive_decomp(left->right, right, wsp);
	return;
      }
    if(right->left->length == 0)
      {
	recursive_decomp_p(right->left, left, wsp);
      }
    else
      recursive_decomp(right->left, left, wsp);

    if(right->right->length == 0)
      {
	recursive_decomp_p(right->right, left, wsp);
      }
    else
      recursive_decomp(right->right, left, wsp);
    return;
  };

  void recursive_decomp_p(const Node p, const Node node,
			  std::vector<WSP<Point> > &wsp)
  {
    
    double dist;
    double radius;
    dist = dist_sq_between_quad_boxes(p, node);
    radius = node->length;
    if(dist > (spread*radius))
      {
	WSP<Point> c(p, node);
	wsp.push_back(c);
	return;
      }
    if(node->left->length == 0)
      {
	WSP<Point> c(p, node->left);
	wsp.push_back(c);
      }
    else
      {
	recursive_decomp_p(p, node->left, wsp);
      }

    if(node->right->length == 0)
      {
	WSP<Point> c(p, node->right);
	wsp.push_back(c);
      }
    else
      {
	recursive_decomp_p(p, node->right, wsp);
      }
  };

};
#endif
