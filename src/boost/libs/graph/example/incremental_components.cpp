//=======================================================================
// Copyright 1997, 1998, 1999, 2000 University of Notre Dame.
// Authors: Andrew Lumsdaine, Lie-Quan Lee, Jeremy G. Siek
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//=======================================================================
#include <boost/config.hpp>
#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>
#include <boost/graph/graph_utility.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/pending/disjoint_sets.hpp>
#include <boost/graph/incremental_components.hpp>

/*

  This example shows how to use the disjoint set data structure
  to compute the connected components of an undirected, changing
  graph.

  Sample output:

  An undirected graph:
  0 <--> 1 4 
  1 <--> 0 4 
  2 <--> 5 
  3 <--> 
  4 <--> 1 0 
  5 <--> 2 

  representative[0] = 1
  representative[1] = 1
  representative[2] = 5
  representative[3] = 3
  representative[4] = 1
  representative[5] = 5

  component 0 contains: 4 1 0 
  component 1 contains: 3 
  component 2 contains: 5 2 

 */

using namespace std;

int main(int , char* []) 
{
  using namespace boost;
  typedef adjacency_list <vecS, vecS, undirectedS> Graph;
  typedef graph_traits<Graph>::vertex_descriptor Vertex;
  typedef graph_traits<Graph>::vertices_size_type size_type;

  const int N = 6;
  Graph G(N);

  std::vector<size_type> rank(num_vertices(G));
  std::vector<Vertex> parent(num_vertices(G));
  typedef size_type* Rank;
  typedef Vertex* Parent;
  disjoint_sets<Rank, Parent>  ds(&rank[0], &parent[0]);

  initialize_incremental_components(G, ds);
  incremental_components(G, ds);

  graph_traits<Graph>::edge_descriptor e;
  bool flag;
  boost::tie(e,flag) = add_edge(0, 1, G);
  ds.union_set(0,1);

  boost::tie(e,flag) = add_edge(1, 4, G);
  ds.union_set(1,4);

  boost::tie(e,flag) = add_edge(4, 0, G);
  ds.union_set(4,0);

  boost::tie(e,flag) = add_edge(2, 5, G);
  ds.union_set(2,5);
    
  cout << "An undirected graph:" << endl;
  print_graph(G, get(vertex_index, G));
  cout << endl;
    
  graph_traits<Graph>::vertex_iterator i,end;
  for (boost::tie(i, end) = vertices(G); i != end; ++i)
    cout << "representative[" << *i << "] = " << 
      ds.find_set(*i) << endl;;
  cout << endl;

  typedef component_index<unsigned int> Components;
  Components components(&parent[0], &parent[0] + parent.size());

  for (Components::size_type c = 0; c < components.size(); ++c) {
    cout << "component " << c << " contains: ";
    Components::value_type::iterator
      j = components[c].begin(),
      jend = components[c].end();
    for ( ; j != jend; ++j)
      cout << *j << " ";
    cout << endl;
  }

  return 0;
}

