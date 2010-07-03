//=======================================================================
// Copyright 1997-2001 University of Notre Dame.
// Authors: Andrew Lumsdaine, Lie-Quan Lee, Jeremy G. Siek
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//=======================================================================

#include <boost/config.hpp>
#include <iostream>
#include <vector>
#include <boost/graph/strong_components.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/graph_utility.hpp>
/*
  Sample output:
  A directed graph:
  a --> b f h 
  b --> c a 
  c --> d b 
  d --> e 
  e --> d 
  f --> g 
  g --> f d 
  h --> i 
  i --> h j e c 
  j --> 

  Total number of components: 4
  Vertex a is in component 3
  Vertex b is in component 3
  Vertex c is in component 3
  Vertex d is in component 0
  Vertex e is in component 0
  Vertex f is in component 1
  Vertex g is in component 1
  Vertex h is in component 3
  Vertex i is in component 3
  Vertex j is in component 2
 */

int main(int, char*[])
{
  using namespace boost;
  const char* name = "abcdefghij";

  GraphvizDigraph G;
  read_graphviz("scc.dot", G);

  std::cout << "A directed graph:" << std::endl;
  print_graph(G, name);
  std::cout << std::endl;

  typedef graph_traits<GraphvizGraph>::vertex_descriptor Vertex;
    
  std::vector<int> component(num_vertices(G)), discover_time(num_vertices(G));
  std::vector<default_color_type> color(num_vertices(G));
  std::vector<Vertex> root(num_vertices(G));
  int num = strong_components(G, &component[0], 
                              root_map(&root[0]).
                              color_map(&color[0]).
                              discover_time_map(&discover_time[0]));
    
  std::cout << "Total number of components: " << num << std::endl;
  std::vector<int>::size_type i;
  for (i = 0; i != component.size(); ++i)
    std::cout << "Vertex " << name[i]
         <<" is in component " << component[i] << std::endl;
    
  return 0;
}
