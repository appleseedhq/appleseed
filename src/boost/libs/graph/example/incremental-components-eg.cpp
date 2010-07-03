//=======================================================================
// Copyright 2001 Jeremy G. Siek, Andrew Lumsdaine, Lie-Quan Lee, 
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
#include <boost/graph/adjacency_list.hpp>
#include <boost/pending/disjoint_sets.hpp>
#include <boost/graph/incremental_components.hpp>

int
main(int, char *[])
{
  using namespace boost;
  // Create a graph
  typedef adjacency_list < vecS, vecS, undirectedS > Graph;
  typedef graph_traits < Graph >::vertex_descriptor Vertex;
  const int N = 6;
  Graph G(N);
  add_edge(0, 1, G);
  add_edge(1, 4, G);
  // create the disjoint-sets object, which requires rank and parent vertex properties
  std::vector < Vertex > rank(num_vertices(G));
  std::vector < Vertex > parent(num_vertices(G));
  typedef graph_traits<Graph>::vertices_size_type* Rank;
  typedef Vertex* Parent;
  disjoint_sets < Rank, Parent > ds(&rank[0], &parent[0]);

  // determine the connected components, storing the results in the disjoint-sets object
  initialize_incremental_components(G, ds);
  incremental_components(G, ds);

  // Add a couple more edges and update the disjoint-sets
  graph_traits < Graph >::edge_descriptor e;
  bool flag;
  tie(e, flag) = add_edge(4, 0, G);
  ds.union_set(4, 0);
  tie(e, flag) = add_edge(2, 5, G);
  ds.union_set(2, 5);

  graph_traits < Graph >::vertex_iterator iter, end;
  for (tie(iter, end) = vertices(G); iter != end; ++iter)
    std::cout << "representative[" << *iter << "] = " <<
      ds.find_set(*iter) << std::endl;;
  std::cout << std::endl;

  typedef component_index < unsigned int >Components;
  Components components(parent.begin(), parent.end());
  for (Components::size_type i = 0; i < components.size(); ++i) {
    std::cout << "component " << i << " contains: ";
    for (Components::value_type::iterator j = components[i].begin();
         j != components[i].end(); ++j)
      std::cout << *j << " ";
    std::cout << std::endl;
  }

  return EXIT_SUCCESS;
}
