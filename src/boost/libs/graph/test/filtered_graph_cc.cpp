//=======================================================================
// Copyright 1997, 1998, 1999, 2000 University of Notre Dame.
// Authors: Andrew Lumsdaine, Lie-Quan Lee, Jeremy G. Siek
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//=======================================================================
#include <boost/graph/graph_concepts.hpp>
#include <boost/graph/graph_archetypes.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/filtered_graph.hpp>

int main(int,char*[])
{
  using namespace boost;
  // Check filtered_graph
  {
    typedef adjacency_list<vecS, vecS, directedS, 
      no_property, property<edge_residual_capacity_t, long> > Graph;
    typedef property_map<Graph, edge_residual_capacity_t>::type ResCapMap;
    typedef filtered_graph<Graph, is_residual_edge<ResCapMap> > ResGraph;
    typedef graph_traits<ResGraph>::edge_descriptor Edge;

    function_requires< VertexListGraphConcept<ResGraph> >();
    function_requires< EdgeListGraphConcept<ResGraph> >();
    function_requires< IncidenceGraphConcept<ResGraph> >();
    function_requires< AdjacencyGraphConcept<ResGraph> >();
    function_requires< PropertyGraphConcept<ResGraph, Edge, 
      edge_residual_capacity_t> >();
  }
  // Check filtered_graph with bidirectional adjacency_list
  {
    typedef adjacency_list<vecS, vecS, bidirectionalS, 
      no_property, property<edge_residual_capacity_t, long> > Graph;
    typedef property_map<Graph, edge_residual_capacity_t>::type ResCapMap;
    typedef filtered_graph<Graph, is_residual_edge<ResCapMap> > ResGraph;
    function_requires< BidirectionalGraphConcept<ResGraph> >();
  }
  return 0;
}
