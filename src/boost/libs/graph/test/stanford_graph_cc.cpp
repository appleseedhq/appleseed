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
#include <boost/graph/stanford_graph.hpp>

int main(int,char*[])
{
  using namespace boost;
  // Check Stanford GraphBase Graph
  {
    typedef Graph* Graph;
    typedef graph_traits<Graph>::vertex_descriptor Vertex;
    typedef graph_traits<Graph>::edge_descriptor Edge;
    function_requires< VertexListGraphConcept<Graph> >();
    function_requires< IncidenceGraphConcept<Graph> >();
    function_requires< AdjacencyGraphConcept<Graph> >();
    function_requires< PropertyGraphConcept<Graph, Edge, edge_length_t > >();
    function_requires< 
      PropertyGraphConcept<Graph, Vertex, u_property<Vertex> > >();
    function_requires< 
      PropertyGraphConcept<Graph, Edge, a_property<Vertex> > >();
  }
  {
    typedef const Graph* Graph;
    typedef graph_traits<Graph>::vertex_descriptor Vertex;
    typedef graph_traits<Graph>::edge_descriptor Edge;
    function_requires< VertexListGraphConcept<Graph> >();
    function_requires< IncidenceGraphConcept<Graph> >();
    function_requires< AdjacencyGraphConcept<Graph> >();
    function_requires< 
      ReadablePropertyGraphConcept<Graph, Edge, edge_length_t > >();
    function_requires< 
      ReadablePropertyGraphConcept<Graph, Vertex, u_property<Vertex> > >();
    function_requires< 
      ReadablePropertyGraphConcept<Graph, Edge, a_property<Vertex> > >();
  }
  return 0;
}
