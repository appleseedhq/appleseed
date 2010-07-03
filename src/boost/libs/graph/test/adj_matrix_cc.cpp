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
#include <boost/graph/adjacency_matrix.hpp>

int main(int,char*[])
{
  using namespace boost;
  // Check adjacency_matrix without properties
  {
    typedef adjacency_matrix<directedS> Graph;
    function_requires< VertexListGraphConcept<Graph> >();
    function_requires< EdgeListGraphConcept<Graph> >();
    function_requires< IncidenceGraphConcept<Graph> >();
    function_requires< AdjacencyGraphConcept<Graph> >();
    function_requires< MutableGraphConcept<Graph> >();
    function_requires< AdjacencyMatrixConcept<Graph> >();
  }
  {
    typedef adjacency_matrix<undirectedS> Graph;
    function_requires< VertexListGraphConcept<Graph> >();
    function_requires< EdgeListGraphConcept<Graph> >();
    function_requires< IncidenceGraphConcept<Graph> >();
    function_requires< AdjacencyGraphConcept<Graph> >();
    function_requires< MutableGraphConcept<Graph> >();
    function_requires< AdjacencyMatrixConcept<Graph> >();
  }
  // Check adjacency_matrix with properties
  {
    typedef adjacency_matrix<directedS, 
      property<vertex_color_t, int>,
      property<edge_weight_t, float> > Graph;
    typedef graph_traits<Graph>::vertex_descriptor Vertex;
    typedef graph_traits<Graph>::edge_descriptor Edge;
    function_requires< VertexListGraphConcept<Graph> >();
    function_requires< EdgeListGraphConcept<Graph> >();
    function_requires< IncidenceGraphConcept<Graph> >();
    function_requires< AdjacencyGraphConcept<Graph> >();
    function_requires< AdjacencyMatrixConcept<Graph> >();
    function_requires< VertexMutablePropertyGraphConcept<Graph> >();
    function_requires< EdgeMutablePropertyGraphConcept<Graph> >();
    function_requires< ReadablePropertyGraphConcept<Graph, 
      Vertex, vertex_index_t> >();
    function_requires< PropertyGraphConcept<Graph, Vertex, vertex_color_t> >();
    function_requires< PropertyGraphConcept<Graph, Edge, edge_weight_t> >();
  }
  {
    typedef adjacency_matrix<undirectedS, 
      property<vertex_color_t, int>,
      property<edge_weight_t, float> > Graph;
    typedef graph_traits<Graph>::vertex_descriptor Vertex;
    typedef graph_traits<Graph>::edge_descriptor Edge;
    function_requires< VertexListGraphConcept<Graph> >();
    function_requires< EdgeListGraphConcept<Graph> >();
    function_requires< IncidenceGraphConcept<Graph> >();
    function_requires< AdjacencyGraphConcept<Graph> >();
    function_requires< AdjacencyMatrixConcept<Graph> >();
    function_requires< VertexMutablePropertyGraphConcept<Graph> >();
    function_requires< EdgeMutablePropertyGraphConcept<Graph> >();
    function_requires< ReadablePropertyGraphConcept<Graph, 
      Vertex, vertex_index_t> >();
    function_requires< PropertyGraphConcept<Graph, Vertex, vertex_color_t> >();
    function_requires< PropertyGraphConcept<Graph, Edge, edge_weight_t> >();
  }
  return 0;
}
