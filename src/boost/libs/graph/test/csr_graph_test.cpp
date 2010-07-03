// Copyright 2005 The Trustees of Indiana University.

// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  Authors: Jeremiah Willcock
//           Douglas Gregor
//           Andrew Lumsdaine

// The libstdc++ debug mode makes this test run for hours...
#ifdef _GLIBCXX_DEBUG
#  undef _GLIBCXX_DEBUG
#endif

// Test for the compressed sparse row graph type
#include <boost/graph/compressed_sparse_row_graph.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/erdos_renyi_generator.hpp>
#include <boost/graph/graph_utility.hpp>
#include <boost/random/linear_congruential.hpp>
#include <cassert>
#include <iostream>
#include <vector>
#include <algorithm>
#include <ctime>
#include <boost/lexical_cast.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/limits.hpp>
#include <string>
#include <boost/graph/iteration_macros.hpp>
#include <boost/test/minimal.hpp>

// Algorithms to test against
#include <boost/graph/betweenness_centrality.hpp>
#include <boost/graph/kruskal_min_spanning_tree.hpp>

typedef boost::adjacency_list<> GraphT;
typedef boost::erdos_renyi_iterator<boost::minstd_rand, GraphT> ERGen;

struct VertexData 
{
  int index;
};

typedef boost::compressed_sparse_row_graph<boost::directedS, VertexData>
  CSRGraphT;

template <class G1, class VI1, class G2, class VI2, class IsomorphismMap>
void assert_graphs_equal(const G1& g1, const VI1& vi1,
                         const G2& g2, const VI2& vi2,
                         const IsomorphismMap& iso) {
  using boost::out_degree;

  BOOST_CHECK (num_vertices(g1) == num_vertices(g2));
  BOOST_CHECK (num_edges(g1) == num_edges(g2));

  typedef typename boost::graph_traits<G1>::vertex_iterator vertiter1;
  {
    vertiter1 i, iend;
    for (boost::tie(i, iend) = vertices(g1); i != iend; ++i) {
      typename boost::graph_traits<G1>::vertex_descriptor v1 = *i;
      typename boost::graph_traits<G2>::vertex_descriptor v2 = iso[v1];

      BOOST_CHECK (vi1[v1] == vi2[v2]);

      BOOST_CHECK (out_degree(v1, g1) == out_degree(v2, g2));
      std::vector<std::size_t> edges1(out_degree(v1, g1));
      typename boost::graph_traits<G1>::out_edge_iterator oe1, oe1end;
      for (boost::tie(oe1, oe1end) = out_edges(v1, g1); oe1 != oe1end; ++oe1) {
        BOOST_CHECK (source(*oe1, g1) == v1);
        edges1.push_back(vi1[target(*oe1, g1)]);
      }
      std::vector<std::size_t> edges2(out_degree(v2, g2));
      typename boost::graph_traits<G2>::out_edge_iterator oe2, oe2end;
      for (boost::tie(oe2, oe2end) = out_edges(v2, g2); oe2 != oe2end; ++oe2) {
        BOOST_CHECK (source(*oe2, g2) == v2);
        edges2.push_back(vi2[target(*oe2, g2)]);
      }

      std::sort(edges1.begin(), edges1.end());
      std::sort(edges2.begin(), edges2.end());
      BOOST_CHECK (edges1 == edges2);
    }
  }

  {
    std::vector<std::pair<std::size_t, std::size_t> > all_edges1;
    std::vector<std::pair<std::size_t, std::size_t> > all_edges2;
    typename boost::graph_traits<G1>::edge_iterator ei1, ei1end;
    for (boost::tie(ei1, ei1end) = edges(g1); ei1 != ei1end; ++ei1)
      all_edges1.push_back(std::make_pair(vi1[source(*ei1, g1)],
                                          vi1[target(*ei1, g1)]));
    typename boost::graph_traits<G2>::edge_iterator ei2, ei2end;
    for (boost::tie(ei2, ei2end) = edges(g2); ei2 != ei2end; ++ei2)
      all_edges2.push_back(std::make_pair(vi2[source(*ei2, g2)],
                                          vi2[target(*ei2, g2)]));
    std::sort(all_edges1.begin(), all_edges1.end());
    std::sort(all_edges2.begin(), all_edges2.end());
    BOOST_CHECK (all_edges1 == all_edges2);
  }
}

template<typename GraphT, typename VertexIndexMap>
class edge_to_index_pair
{
  typedef typename boost::graph_traits<GraphT>::vertices_size_type
    vertices_size_type;
  typedef typename boost::graph_traits<GraphT>::edge_descriptor edge_descriptor;

 public:
  typedef std::pair<vertices_size_type, vertices_size_type> result_type;

  edge_to_index_pair() : g(0), index() { }
  edge_to_index_pair(const GraphT& g, const VertexIndexMap& index)
    : g(&g), index(index)
  { }

  result_type operator()(edge_descriptor e) const
  {
    return result_type(get(index, source(e, *g)), get(index, target(e, *g)));
  }

 private:
  const GraphT* g;
  VertexIndexMap index;
};

template<typename GraphT, typename VertexIndexMap>
edge_to_index_pair<GraphT, VertexIndexMap>
make_edge_to_index_pair(const GraphT& g, const VertexIndexMap& index)
{
  return edge_to_index_pair<GraphT, VertexIndexMap>(g, index);
}

template<typename GraphT>
edge_to_index_pair
  <GraphT,
   typename boost::property_map<GraphT,boost::vertex_index_t>::const_type>
make_edge_to_index_pair(const GraphT& g)
{
  typedef typename boost::property_map<GraphT,
                                       boost::vertex_index_t>::const_type
    VertexIndexMap;
  return edge_to_index_pair<GraphT, VertexIndexMap>(g,
                                                   get(boost::vertex_index,
                                                       g));
}

void check_consistency(const CSRGraphT& g) {
  // Do a bunch of tests on the graph internal data
  // Check that m_last_source is valid
  BOOST_CHECK(g.m_last_source <= num_vertices(g));
  // Check that m_rowstart entries are valid, and that entries after
  // m_last_source + 1 are all zero
  BOOST_CHECK(g.m_rowstart[0] == 0);
  for (CSRGraphT::vertices_size_type i = 0; i < g.m_last_source; ++i) {
    BOOST_CHECK(g.m_rowstart[i + 1] >= g.m_rowstart[i]);
    BOOST_CHECK(g.m_rowstart[i + 1] <= num_edges(g));
  }
  for (CSRGraphT::vertices_size_type i = g.m_last_source + 1;
       i < g.m_rowstart.size(); ++i) {
    BOOST_CHECK(g.m_rowstart[i] == 0);
  }
  // Check that m_column entries are within range
  for (CSRGraphT::edges_size_type i = 0; i < num_edges(g); ++i) {
    BOOST_CHECK(g.m_column[i] < num_vertices(g));
  }
}

template<typename OrigGraph>
void test(const OrigGraph& g)
{
  // Check copying of a graph
  CSRGraphT g2(g);
  check_consistency(g2);
  BOOST_CHECK((std::size_t)std::distance(edges(g2).first, edges(g2).second)
              == num_edges(g2));
  assert_graphs_equal(g, boost::identity_property_map(),
                      g2, boost::identity_property_map(),
                      boost::identity_property_map());

  // Check constructing a graph from iterators
  CSRGraphT g3(boost::make_transform_iterator(edges(g2).first,
                                             make_edge_to_index_pair(g2)),
              boost::make_transform_iterator(edges(g2).second,
                                             make_edge_to_index_pair(g2)),
              num_vertices(g));
  check_consistency(g3);
  BOOST_CHECK((std::size_t)std::distance(edges(g3).first, edges(g3).second)
              == num_edges(g3));
  assert_graphs_equal(g2, boost::identity_property_map(),
                      g3, boost::identity_property_map(),
                      boost::identity_property_map());

  // Check constructing a graph using add_edge and add_vertices
  CSRGraphT g4;
  BOOST_CHECK(num_vertices(g4) == 0);
  std::size_t first_vert = add_vertices(num_vertices(g3), g4);
  BGL_FORALL_VERTICES(v, g4, CSRGraphT)
    g4[v].index = v;

  BOOST_CHECK(first_vert == 0);
  BOOST_CHECK(num_vertices(g4) == num_vertices(g3));
  CSRGraphT::edge_iterator ei, ei_end;
  int i;
  for (boost::tie(ei, ei_end) = edges(g3), i = 0; ei != ei_end; ++ei, ++i) {
    CSRGraphT::edge_descriptor e = add_edge(source(*ei, g3), target(*ei, g3), g4);
    BOOST_CHECK(source(e, g4) == source(*ei, g3));
    BOOST_CHECK(target(e, g4) == target(*ei, g3));
    if (i % 13 == 0) check_consistency(g4);
  }
  assert_graphs_equal(g3, boost::identity_property_map(),
                      g4, boost::identity_property_map(),
                      boost::identity_property_map());

  // Check edge_from_index (and implicitly the edge_index property map) for
  // each edge in g2
  // This test also checks for the correct sorting of the edge iteration
  std::size_t last_src = 0, last_tgt = 0;
  for (boost::tie(ei, ei_end) = edges(g2); ei != ei_end; ++ei) {
    BOOST_CHECK(edge_from_index(get(boost::edge_index, g2, *ei), g2) == *ei);
    std::size_t src = get(boost::vertex_index, g2, source(*ei, g2));
    std::size_t tgt = get(boost::vertex_index, g2, target(*ei, g2));
    BOOST_CHECK(src > last_src || (src == last_src && tgt >= last_tgt));
    last_src = src;
    last_tgt = tgt;
  }

  // Check out edge iteration and vertex iteration for sortedness
  // Also, check a few runs of edge and edge_range
  CSRGraphT::vertex_iterator vi, vi_end;
  std::size_t last_vertex = 0;
  bool first_iter = true;
  for (boost::tie(vi, vi_end) = vertices(g2); vi != vi_end; ++vi) {
    std::size_t v = get(boost::vertex_index, g2, *vi);
    BOOST_CHECK(first_iter || v > last_vertex);
    last_vertex = v;
    first_iter = false;

    CSRGraphT::out_edge_iterator oei, oei_end;
    std::size_t last_tgt = 0;
    for (boost::tie(oei, oei_end) = out_edges(*vi, g2); oei != oei_end; ++oei) {
      BOOST_CHECK(source(*oei, g2) == *vi);
      CSRGraphT::vertex_descriptor tgtd = target(*oei, g2);
      std::size_t tgt = get(boost::vertex_index, g2, tgtd);
      BOOST_CHECK(tgt >= last_tgt);
      last_tgt = tgt;

      std::pair<CSRGraphT::edge_descriptor, bool> edge_info = edge(*vi, tgtd, g2);
      BOOST_CHECK(edge_info.second == true);
      BOOST_CHECK(source(edge_info.first, g2) == *vi);
      BOOST_CHECK(target(edge_info.first, g2) == tgtd);
      std::pair<CSRGraphT::out_edge_iterator, CSRGraphT::out_edge_iterator> er =
        edge_range(*vi, tgtd, g2);
      BOOST_CHECK(er.first != er.second);
      for (; er.first != er.second; ++er.first) {
        BOOST_CHECK(source(*er.first, g2) == *vi);
        BOOST_CHECK(target(*er.first, g2) == tgtd);
      }
    }

    // Find a vertex for testing
    CSRGraphT::vertex_descriptor test_vertex = vertex(num_vertices(g2) / 2, g2);
    int edge_count = 0;
    CSRGraphT::out_edge_iterator oei2, oei2_end;
    for (boost::tie(oei2, oei_end) = out_edges(*vi, g2); oei2 != oei_end; ++oei2) {
      if (target(*oei2, g2) == test_vertex)
        ++edge_count;
    }

    // Test edge and edge_range on an edge that may not be present
    std::pair<CSRGraphT::edge_descriptor, bool> edge_info = 
      edge(*vi, test_vertex, g2);
    BOOST_CHECK(edge_info.second == (edge_count != 0));
    std::pair<CSRGraphT::out_edge_iterator, CSRGraphT::out_edge_iterator> er =
      edge_range(*vi, test_vertex, g2);
    BOOST_CHECK(er.second - er.first == edge_count);
  }

  // Run brandes_betweenness_centrality, which touches on a whole lot
  // of things, including VertexListGraph and IncidenceGraph
  using namespace boost;
  std::vector<double> vertex_centralities(num_vertices(g3));
  std::vector<double> edge_centralities(num_edges(g3));
  brandes_betweenness_centrality
    (g3,
     make_iterator_property_map(vertex_centralities.begin(),
                                get(boost::vertex_index, g3)),
     make_iterator_property_map(edge_centralities.begin(),
                                get(boost::edge_index, g3)));
    // Extra qualifications for aCC

  // Invert the edge centralities and use these as weights to
  // Kruskal's MST algorithm, which will test the EdgeListGraph
  // capabilities.
  double max_val = (std::numeric_limits<double>::max)();
  for (std::size_t i = 0; i < num_edges(g3); ++i)
    edge_centralities[i] =
      edge_centralities[i] == 0.0? max_val : 1.0 / edge_centralities[i];

  typedef boost::graph_traits<CSRGraphT>::edge_descriptor edge_descriptor;
  std::vector<edge_descriptor> mst_edges;
  mst_edges.reserve(num_vertices(g3));
  kruskal_minimum_spanning_tree
    (g3, std::back_inserter(mst_edges),
     weight_map(make_iterator_property_map(edge_centralities.begin(),
                                           get(boost::edge_index, g3))));
}

void test(int nnodes, double density, int seed)
{
  boost::minstd_rand gen(seed);
  std::cout << "Testing " << nnodes << " density " << density << std::endl;
  GraphT g(ERGen(gen, nnodes, density), ERGen(), nnodes);
  test(g);
}

void test_graph_properties()
{
  using namespace boost;

  typedef compressed_sparse_row_graph<directedS,
                                      no_property,
                                      no_property,
                                      property<graph_name_t, std::string> >
    CSRGraphT;

  CSRGraphT g;
  BOOST_CHECK(get_property(g, graph_name) == "");
  set_property(g, graph_name, "beep");
  BOOST_CHECK(get_property(g, graph_name) == "beep");
}

struct Vertex
{
  double centrality;
};

struct Edge
{
  Edge(double weight) : weight(weight), centrality(0.0) { }

  double weight;
  double centrality;
};

void test_vertex_and_edge_properties()
{
  using namespace boost;
  typedef compressed_sparse_row_graph<directedS, Vertex, Edge>
    CSRGraphWithPropsT;

  typedef std::pair<int, int> E;
  E edges_init[6] = { E(0, 1), E(0, 3), E(1, 2), E(3, 1), E(3, 4), E(4, 2) };
  double weights[6] = { 1.0, 1.0, 0.5, 1.0, 1.0, 0.5 };
  double centrality[5] = { 0.0, 1.5, 0.0, 1.0, 0.5 };

  CSRGraphWithPropsT g(&edges_init[0], &edges_init[0] + 6, &weights[0], 5, 6);
  brandes_betweenness_centrality
    (g,
     centrality_map(get(&Vertex::centrality, g)).
     weight_map(get(&Edge::weight, g)).
     edge_centrality_map(get(&Edge::centrality, g)));

  BGL_FORALL_VERTICES(v, g, CSRGraphWithPropsT)
    BOOST_CHECK(g[v].centrality == centrality[v]);
}

int test_main(int argc, char* argv[])
{
  // Optionally accept a seed value
  int seed = std::time(0);
  if (argc > 1) seed = boost::lexical_cast<int>(argv[1]);

  std::cout << "Seed = " << seed << std::endl;
  {
    std::cout << "Testing empty graph" << std::endl;
    CSRGraphT g;
    test(g);
  }
  //  test(1000, 0.05, seed);
  //  test(1000, 0.0, seed);
  //  test(1000, 0.1, seed);
  test(1000, 0.001, seed);
  test(1000, 0.0005, seed);
  {
    std::cout << "Testing partially constructed CSR graph" << std::endl;
    CSRGraphT g;
    add_vertices(std::size_t(5), g);
    add_edge(std::size_t(1), std::size_t(2), g);
    check_consistency(g);
    add_edge(std::size_t(2), std::size_t(3), g);
    check_consistency(g);
    add_edge(std::size_t(2), std::size_t(4), g);
    check_consistency(g);
    CSRGraphT::edge_iterator ei, ei_end;
    for (boost::tie(ei, ei_end) = edges(g); ei != ei_end; ++ei) {
      BOOST_CHECK(edge_from_index(get(boost::edge_index, g, *ei), g) == *ei);
    }
    test(g);
  }

  test_graph_properties();
  test_vertex_and_edge_properties();

  return 0;
}
