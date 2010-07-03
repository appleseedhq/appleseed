//=======================================================================
// Copyright 2001 Jeremy G. Siek, Andrew Lumsdaine, Lie-Quan Lee, 
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//=======================================================================
#include <fstream>              // for file I/O
#include <boost/graph/graphviz.hpp>     // for read/write_graphviz()
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/lexical_cast.hpp>
int
main()
{
  using namespace boost;
  GraphvizDigraph g_dot;
  read_graphviz("figs/ospf-graph.dot", g_dot);

  typedef adjacency_list < vecS, vecS, directedS, no_property,
    property < edge_weight_t, int > > Graph;
  typedef graph_traits < Graph >::vertex_descriptor vertex_descriptor;
  Graph g(num_vertices(g_dot));
  property_map < GraphvizDigraph, edge_attribute_t >::type
    edge_attr_map = get(edge_attribute, g_dot);
  graph_traits < GraphvizDigraph >::edge_iterator ei, ei_end;
  for (tie(ei, ei_end) = edges(g_dot); ei != ei_end; ++ei) {
    int weight = lexical_cast < int >(edge_attr_map[*ei]["label"]);
    property < edge_weight_t, int >edge_property(weight);
    add_edge(source(*ei, g_dot), target(*ei, g_dot), edge_property, g);
  }

  vertex_descriptor router_six;
  property_map < GraphvizDigraph, vertex_attribute_t >::type
    vertex_attr_map = get(vertex_attribute, g_dot);
  graph_traits < GraphvizDigraph >::vertex_iterator vi, vi_end;
  for (tie(vi, vi_end) = vertices(g_dot); vi != vi_end; ++vi)
    if ("RT6" == vertex_attr_map[*vi]["label"]) {
      router_six = *vi;
      break;
    }

  std::vector < vertex_descriptor > parent(num_vertices(g));
  // All vertices start out as there own parent
  typedef graph_traits < Graph >::vertices_size_type size_type;
  for (size_type p = 0; p < num_vertices(g); ++p)
    parent[p] = p;

#if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
  std::vector<int> distance(num_vertices(g));
  property_map<Graph, edge_weight_t>::type weightmap = get(edge_weight, g);
  property_map<Graph, vertex_index_t>::type indexmap = get(vertex_index, g);
  dijkstra_shortest_paths
    (g, router_six, &parent[0], &distance[0], weightmap,
     indexmap, std::less<int>(), closed_plus<int>(), 
     (std::numeric_limits<int>::max)(), 0, default_dijkstra_visitor());
#else
  dijkstra_shortest_paths(g, router_six, predecessor_map(&parent[0]));
#endif

  graph_traits < GraphvizDigraph >::edge_descriptor e;
  for (size_type i = 0; i < num_vertices(g); ++i)
    if (parent[i] != i) {
      e = edge(parent[i], i, g_dot).first;
      edge_attr_map[e]["color"] = "black";
    }

#if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
  // VC++ can't handle write_graphviz :(
  {
    std::ofstream out("figs/ospf-sptree.dot");
    out << "digraph loops {\n"
        << "size=\"3,3\"\n"
        << "ratio=\"fill\"\n"
        << "shape=\"box\"\n";
    graph_traits<Graph>::vertex_iterator vi, vi_end;
    for (tie(vi, vi_end) = vertices(g); vi != vi_end; ++vi) {
      out << *vi << "[";
      for (std::map<std::string,std::string>::iterator ai = vattr_map[*vi].begin();
           ai != vattr_map[*vi].end(); ++ai) {
        out << ai->first << "=" << ai->second;
        if (next(ai) != vattr_map[*vi].end())
          out << ", ";
      }
      out<< "]";
    }

    for (tie(ei, ei_end) = edges(g); ei != ei_end; ++ei) {
      out << source(*ei, g) << " -> " << target(*ei, g) << "[";
      std::map<std::string,std::string>& attr_map = eattr_map[*ei];
      for (std::map<std::string,std::string>::iterator eai = attr_map.begin();
           eai != attr_map.end(); ++eai) {
        out << eai->first << "=" << eai->second;
        if (next(eai) != attr_map.end())
          out << ", ";
      }
      out<< "]";
    }
    out << "}\n";
  }
#else
  graph_property < GraphvizDigraph, graph_edge_attribute_t >::type &
    graph_edge_attr_map = get_property(g_dot, graph_edge_attribute);
  graph_edge_attr_map["color"] = "grey";
  write_graphviz("figs/ospf-sptree.dot", g_dot);
#endif

  std::ofstream rtable("routing-table.dat");
  rtable << "Dest    Next Hop    Total Cost" << std::endl;
  for (tie(vi, vi_end) = vertices(g_dot); vi != vi_end; ++vi)
    if (parent[*vi] != *vi) {
      rtable << vertex_attr_map[*vi]["label"] << "    ";
      vertex_descriptor v = *vi, child;
      int path_cost = 0;
      property_map < Graph, edge_weight_t >::type
        weight_map = get(edge_weight, g);
      do {
        path_cost += get(weight_map, edge(parent[v], v, g).first);
        child = v;
        v = parent[v];
      } while (v != parent[v]);
      rtable << vertex_attr_map[child]["label"] << "     ";
      rtable << path_cost << std::endl;

    }

  return EXIT_SUCCESS;
}
