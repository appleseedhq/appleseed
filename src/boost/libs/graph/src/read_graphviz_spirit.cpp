// Copyright 2004-5 Trustees of Indiana University

// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//
// read_graphviz_spirit.hpp - 
//   Initialize a model of the BGL's MutableGraph concept and an associated
//  collection of property maps using a graph expressed in the GraphViz
// DOT Language.  
//
//   Based on the grammar found at:
//   http://www.graphviz.org/cvs/doc/info/lang.html
//
//   See documentation for this code at: 
//     http://www.boost.org/libs/graph/doc/read-graphviz.html
//

// Authors: Ronald Garcia and Douglas Gregor
//

#define BOOST_GRAPH_SOURCE

#ifndef BOOST_GRAPH_READ_GRAPHVIZ_ITERATORS
#  define BOOST_GRAPH_READ_GRAPHVIZ_ITERATORS
#endif
#include <boost/graph/graphviz.hpp>

namespace boost { namespace detail { namespace graph { 

BOOST_GRAPH_DECL
bool read_graphviz(std::istream& in, mutate_graph& graph) 
{
  using namespace boost;
  using namespace boost::spirit::classic;

  typedef std::istream_iterator<char> is_t;
  typedef multi_pass<is_t> iterator_t;

  iterator_t first(make_multi_pass(is_t(in)));
  iterator_t last(make_multi_pass(is_t()));

  // Turn off white space skipping on the stream
  in.unsetf(std::ios::skipws);

  typedef skip_parser_iteration_policy< boost::detail::graph::dot_skipper>
    iter_policy_t;
  typedef scanner_policies<iter_policy_t> scanner_policies_t;
  typedef scanner<iterator_t, scanner_policies_t> scanner_t;

  boost::detail::graph::dot_grammar p(graph);
  boost::detail::graph::dot_skipper skip_p;

  iter_policy_t iter_policy(skip_p);
  scanner_policies_t policies(iter_policy);

  scanner_t scan(first, last, policies);

  return p.parse(scan);
}

} } } // end namespace boost::detail::graph
