/*!
* Copyright 2007  Technical University of Catalonia
*
* Use, modification and distribution is subject to the Boost Software
* License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
* http://www.boost.org/LICENSE_1_0.txt)
*
*  Authors: Dmitry Bufistov
*           Andrey Parfenov
*/
#include <boost/graph/howard_cycle_ratio.hpp>
#include <boost/random/mersenne_twister.hpp> 
#include <boost/random/uniform_real.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/iteration_macros.hpp>
#include <boost/graph/graph_utility.hpp>
#include <boost/graph/property_iter_range.hpp>
#include <boost/graph/random.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>


using namespace boost;
typedef adjacency_list<listS, listS, directedS, property<vertex_index_t, int, property<boost::vertex_name_t, std::string> >, 
        property<edge_weight_t, double, property<edge_weight2_t, double, property<edge_index_t, int> > > > grap_real_t;

template <typename TGraph>      
void gen_rand_graph(TGraph& g, size_t nV, size_t nE)
{
        g.clear();
        boost::mt19937 rng;
        boost::generate_random_graph(g, nV, nE, rng, true, true);
        boost::uniform_real<> ur(-1,10); 
        boost::variate_generator<boost::mt19937&, boost::uniform_real<> >       ew1rg(rng, ur);
        randomize_property<edge_weight_t>(g, ew1rg);
        boost::uniform_int<> uint(1,5); 
        boost::variate_generator<boost::mt19937&, boost::uniform_int<> >        ew2rg(rng, uint);
        randomize_property<edge_weight2_t>(g, ew2rg);
}

int main(int argc, char* argv[])
{
        const double epsilon = 0.000000001;
        double min_cr, max_cr; ///Minimum and maximum cycle ratio
        typedef std::vector<graph_traits<grap_real_t>::edge_descriptor> ccReal_t; 
        ccReal_t cc; ///For storing critical edges
        
        
        grap_real_t tgr;
        property_map<grap_real_t, vertex_index_t>::type vim = get(vertex_index, tgr);
        property_map<grap_real_t, edge_weight_t>::type ew1m = get(edge_weight, tgr);
        property_map<grap_real_t, edge_weight2_t>::type ew2m = ew2m;
        
        gen_rand_graph(tgr, 1000, 300000);
        std::cout << "Vertices number: " << num_vertices(tgr) << '\n';
        std::cout << "Edges number: " << num_edges(tgr) << '\n';
        int i = 0;
        BGL_FORALL_VERTICES(vd, tgr, grap_real_t) put(vertex_index, tgr, vd, i++); ///Initialize vertex index property
        boost::posix_time::ptime        st = boost::posix_time::microsec_clock::local_time();
        max_cr = maximum_cycle_ratio(tgr, get(vertex_index, tgr), get(edge_weight, tgr), get(edge_weight2, tgr));
        std::cout << "Maximum cycle ratio is " << max_cr << '\n';
        std::cout << "Run time of the maximum_cycle_ratio() is " << to_simple_string(boost::posix_time::microsec_clock::local_time() - st) << '\n';

        
        ///One way to get the "good" value of the plus_infinity parameter
        double pl_infnt = double(*std::max_element(get_property_iter_range(tgr, edge_weight).first, get_property_iter_range(tgr, edge_weight).second)) / 
                *std::min_element(get_property_iter_range(tgr, edge_weight2).first, get_property_iter_range(tgr, edge_weight2).second);
        std::cout << "Set infinity for minimum_cycle_ratio() call to " << pl_infnt << '\n';
        i = 0;
        BGL_FORALL_EDGES(ed, tgr, grap_real_t) put(edge_index, tgr, ed, i++); ///Initialize edge index property
        min_cr = minimum_cycle_ratio(tgr, get(vertex_index, tgr), get(edge_weight, tgr), get(edge_weight2, tgr), get(edge_index, tgr), &cc, pl_infnt);
        std::cout << "Minimal cycle ratio is " << min_cr << '\n';
        std::pair<double, double> cr(.0,.0);
        std::cout << "\nCritical cycle is:\n";
        for (ccReal_t::iterator itr = cc.begin(); itr != cc.end(); ++itr) 
        {
                cr.first += get(edge_weight, tgr, *itr); cr.second += get(edge_weight2, tgr, *itr);
                std::cout << "(" << get(vertex_index, tgr, source(*itr, tgr)) << "," << get(vertex_index, tgr, target(*itr, tgr)) << ") ";
        }
        std::cout << '\n';
        assert(std::abs(cr.first / cr.second - min_cr) < epsilon);
        
        return 0;
}

