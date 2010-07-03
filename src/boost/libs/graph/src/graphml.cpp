// Copyright (C) 2006  Tiago de Paula Peixoto <tiago@forked.de>
// Copyright (C) 2004  The Trustees of Indiana University.
//
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  Authors: Douglas Gregor
//           Andrew Lumsdaine
//           Tiago de Paula Peixoto

#include <boost/variant.hpp>
#include <expat.h>
#include <boost/graph/graphml.hpp>
#include <string>

using namespace boost;

class graphml_reader
{
public:
    graphml_reader(mutate_graph& g) 
        : m_g(g), m_canonical_vertices(false) { }
    
    void run(std::istream& in)
    {
        const int buffer_size = 4096;
        XML_Parser parser = XML_ParserCreateNS(0,'|');
        XML_SetElementHandler(parser, &on_start_element, &on_end_element);
        XML_SetCharacterDataHandler(parser, &on_character_data);
        XML_SetUserData(parser, this);
        char buffer[buffer_size];

        bool okay = true;
        do 
        {
          in.read(buffer, buffer_size);

          okay = XML_Parse(parser, buffer, in.gcount(), in.gcount() == 0);
        } 
        while (okay && in.good());

        if (!okay) 
        {
            std::stringstream s;
            s << "Parse error: " << XML_ErrorString(XML_GetErrorCode(parser))
              << " on line " << XML_GetCurrentLineNumber(parser) 
              <<", column " << XML_GetCurrentColumnNumber(parser);
            XML_ParserFree(parser);
            throw parse_error(s.str());
        }
        XML_ParserFree(parser);
    }

private:
    /// The kinds of keys. Not all of these are supported
    enum key_kind { 
        graph_key, 
        node_key, 
        edge_key,
        hyperedge_key,
        port_key,
        endpoint_key, 
        all_key
    };

    static void 
    on_start_element(void* user_data, const XML_Char *c_name,
                     const XML_Char **atts)
    {
        graphml_reader* self = static_cast<graphml_reader*>(user_data);

        std::string name(c_name);
        std::string::size_type pos = name.find('|');
        if (pos != std::string::npos)
          name.erase(0, pos+1);

        if (name == "key") 
        {
            std::string id;
            std::string key_name;
            std::string key_type;
            key_kind kind = all_key;

            while (*atts) 
            {
                std::string name = *atts++;
                std::string value = *atts++;

                if (name == "id") id = value;
                else if (name == "attr.name") key_name = value;
                else if (name == "attr.type") key_type = value;
                else if (name == "for") 
                {
                    if (value == "graph") kind = graph_key;
                    else if (value == "node") kind = node_key;
                    else if (value == "edge") kind = edge_key;
                    else if (value == "hyperedge") kind = hyperedge_key;
                    else if (value == "port") kind = port_key;
                    else if (value == "endpoint") kind = endpoint_key;
                    else if (value == "all") kind = all_key;
                    else 
                    {
                        throw parse_error("unrecognized key kind '" + value + "'");
                    }
                }
            }

            self->m_keys[id] = kind;
            self->m_key_name[id] = key_name;
            self->m_key_type[id] = key_type;
            self->m_active_key = id;
        } 
        else if (name == "node") 
        {
            std::string id;

            while (*atts) 
            {
                std::string name = *atts++;
                std::string value = *atts++;
                
                if (name == "id") id = value;
            }

            self->handle_vertex(id);
            self->m_active_descriptor = id;
        } 
        else if (name == "edge") 
        {
            std::string id;
            std::string source, target;
            while (*atts) 
            {
                std::string name = *atts++;
                std::string value = *atts++;

                if (name == "id") id = value;
                else if (name == "source") source = value;
                else if (name == "target") target = value;
                else if (name == "directed") 
                {
                    bool edge_is_directed = (value == "directed");
                    if (edge_is_directed != self->m_g.is_directed()) 
                    {
                        if (edge_is_directed) 
                            throw directed_graph_error();
                        else
                            throw undirected_graph_error();
                    }
                }
            }

            self->m_active_descriptor = self->m_edge.size();
            self->handle_edge(source, target);
        } 
        else if (name == "graph") 
        {
            while (*atts) 
            {
                std::string name = *atts++;
                std::string value = *atts++;
                
                if (name == "edgedefault") 
                {
                    bool edge_is_directed = (value == "directed");
                    if (edge_is_directed != self->m_g.is_directed()) 
                    {
                        if (edge_is_directed) 
                            throw directed_graph_error();
                        else
                            throw undirected_graph_error();
                    }
                }
                else if (name == "parse.nodeids")
                {
                    self->m_canonical_vertices = (value == "canonical");
                }
            }
            self->m_active_descriptor = "";
        } 
        else if (name == "data") 
        {
            while (*atts) 
            {
                std::string name = *atts++;
                std::string value = *atts++;

                if (name == "key") self->m_active_key = value;
            }
        }

        self->m_character_data.clear();
    }
    
    static void
    on_end_element(void* user_data, const XML_Char *c_name)
    {
        graphml_reader* self = static_cast<graphml_reader*>(user_data);

        std::string name(c_name);
        std::string::size_type pos = name.find('|');
        if (pos != std::string::npos)
          name.erase(0, pos+1);

        if (name == "data") 
        {            
            self->handle_property(self->m_active_key, self->m_active_descriptor,
                                  self->m_character_data);
        } 
        else if (name == "default")
        {
            self->m_key_default[self->m_active_key] = self->m_character_data;
        }
    }

    static void
    on_character_data(void* user_data, const XML_Char* s, int len)
    {
        graphml_reader* self = static_cast<graphml_reader*>(user_data);
        self->m_character_data.append(s, len);
    }

    void 
    handle_vertex(const std::string& v)
    {
        bool is_new = false;

        if (m_canonical_vertices)
        {
            size_t id;

            //strip leading "n" from name
            try 
            {
                id = lexical_cast<size_t>(std::string(v,1));
            }
            catch (bad_lexical_cast)
            {
                throw parse_error("invalid vertex: " + v);
            }
            
            while(id >= m_canonical_vertex.size())
            {
                m_canonical_vertex.push_back(m_g.do_add_vertex());
                is_new = true;
            }
        }
        else
        {
            if (m_vertex.find(v) == m_vertex.end())
            {
                m_vertex[v] = m_g.do_add_vertex();
                is_new = true;
            }
        }

        if (is_new)
        {
            std::map<std::string, std::string>::iterator iter;
            for (iter = m_key_default.begin(); iter != m_key_default.end(); ++iter)
            {
                if (m_keys[iter->first] == node_key)
                    handle_property(iter->first, v, iter->second);
            }
        }
    }

    any
    get_vertex_descriptor(const std::string& v)
    {
        if (m_canonical_vertices)
        {
            //strip leading "n" from name
            size_t id = lexical_cast<size_t>(std::string(v,1));
            return m_canonical_vertex[id];
        }
        else
        {            
            return m_vertex[v];
        }
    }

    void 
    handle_edge(const std::string& u, const std::string& v)
    {
        handle_vertex(u);
        handle_vertex(v);

        any source, target;
        source = get_vertex_descriptor(u);
        target = get_vertex_descriptor(v);

        any edge;
        bool added;
        tie(edge, added) = m_g.do_add_edge(source, target);
        if (!added)
            throw bad_parallel_edge(u, v);

        size_t e = m_edge.size();
        m_edge.push_back(edge);
        
        std::map<std::string, std::string>::iterator iter;
        for (iter = m_key_default.begin(); iter != m_key_default.end(); ++iter)
        {
            if (m_keys[iter->first] == edge_key)
                handle_property(iter->first, e, iter->second);
        }
    }

    void handle_property(const std::string& key_id, const variant<std::string,size_t>& descriptor, const std::string& value)
    {
        if (get<std::string>(&descriptor))
        {
            if (get<std::string>(descriptor) == "")
                m_g.set_graph_property(m_key_name[key_id], value, m_key_type[key_id]);
            else
                m_g.set_vertex_property(m_key_name[key_id], get_vertex_descriptor(get<std::string>(descriptor)), value, m_key_type[key_id]);
        }
        else
        {
            m_g.set_edge_property(m_key_name[key_id], get_edge_descriptor(get<size_t>(descriptor)), value, m_key_type[key_id]);
        }
    }

    any
    get_edge_descriptor(size_t e)
    {
        return m_edge[e];
    }

    mutate_graph& m_g;
    std::map<std::string, key_kind> m_keys;
    std::map<std::string, std::string> m_key_name;
    std::map<std::string, std::string> m_key_type;
    std::map<std::string, std::string> m_key_default;
    std::map<std::string, any> m_vertex;
    std::vector<any> m_canonical_vertex;
    std::vector<any> m_edge;
    variant<std::string, size_t> m_active_descriptor;
    std::string m_active_key;
    std::string m_character_data;
    bool m_canonical_vertices;
    bool m_canonical_edges;
};

namespace boost
{
void
read_graphml(std::istream& in, mutate_graph& g)
{    
    graphml_reader reader(g);
    reader.run(in);
}
}
