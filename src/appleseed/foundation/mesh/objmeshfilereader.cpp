
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "objmeshfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/vector.h"
#include "foundation/mesh/iobjmeshbuilder.h"
#include "foundation/mesh/objmeshfilelexer.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>
#include <map>
#include <vector>

using namespace std;

namespace foundation
{

//
// OBJMeshFileReader class implementation.
//

namespace
{
    const size_t Undefined = ~size_t(0);
}

struct OBJMeshFileReader::Impl
{
    enum Symbol
    {
        // Special symbols.
        EndOfFile = 256,
        EndOfLine,
        Identifier,
        Integer,
        Double,

        // Keywords.
        D,                  // material transparency (same as TR) -- unused
        F,                  // face
        G,                  // group name
        Illum,              // illumination model
        KA,                 // define the ambient color of a material
        KD,                 // define the diffuse color of a material
        KS,                 // define the specular color of a material
        MapKD,              // reference a diffuse map file
        MtlLib,             // reference a material library file
        NewMtl,             // start defining a new material
        O,                  // start defining a new object
        S,                  // define a smoothing group
        TR,                 // material transparency (same as D) -- unused
        UseMtl,             // apply a given material
        V,                  // define a vertex position
        VN,                 // define a vertex normal
        VT,                 // define a texture vertex
    };

    typedef map<string, Symbol> KeywordTable;
    typedef map<string, size_t> MaterialMap;

    // Read options.
    const Options           m_options;

    // Mesh builder.
    IOBJMeshBuilder*        m_builder;

    // Keyword table.
    KeywordTable            m_keywords;

    // Lexical analyzer.
    OBJMeshFileLexer        m_lexer;

    // Features defined in the file.
    vector<Vector3d>        m_vertices;
    vector<Vector2d>        m_tex_coords;
    vector<Vector3d>        m_normals;

    // Mappings between internal indices and mesh indices.
    vector<size_t>          m_vertex_index_mapping;
    vector<size_t>          m_tex_coord_index_mapping;
    vector<size_t>          m_normal_index_mapping;

    // Temporary vectors for collecting indices while parsing face statements.
    vector<size_t>          m_face_vertex_indices;
    vector<size_t>          m_face_tex_coord_indices;
    vector<size_t>          m_face_normal_indices;

    bool                    m_inside_mesh_def;          // currently inside a mesh definition?
    string                  m_mesh_name;

    MaterialMap             m_materials;
    size_t                  m_current_material;         // index of the material currently in use
    bool                    m_inside_material_def;      // currently inside a material definition?

    // Constructor.
    explicit Impl(const Options options)
      : m_options(options)
    {
        // Construct the keyword table.
        m_keywords["d"]         = D;
        m_keywords["f"]         = F;
        m_keywords["g"]         = G;
        m_keywords["illum"]     = Illum;
        m_keywords["ka"]        = KA;
        m_keywords["kd"]        = KD;
        m_keywords["Ks"]        = KS;
        m_keywords["map_Kd"]    = MapKD;
        m_keywords["mtllib"]    = MtlLib;
        m_keywords["newmtl"]    = NewMtl;
        m_keywords["o"]         = O;
        m_keywords["s"]         = S;
        m_keywords["tr"]        = TR;
        m_keywords["usemtl"]    = UseMtl;
        m_keywords["v"]         = V;
        m_keywords["vn"]        = VN;
        m_keywords["vt"]        = VT;
    }

    // Reset the parser to its initial state.
    void reset()
    {
        m_builder = 0;

        clear_release_memory(m_vertices);
        clear_release_memory(m_tex_coords);
        clear_release_memory(m_normals);

        clear_release_memory(m_vertex_index_mapping);
        clear_release_memory(m_tex_coord_index_mapping);
        clear_release_memory(m_normal_index_mapping);

        clear_release_memory(m_face_vertex_indices);
        clear_release_memory(m_face_tex_coord_indices);
        clear_release_memory(m_face_normal_indices);

        m_inside_mesh_def = false;
        m_mesh_name.clear();

        m_materials.clear();
        m_current_material = Undefined;
        m_inside_material_def = false;
    }

    // Close the input file and throw an ExceptionParseError exception.
    void parse_error()
    {
        const size_t line_number = m_lexer.get_line_number();
        m_lexer.close();
        throw ExceptionParseError(line_number);
    }

    void parse_file()
    {
        while (true)
        {
            m_lexer.eat_blanks();

            // Handle end of file.
            if (m_lexer.is_eof())
                break;

            // Handle empty lines.
            if (m_lexer.is_eol())
            {
                m_lexer.accept_newline();
                continue;
            }

            const string keyword = m_lexer.accept_string();
            const KeywordTable::const_iterator i = m_keywords.find(keyword);

            // Ignore unknown statements.
            if (i == m_keywords.end())
            {
                m_lexer.eat_line();
                continue;
            }

            switch (i->second)
            {
              case F:
                parse_f_statement();
                break;
              case MtlLib:
                parse_mtllib_statement();
                break;
              case O:
              case G:
                parse_o_g_statement();
                break;
              case UseMtl:
                parse_usemtl_statement();
                break;
              case V:
                parse_v_statement();
                break;
              case VN:
                parse_vn_statement();
                break;
              case VT:
                parse_vt_statement();
                break;
              default:
                // Ignore unhandled statements.
                m_lexer.eat_line();
                continue;
            }

            m_lexer.eat_blanks();
            m_lexer.accept_newline();
        }

        // End the definition of the last object.
        if (m_inside_mesh_def)
            m_builder->end_mesh();
    }

    // Convert 1-based indices (including negative indices) to 0-based indices.
    size_t fix_index(const long index, const size_t count)
    {
        if (index > 0)
        {
            const size_t i = static_cast<size_t>(index);
            if (i > count)
                parse_error();
            return i - 1;
        }
        else if (index < 0)
        {
            const size_t i = static_cast<size_t>(-index);
            if (i > count)
                parse_error();
            return count - i;
        }
        else
        {
            parse_error();
            return 0;       // keep the compiler happy
        }
    }

    void parse_f_statement()
    {
        clear_keep_memory(m_face_vertex_indices);
        clear_keep_memory(m_face_tex_coord_indices);
        clear_keep_memory(m_face_normal_indices);

        while (true)
        {
            m_lexer.eat_blanks();

            if (m_lexer.is_eol())
                break;

            //
            // Recognized (epsilon)
            // Accept n(/((n(/n)?)|(/n)))?
            //

            {
                const long n = m_lexer.accept_long();
                const size_t v = fix_index(n, m_vertices.size());
                m_face_vertex_indices.push_back(v);
            }

            //
            // Recognized n
            // Accept (/((n(/n)?)|(/n)))?
            //

            {
                const unsigned char c = m_lexer.get_char();
                if (m_lexer.is_space(c))
                    continue;
                else if (c == '/')
                    m_lexer.next_char();
                else parse_error();
            }

            //
            // Recognized n/
            // Accept (n(/n)?)|(/n)
            //

            {
                const unsigned char c = m_lexer.get_char();
                if (c == '/')
                {
                    m_lexer.next_char();
                    goto terminate;
                }
                else
                {
                    const long n = m_lexer.accept_long();
                    const size_t vt = fix_index(n, m_tex_coords.size());
                    m_face_tex_coord_indices.push_back(vt);
                }
            }

            //
            // Recognized n/n
            // Accept (/n)?
            //

            {
                const unsigned char c = m_lexer.get_char();
                if (m_lexer.is_space(c))
                    continue;
                else if (c == '/')
                    m_lexer.next_char();
                else parse_error();
            }

        terminate:

            //
            // Recognized n//
            // Accept n
            //

            {
                const long n = m_lexer.accept_long();
                const size_t vn = fix_index(n, m_normals.size());
                m_face_normal_indices.push_back(vn);
            }
        }

        // Check whether the face is well-formed.
        const size_t vc = m_face_vertex_indices.size();
        const size_t tc = m_face_tex_coord_indices.size();
        const size_t nc = m_face_normal_indices.size();
        const bool well_formed =
                vc >= 3
            && (tc == 0 || tc == vc)
            && (nc == 0 || nc == vc);

        if (well_formed)
        {
            // The face is well-formed, insert it into the mesh.
            insert_face_into_mesh();
        }
        else
        {
            // The face is ill-formed, ignore it or abort parsing.
            if (m_options & StopOnInvalidFaceDef)
                throw ExceptionInvalidFaceDef(m_lexer.get_line_number());
        }
    }

    static void translate_indices(
        vector<size_t>&         indices,
        const vector<size_t>&   mapping)
    {
        for (size_t i = 0; i < indices.size(); ++i)
            indices[i] = mapping[indices[i]];
    }

    void insert_vertices_into_mesh()
    {
        for (size_t i = 0; i < m_face_vertex_indices.size(); ++i)
        {
            const size_t vertex_index = m_face_vertex_indices[i];
            ensure_size(m_vertex_index_mapping, vertex_index + 1, Undefined);
            if (m_vertex_index_mapping[vertex_index] == Undefined)
                m_vertex_index_mapping[vertex_index] = m_builder->push_vertex(m_vertices[vertex_index]);
        }
    }

    void insert_vertex_normals_into_mesh()
    {
        for (size_t i = 0; i < m_face_normal_indices.size(); ++i)
        {
            const size_t normal_index = m_face_normal_indices[i];
            ensure_size(m_normal_index_mapping, normal_index + 1, Undefined);
            if (m_normal_index_mapping[normal_index] == Undefined)
                m_normal_index_mapping[normal_index] = m_builder->push_vertex_normal(m_normals[normal_index]);
        }
    }

    void insert_tex_coords_into_mesh()
    {
        for (size_t i = 0; i < m_face_tex_coord_indices.size(); ++i)
        {
            const size_t tex_coord_index = m_face_tex_coord_indices[i];
            ensure_size(m_tex_coord_index_mapping, tex_coord_index + 1, Undefined);
            if (m_tex_coord_index_mapping[tex_coord_index] == Undefined)
                m_tex_coord_index_mapping[tex_coord_index] = m_builder->push_tex_coords(m_tex_coords[tex_coord_index]);
        }
    }

    void insert_face_into_mesh()
    {
        if (!m_inside_mesh_def)
        {
            // Begin the definition of the new mesh.
            m_builder->begin_mesh(m_mesh_name);
            m_inside_mesh_def = true;
        }

        // Insert the features into the mesh, updating index mappings as necessary.
        insert_vertices_into_mesh();
        insert_vertex_normals_into_mesh();
        insert_tex_coords_into_mesh();

        // Translate feature indices from internal space to mesh space.
        translate_indices(m_face_vertex_indices, m_vertex_index_mapping);
        translate_indices(m_face_normal_indices, m_normal_index_mapping);
        translate_indices(m_face_tex_coord_indices, m_tex_coord_index_mapping);

        const size_t n = m_face_vertex_indices.size();

        // Begin defining a new face.
        m_builder->begin_face(n); 

        // Set face vertices.
        m_builder->set_face_vertices(&m_face_vertex_indices.front());

        // Set face vertex normals (if any).
        if (m_face_normal_indices.size() == n)
            m_builder->set_face_vertex_normals(&m_face_normal_indices.front());

        // Set face vertex texture coordinates (if any).
        if (m_face_tex_coord_indices.size() == n)
            m_builder->set_face_vertex_tex_coords(&m_face_tex_coord_indices.front());

        // End defining the face.
        m_builder->end_face();
    }

    void parse_mtllib_statement()
    {
        m_lexer.eat_blanks();

#if 0
        load_material_file(m_lexer.accept_string());
#else
        m_lexer.accept_string();
#endif

        while (true)
        {
            m_lexer.eat_blanks();

            if (m_lexer.is_eol())
                break;

#if 0
            load_material_file(m_lexer.accept_string());
#else
            m_lexer.accept_string();
#endif
        }
    }

    void parse_o_g_statement()
    {
        m_lexer.eat_blanks();

        // End the current mesh.
        if (m_inside_mesh_def)
        {
            m_builder->end_mesh();
            m_inside_mesh_def = false;
        }

        if (m_lexer.is_eol())
        {
            // No name was specified.
            m_mesh_name.clear();
        }
        else
        {
            // Get the name of the mesh.
            m_mesh_name = m_lexer.accept_string();
        }
    }

    void parse_usemtl_statement()
    {
        m_lexer.eat_blanks();

        if (m_lexer.is_eol())
        {
            // No material was specified.
            m_current_material = Undefined;
        }
        else
        {
            const string material_name = m_lexer.accept_string();
#if 0
            const string_to_feature_id_map::const_iterator i = m_material_id.find(material_name);
            if (i != m_material_id.end())
            {
                // A material was specified, and it is defined.
                m_current_material = i->second;
            }
            else
#endif
            {
                // A material was specified, but it was not defined.
                m_current_material = Undefined;
            }
        }
    }

    void parse_v_statement()
    {
        Vector3d v;

        m_lexer.eat_blanks();
        v.x = m_lexer.accept_double();

        m_lexer.eat_blanks();
        v.y = m_lexer.accept_double();

        m_lexer.eat_blanks();
        v.z = m_lexer.accept_double();

        m_lexer.eat_blanks();

        if (!m_lexer.is_eol())
            m_lexer.accept_double();

        m_vertices.push_back(v);
    }

    void parse_vt_statement()
    {
        Vector2d v;

        m_lexer.eat_blanks();
        v.x = m_lexer.accept_double();

        m_lexer.eat_blanks();
        v.y = m_lexer.accept_double();

        m_lexer.eat_blanks();
        if (!m_lexer.is_eol())
            m_lexer.accept_double();

        m_tex_coords.push_back(v);
    }

    void parse_vn_statement()
    {
        Vector3d n;

        m_lexer.eat_blanks();
        n.x = m_lexer.accept_double();

        m_lexer.eat_blanks();
        n.y = m_lexer.accept_double();

        m_lexer.eat_blanks();
        n.z = m_lexer.accept_double();

        n = normalize(n);

        m_normals.push_back(n);
    }

#if 0

    void load_material_file(const string &filename)
    {
        //!\todo This is quite dirty, I must admit.
        FILE * const file_copy = m_file;
        const int line_copy = m_line;

        m_file = TryOpeningFile(m_path, filename, "r");

        if (m_file) {
            m_line = 1;

            m_inside_material_def = false;

            parse_material_file();

            if (m_inside_material_def) {
                assert(m_matbuilder);
                m_matbuilder->EndMaterial();
            }
        } else {
            if (m_option_mask & STOP_ON_MISSING_FILE_BIT) {
                //!\todo Close files and free resources.
                throw FileNotFoundException(filename);
            }

            // Ignore missing material file.
        }

        m_file = file_copy;
        m_line = line_copy;
    }

    void parse_material_file()
    {
        while (true)
        {
            m_lexer.eat_blanks();

            // Handle end of file.
            if (m_lexer.is_eof())
                break;

            // Handle empty lines.
            if (m_lexer.is_eol())
            {
                m_lexer.accept_newline();
                continue;
            }

            const string keyword = m_lexer.accept_string();
            const KeywordTable::const_iterator i = m_keywords.find(keyword);

            // Ignore unknown statements.
            if (i == m_keywords.end())
            {
                m_lexer.eat_line();
                continue;
            }

            switch (i->second)
            {
              case KA:
                parse_ka_statement();
                break;
              case KD:
                parse_kd_statement();
                break;
              case KS:
                parse_ks_statement();
                break;
              case MapKD:
                parse_map_kd_statement();
                break;
              case NewMtl:
                parse_newmtl_statement();
                break;
              default:
                // Ignore unhandled statements.
                m_lexer.eat_line();
                continue;
            }

            m_lexer.eat_blanks();
            m_lexer.accept_newline();
        }
    }

    void parse_ka_statement()
    {
        Color3d ambient;

        m_lexer.eat_blanks();
        ambient.r = m_lexer.accept_double();

        m_lexer.eat_blanks();
        ambient.g = m_lexer.accept_double();

        m_lexer.eat_blanks();
        ambient.b = m_lexer.accept_double();

        if (!m_inside_material_def)
            parse_error();

        throw ExceptionNotImplemented();
    }

    void parse_kd_statement()
    {
        Color3d diffuse;

        m_lexer.eat_blanks();
        diffuse.r = m_lexer.accept_double();

        m_lexer.eat_blanks();
        diffuse.g = m_lexer.accept_double();

        m_lexer.eat_blanks();
        diffuse.b = m_lexer.accept_double();

        if (!m_inside_material_def)
            parse_error();

        throw ExceptionNotImplemented();
    }

    void parse_ks_statement()
    {
        Color3d specular;

        m_lexer.eat_blanks();
        specular.r = m_lexer.accept_double();

        m_lexer.eat_blanks();
        specular.g = m_lexer.accept_double();

        m_lexer.eat_blanks();
        specular.b = m_lexer.accept_double();

        if (!m_inside_material_def)
            parse_error();

        throw ExceptionNotImplemented();
    }

    void parse_map_kd_statement()
    {
        m_lexer.eat_blanks();

        const string filename = m_lexer.accept_string();

        if (!m_inside_material_def)
            parse_error();

        throw ExceptionNotImplemented();
    }

    void parse_newmtl_statement()
    {
        m_lexer.eat_blanks();

        const string material_name = m_lexer.accept_string();

    //  m_material_id[material_name] = m_matbuilder->BeginMaterial(material_name);
        m_inside_material_def = true;
    }

#endif

};

OBJMeshFileReader::OBJMeshFileReader(const Options options)
  : impl(new Impl(options))
{
}

OBJMeshFileReader::~OBJMeshFileReader()
{
    delete impl;
}

void OBJMeshFileReader::read(
    const string&       filename,
    IMeshBuilder&       builder)
{
    OBJMeshBuilderAdaptor adaptor(builder);
    read(filename, adaptor);
}

void OBJMeshFileReader::read(
    const string&       filename,
    IOBJMeshBuilder&    builder)
{
    // Reset the parser to its initial state.
    impl->reset();

    // Store a pointer to the mesh builder.
    impl->m_builder = &builder;

    // Open the input file.
    if (!impl->m_lexer.open(filename))
        throw ExceptionIOError();

    // Parse the file.
    impl->parse_file();

    // Close the input file.
    impl->m_lexer.close();
}

}   // namespace foundation
