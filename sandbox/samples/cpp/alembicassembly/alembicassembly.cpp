
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#define APPLESEED_ENABLE_IMATH_INTEROP
// appleseed.renderer headers.
//#include "renderer/api/bsdf.h"
#include "renderer/api/object.h"
//#include "renderer/api/material.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/triangulator.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/string.h"

// appleseed.main headers.
#include "main/dllvisibility.h"

// AlembicAssembly headers.
#include "alembicassembly.h"

#include <Alembic/Abc/All.h>
#include <Alembic/AbcCoreFactory/All.h>
#include <Alembic/AbcGeom/All.h>

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

namespace asf = foundation;
namespace asr = renderer;

namespace
{


const char* AlembicAssembly::Model = "alembic_assembly";


AlembicAssembly::AlembicAssembly(
    const char*             name,
    const asr::ParamArray&  params)
  : asr::ProceduralAssembly(name, params)
{
    // retrieve assembly parameters
    //const auto params = get_parameters();

    // file path
    m_file_path = params.get<std::string>("file_path");

    if(m_file_path.empty())
    {
        std::cout << get_name() << ": Empty 'file_path' property" << std::endl;
        //return false;
    }

    // camera values
    //const auto& cam = project.get_uncached_active_camera();

    m_fps = params.get<float>("fps");
    m_frame = 2.0f;//cam->get_shutter_middle_time();
    m_shutter_open = -0.5f;//cam->get_shutter_open_time();
    m_shutter_close = 0.5f;//cam->get_shutter_close_time();

    std::cout << "file_path: " << m_file_path << std::endl;
    std::cout << "fps: " << m_fps << std::endl;
    std::cout << "frame: " << m_frame << std::endl;
    std::cout << "shutter_open: " << m_shutter_open << std::endl;
    std::cout << "shutter_close: " << m_shutter_close << std::endl;
}

bool AlembicAssembly::expand_contents(
    const asr::Project&     project,
    const asr::Assembly*    parent,
    asf::IAbortSwitch*      abort_switch)
{
    /*{
        // Create a new BSDF.
        asr::ParamArray params;
        params.insert("reflectance", "white");
        parent->bsdfs().insert(
            asr::LambertianBRDFFactory().create("white_material_brdf", params));
    }
    {
        // Create a new material.
        asr::ParamArray params;
        params.insert("surface_shader", "physical_shader");
        params.insert("bsdf", "white_material_brdf");
        parent->materials().insert(asr::GenericMaterialFactory().create("white_material", params));
    }*/

    std::cout << "Parent assembly name: " << parent->get_name() << std::endl;
    std::cout << "Assembly name: " << get_name() << std::endl;

    Alembic::AbcCoreFactory::IFactory factory;
    Alembic::AbcCoreFactory::IFactory::CoreType coreType;

    //auto archive = factory.getArchive("/home/narann/storage/alembic_octopus.abc", coreType);
    //auto archive = factory.getArchive("/home/narann/Desktop/abc_axis.abc", coreType);
    //auto archive = factory.getArchive("/home/narann/storage/room_geo.abc", coreType);
    auto archive = factory.getArchive(m_file_path, coreType);

    if (!archive.valid())
    {
        std::cout << "Invalid archive" << std::endl;
        return false;
    }

    std::cout << "Archive name: " << archive.getName() << std::endl;
    std::cout << "Number TimeSampling: " << archive.getNumTimeSamplings() << std::endl;
    std::cout << "Archive version: " << archive.getArchiveVersion() << std::endl;

    // retrieve the top IObject of the hierarchy
    auto top = archive.getTop();
    /*std::cout << "top valid?: " << top.valid() << std::endl;
    std::cout << "top name: " << top.getName() << std::endl;
    std::cout << "top full name: " << top.getFullName() << std::endl;
    std::cout << "top num children: " << top.getNumChildren() << std::endl;

    auto parent = top.getParent();
    std::cout << "parent of the top valid?: " << parent.valid() << std::endl; // of course not!

    auto header = top.getHeader();
    std::cout << "top header name: " << header.getName() << std::endl;
    std::cout << "top header full name: " << header.getFullName() << std::endl;

    auto metadata = top.getMetaData();
    std::cout << "metadata size: " << metadata.size() << std::endl;
    std::cout << "metadata serialize: " << metadata.serialize() << std::endl;

    // iterate over abc metadatas
    for(auto &it : metadata)
    {
        auto key   = it.first;
        auto value = it.second;
        std::cout << key << " = " << value << std::endl;
    }*/

    // xform stack
    //std::vector<asf::Matrix4d> mtx_stack;

    // child
    for(auto i = 0; i < top.getNumChildren(); i++)
    {
        auto child = top.getChild(i);
        std::cout << "child name: " << child.getFullName() << std::endl;
        foo(this, child);
    }

    //foo(top);

    return true;
}

void AlembicAssembly::foo(const asr::Assembly* assembly, Alembic::Abc::IObject o)
{
    const auto name = o.getFullName();

    // Create a new mesh object.
    auto object = asr::MeshObjectFactory().create(name.c_str(), asr::ParamArray());
    std::cout << "full name: " << o.getFullName() << std::endl;

    auto h = o.getHeader();

    if (Alembic::AbcGeom::IXform::matches(h))
    {
        auto abc_xform = Alembic::AbcGeom::IXform(o.getParent(), h.getName());
        std::cout << "xform !!! " << std::endl;

        const auto &xform_schema = abc_xform.getSchema();

        std::cout << "xform_schema.getNumOps() " << xform_schema.getNumOps() << std::endl;

        if (xform_schema.getNumOps() > 0)
        {
            const auto time_sampling = xform_schema.getTimeSampling();
            const auto num_samples = xform_schema.getNumSamples();

            std::cout << "num_samples " << num_samples << std::endl;

            std::set<Alembic::Abc::chrono_t> sample_set;

            if (num_samples < 2)
            {
                std::cout << "No sampling " << std::endl;
                sample_set.insert(0.0f);
            }
            else  // more than 1 sample
            {
                // absolute open/close times
                const auto t_open = (m_frame + m_shutter_open) / m_fps;
                const auto t_close = (m_frame + m_shutter_close) / m_fps;;

                const auto open_pair = time_sampling->getFloorIndex(t_open, num_samples);
                const auto close_pair = time_sampling->getCeilIndex(t_close, num_samples);

                std::cout << "open_pair.first " << open_pair.first << std::endl;
                std::cout << "close_pair.first " << close_pair.first << std::endl;

                // get _every_ sample times between open and close
                for (auto i = open_pair.first; i <= close_pair.first; i++)
                {
                    const auto t = time_sampling->getSampleTime(i);
                    sample_set.insert(t);
                }
            }

            //std::map<Alembic::Abc::chrono_t, asf::Matrix4d> mtx_time_map;

            asr::TransformSequence xform_seq;

            for (const auto t : sample_set)
            {
                std::cout << "t: " << t << " - frame: " << t * m_fps << std::endl;

                const auto sample_sel = Alembic::Abc::ISampleSelector(t);

                const auto xform_sample = xform_schema.getValue(sample_sel);
                const auto xform_mtx = xform_sample.getMatrix();
                const auto mtx = asf::Matrix4d(xform_mtx);

                //mtx_time_map[t] = mtx;
                auto xform = asf::Transformd::from_local_to_parent(mtx);

                xform_seq.set_transform(t, xform);
            }

            m_xform_seq_stack.push_back(xform_seq);

            /*auto sample = Alembic::Abc::ISampleSelector(0.0f);
            auto xform_sample = xform_schema.getValue(sample);
            auto xform_mtx = xform_sample.getMatrix();
            auto mtx = asf::Matrix4d(xform_mtx);
            m_mtx_stack.push_back(mtx);*/
        }

    }
    else if (Alembic::AbcGeom::IPolyMesh::matches(h))
    {
        std::cout << "poly!!!" << std::endl;

        auto polymesh = Alembic::AbcGeom::IPolyMesh(o);

        auto &schema = polymesh.getSchema();
        if (schema.getTopologyVariance() == Alembic::AbcGeom::kHeterogenousTopology)
        {
            std::cout << "isTopologyConstant false" << std::endl;
        }

        std::cout << "schema!!!" << schema.getTopologyVariance() << std::endl;

        auto sample_sel = Alembic::AbcGeom::ISampleSelector(0.0f);

        // Alembic::AbcGeom::IPolyMeshSchema::Sample
        auto sample = schema.getValue(sample_sel);

        //getVelocities()

        /*for(auto i = 0; i < pos_size; i += 3)
        {
            //std::cout << "pos!!!" << (*pos)[i] << std::endl;
            const asf::Vector3d center(pos[i][0], pos[i][1], pos[i][2]);
            const double radius = 0.1;
            output_sphere_instance(center, radius);
        }*/

        // flatten transforms
        //auto mtx = flatten_xform(m_mtx_stack);

        //mtx = asf::Matrix4d::make_scaling(asf::Vector3d(50.0f)) * mtx;


        //auto xform = asf::Transformd::from_local_to_parent(mtx);


        auto xform = asf::Transformd::from_local_to_parent(asf::Matrix4d::identity());
        //auto xform_seq = m_xform_seq_stack.pop_back();

        // Create a new mesh object.
        auto object = asf::auto_release_ptr<asr::MeshObject>(asr::MeshObjectFactory().create(name.c_str(), asr::ParamArray()));

        ///////////////////////////////////////////////////////////////
        // vertex positions
        ///////////////////////////////////////////////////////////////
        const auto pos = sample.getPositions()->get();
        const auto pos_count = sample.getPositions()->size();
        std::cout << "pos_count " << pos_count << std::endl;

        // reserve space to optimize allocation
        object->reserve_vertices(pos_count);

        for(auto i = 0; i < pos_count; i++)
        {
            auto v = asr::GVector3(pos[i][0], pos[i][1], pos[i][2]);
            object->push_vertex(v);
        }

        ///////////////////////////////////////////////////////////////
        // uvs
        ///////////////////////////////////////////////////////////////
        std::vector<size_t> uv_idxs;  // uv indices

        const auto uv_param = schema.getUVsParam();
        if (uv_param.valid())
        {
            std::cout << "uv!!! " << std::endl;
            auto uv_sample = uv_param.getIndexedValue();
            if (uv_sample.valid())
            {
                // retrieve uv indices
                const auto abc_uv_idxs = uv_sample.getIndices()->get();
                const auto uv_idxs_count = uv_sample.getIndices()->size();

                uv_idxs.reserve(uv_idxs_count);

                for(auto i = 0; i < uv_idxs_count; i++)
                {
                    uv_idxs.push_back(abc_uv_idxs[i]);
                }

                // uv vectors
                const auto uvs = uv_sample.getVals()->get();
                const auto uv_count = uv_sample.getVals()->size();
                std::cout << "uv_count " << uv_count << std::endl;

                // and reserve for optimization purpose
                object->reserve_tex_coords(uv_count);

                for (auto i = 0; i < uv_count; ++i)
                {
                    const asf::Vector2f uv(uvs[i]);
                    object->push_tex_coords(uv);
                }
            }
        }

        ///////////////////////////////////////////////////////////////
        // normals
        ///////////////////////////////////////////////////////////////
        std::vector<size_t> n_idxs;  // normal indices

        const auto normal_param = schema.getNormalsParam();
        if (normal_param.valid())
        {
            std::cout << "normal!!! " << std::endl;
            auto normal_sample = normal_param.getIndexedValue();
            //auto normal_sample = normal_param.getExpandedValue();
            if (normal_sample.valid())
            {
                // retrieve normal indices
                const auto abc_n_idxs = normal_sample.getIndices()->get();
                const auto n_idxs_count = normal_sample.getIndices()->size();

                n_idxs.reserve(n_idxs_count);

                for(auto i = 0; i < n_idxs_count; i++)
                {
                    n_idxs.push_back(abc_n_idxs[i]);
                }

                // normal vectors
                const auto normals = normal_sample.getVals()->get();
                const auto normal_count = normal_sample.getVals()->size();

                std::cout << "normal_count: " << normal_count << std::endl;
                std::cout << "n_idxs_count: " << normal_count << std::endl;

                // reserve space to optimize allocation
                object->reserve_vertex_normals(normal_count);

                for (auto i = 0; i < normal_count; ++i)
                {
                    //const asf::Vector3f n(normals[i]);       // todo: transform to world space using matrix stack
                    //auto n_xformed = xform.normal_to_local(n);
                    //object->push_vertex_normal(n_xformed);
                    //object->push_vertex_normal(n);
                    object->push_vertex_normal(normals[i]);
                }
            }
        }

        ///////////////////////////////////////////////////////////////
        // triangles
        ///////////////////////////////////////////////////////////////
        const auto face_sizes = sample.getFaceCounts()->get();  // [3,4,4,3,...]
        const auto face_count = sample.getFaceCounts()->size();
        const auto face_indices = sample.getFaceIndices()->get();
        const auto face_indices_count = sample.getFaceIndices()->size();

        std::cout << "face_count: " << face_count << std::endl;
        std::cout << "face_indices_count: " << face_indices_count << std::endl;

        // compute the number of triangles to reserve proper space
        size_t tri_count = 0;
        for(auto i = 0; i < face_count; i++)
        {
            tri_count += face_sizes[i] - 2;
        }

        // and reserve for optimization purpose
        object->reserve_triangles(tri_count);

        std::cout << "tri_count: " << tri_count << std::endl;

        // iterators
        auto f_i = 0;  // face indices
        auto n_i = 0;  // normal indices
        auto uv_i = 0;  // uv indices

        for(auto i = 0; i < face_count; i++)
        {
            // 3 or 4, maybe more
            const auto face_size = face_sizes[i];

            if (face_size < 3)
            {
                std::cout << "less than 3 point face detected!!! " << face_size << std::endl;
                f_i += face_size;
                n_i += face_size;
                uv_i += face_size;
            }
            else if (face_size == 3)
            {
                const auto pt0 = face_indices[f_i++];
                const auto pt1 = face_indices[f_i++];
                const auto pt2 = face_indices[f_i++];

                auto tri0 = asr::Triangle(pt0, pt2, pt1);

                if(!n_idxs.empty())
                {
                    const auto n0 = n_idxs[n_i++];
                    const auto n1 = n_idxs[n_i++];
                    const auto n2 = n_idxs[n_i++];
                    tri0.m_n0 = n0;
                    tri0.m_n1 = n2;
                    tri0.m_n2 = n1;
                }

                if(!uv_idxs.empty())
                {
                    const auto uv0 = uv_idxs[uv_i++];
                    const auto uv1 = uv_idxs[uv_i++];
                    const auto uv2 = uv_idxs[uv_i++];

                    tri0.m_a0 = uv0;
                    tri0.m_a1 = uv2;
                    tri0.m_a2 = uv1;
                }

                object->push_triangle(tri0);
            }
            else if (face_size == 4)
            {
                const auto pt0 = face_indices[f_i++];
                const auto pt1 = face_indices[f_i++];
                const auto pt2 = face_indices[f_i++];
                const auto pt3 = face_indices[f_i++];

                auto tri0 = asr::Triangle(pt0, pt2, pt1);
                auto tri1 = asr::Triangle(pt2, pt0, pt3);

                if(!n_idxs.empty())
                {
                    const auto n0 = n_idxs[n_i++];
                    const auto n1 = n_idxs[n_i++];
                    const auto n2 = n_idxs[n_i++];
                    const auto n3 = n_idxs[n_i++];

                    tri0.m_n0 = n0;
                    tri0.m_n1 = n2;
                    tri0.m_n2 = n1;

                    tri1.m_n0 = n2;
                    tri1.m_n1 = n0;
                    tri1.m_n2 = n3;
                }

                if(!uv_idxs.empty())
                {
                    const auto uv0 = uv_idxs[uv_i++];
                    const auto uv1 = uv_idxs[uv_i++];
                    const auto uv2 = uv_idxs[uv_i++];
                    const auto uv3 = uv_idxs[uv_i++];

                    tri0.m_a0 = uv0;
                    tri0.m_a1 = uv2;
                    tri0.m_a2 = uv1;

                    tri1.m_a0 = uv2;
                    tri1.m_a1 = uv0;
                    tri1.m_a2 = uv3;
                }

                object->push_triangle(tri0);
                object->push_triangle(tri1);
            }
            else  // arbitrary sized polygon
            {
                std::cout << "face_size: " << face_size << std::endl;

                // we create a polygon and will store it's various
                // positions.
                asf::Triangulator<float>::Polygon3 polygon;

                for(auto j = 0; j < face_size; j++)
                {
                    // get vertex id
                    const auto id = face_indices[f_i++];

                    // put the vertex position to the polygon
                    polygon.emplace_back(object->get_vertex(id));
                }

                asf::Triangulator<float> triangulator;
                asf::Triangulator<float>::IndexArray tris;

                const bool success = triangulator.triangulate(polygon, tris);

                if (success)
                {
                    std::cout << "tris.size(): " << tris.size() << std::endl;

                    // Insert all triangles of the triangulation into
                    // the mesh.
                    for (auto k = 0; k < tris.size(); k += 3)
                    {
                        auto tri = asr::Triangle(tris[k+0],
                                                 tris[k+1],
                                                 tris[k+2]);

                        if(!n_idxs.empty())
                        {
                            const auto n0 = n_idxs[k+0];
                            const auto n1 = n_idxs[k+1];
                            const auto n2 = n_idxs[k+2];

                            tri.m_n0 = n0;
                            tri.m_n1 = n2;
                            tri.m_n2 = n1;
                        }

                        if(!uv_idxs.empty())
                        {
                            const auto uv0 = uv_idxs[k+0];
                            const auto uv1 = uv_idxs[k+1];
                            const auto uv2 = uv_idxs[k+2];

                            tri.m_a0 = uv0;
                            tri.m_a1 = uv2;
                            tri.m_a2 = uv1;
                        }

                        // and finally push the triangle
                        object->push_triangle(tri);
                    }

                    // important step, increment iterators
                    if(!n_idxs.empty())
                        n_i += face_size;

                    if(!uv_idxs.empty())
                        uv_i += face_size;
                }
                else  // triangulator fail
                {
                    std::cout << "Invalid polygon for: " << name << std::endl;
                }
            }
        }
        std::cout << "f_i: " << f_i << std::endl;
        std::cout << "n_i: " << n_i << std::endl;
        std::cout << "uv_i: " << uv_i << std::endl;

        //const auto local_bbox = object->compute_local_bbox();
        //const auto world_bbox = object_instance->get_transform()->to_parent(local_bbox);
        // Insert the object into the assembly.
        assembly->objects().insert(asf::auto_release_ptr<asr::Object>(object));

        /*std::cout << "xform " << std::endl;
        for(auto i = 0; i < 16; i++)
        {
            std::cout << mtx[i] << " ";
        }
        std::cout << std::endl;*/

        auto front_material_mappings = asf::StringDictionary();
                    /*.insert("white_material", "white_material")*/

        // Create an instance of this object and insert it into the assembly.
        assembly->object_instances().insert(
            asr::ObjectInstanceFactory::create(
                (name+"_inst").c_str(), // instance name
                asr::ParamArray(),
                name.c_str(), // object name
                //asf::Transformd::from_local_to_parent(asf::Matrix4d::make_scaling(asf::Vector3d(1.0f))),
                xform,
                //xform_seq,
                front_material_mappings));

    }

    // iterate over children
    for(auto i = 0; i < o.getNumChildren(); i++)
    {
        auto child = o.getChild(i);
        foo(assembly, child);
    }

    // remove the bottom matrix as we move up
    m_mtx_stack.pop_back();
}

// roll into given matrix stack and return the flatten one
asf::Matrix4d flatten_xform(std::vector<asf::Matrix4d> mtx_stack)
{
    // the matrix we will return
    auto out_mtx = asf::Matrix4d::identity();

    // mutiply the each matrix of the stack from bottom to top
    for(auto mtx : mtx_stack)
    {
        out_mtx = mtx * out_mtx;
    }

    return out_mtx;
}


const char* AlembicAssemblyFactory::Model = "alembic_assembly";

}


//
// Plugin entry point.
//

extern "C"
{
    APPLESEED_DLL_EXPORT asr::IAssemblyFactory* appleseed_create_assembly_factory()
    {
        return new AlembicAssemblyFactory();
    }
}
