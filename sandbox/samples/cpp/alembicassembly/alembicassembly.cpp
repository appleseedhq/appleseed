
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

#include "renderer/global/globallogger.h"

// appleseed.renderer headers.
//#include "renderer/api/bsdf.h"
#include "renderer/api/object.h"
//#include "renderer/api/material.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"
//#include "renderer/api/log.h"

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

// roll into given xform sequence stack and return the flatten one
asr::TransformSequence flatten_xform_seq(const std::vector<asr::TransformSequence>& xform_seq_stack)
{
    auto out_xform_seq = asr::TransformSequence();

    if(xform_seq_stack.size() == 1)
    {
        // we don't need to flatten anything
        // TODO: Maybe we can return here?
        out_xform_seq = xform_seq_stack.back();
    }
    else
    {
        for(auto xform_seq : xform_seq_stack)
        {
            out_xform_seq = xform_seq * out_xform_seq;
        }
    }
    out_xform_seq.optimize();
    out_xform_seq.prepare();

    return out_xform_seq;
}

// return if given time samples are linearly time spaced
bool linearly_sampled(const Alembic::Abc::chrono_t start_time,
                      const Alembic::Abc::chrono_t end_time,
                      std::vector<Alembic::Abc::chrono_t> samples)
{
    std::cout << "samples.size() " << samples.size() << std::endl;
    assert(samples.size() > 1);

    // compute time supposed to be between each samples if samples are linearly
    // sampled.
    const auto increm = (end_time - start_time) / (samples.size()-1);
    std::cout << "increm " << increm << std::endl;

    // start sample
    auto t_accum = samples[0];

    const Alembic::Abc::chrono_t epsilon = 1.0 / 10000.0;

    // get each sample time and compare with a linearly sampled time
    for(const auto t : samples)
    {
        std::cout << t << " " << t-t_accum << " " << (std::fabs(t-t_accum) > epsilon) << std::endl;
        if (std::fabs(t-t_accum) > epsilon)
        {
            return false;
        }

        t_accum += increm;
    }
    return true;
}

// return sample times (with border samples) in given shutter for given schema
// returned samples  ------|-----------|---------|--------->
//                   ---------|--------------|------------->
// shutter times             open          close
std::vector<Alembic::Abc::chrono_t> schema_to_sample_times(const float shutter_open_time,
                                                           const float shutter_close_time,
                                                           //const Alembic::AbcGeom::IPolyMeshSchema schema,
                                                           const size_t num_samples,
                                                           const Alembic::AbcCoreAbstract::TimeSamplingPtr time_sampling)
{
    std::cout << "schema num_samples " << num_samples << std::endl;

    std::vector<Alembic::Abc::chrono_t> sample_times;

    if (num_samples < 2)
    {
        std::cout << "No sampling " << std::endl;
        sample_times.push_back(0.0f);
    }
    else  // more than 1 sample
    {
        // get floor and ceil sample idss to don't miss any
        const auto open_pair = time_sampling->getFloorIndex(shutter_open_time, num_samples);
        const auto close_pair = time_sampling->getCeilIndex(shutter_close_time, num_samples);

        std::cout << "open_pair.first " << open_pair.first << std::endl;
        std::cout << "close_pair.first " << close_pair.first << std::endl;

        sample_times.reserve(close_pair.first-open_pair.first+1);

        // get _every_ sample times between open and close
        for (auto i = open_pair.first; i <= close_pair.first; i++)
        {
            const auto t = time_sampling->getSampleTime(i);
            sample_times.push_back(t);
        }
    }
    return sample_times;
}



const char* AlembicAssembly::Model = "alembic_assembly";


AlembicAssembly::AlembicAssembly(
    const char*             name,
    const asr::ParamArray&  params)
  : asr::ProceduralAssembly(name, params)
{}

void AlembicAssembly::retrieve_params(const asr::Project& project)
{
    // retrieve assembly parameters
    const auto params = get_parameters();

    // file path
    m_file_path = params.get<std::string>("file_path");

    if(m_file_path.empty())
    {
        std::cout << get_name() << ": Empty 'file_path' property" << std::endl;
        //return false;
    }

    // shutters
    //m_shutter_open_time = 1.2/24.0;//params.get_optional<float>("shutter_open_time", 0.0f);
    //m_shutter_close_time = 2.2/24.0;//params.get_optional<float>("shutter_close_time", 0.0f);
    m_shutter_open_time = 1.0/24.0;
    m_shutter_close_time = 3.0/24.0;

    std::cout << "file_path: " << m_file_path << std::endl;
    std::cout << "shutter_open_time: " << m_shutter_open_time << std::endl;
    std::cout << "shutter_close_time: " << m_shutter_close_time << std::endl;

    // time offset
    m_time_offset = params.get_optional<float>("time_offset", 0.0f);
}

bool AlembicAssembly::do_expand_contents(
    const asr::Project&     project,
    const asr::Assembly*    parent,
    asf::IAbortSwitch*      abort_switch)
{
    retrieve_params(project);

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
    Alembic::AbcCoreFactory::IFactory::CoreType core_type;

    //auto archive = factory.getArchive("/home/narann/storage/alembic_octopus.abc", core_type);
    //auto archive = factory.getArchive("/home/narann/Desktop/abc_axis.abc", core_type);
    //auto archive = factory.getArchive("/home/narann/storage/room_geo.abc", core_type);
    auto archive = factory.getArchive(m_file_path, core_type);

    if (!archive.valid())
    {
        std::cout << "Invalid archive" << std::endl;
        return false;
    }


    std::cout << "Current time is: " << m_time_offset+(m_shutter_open_time+m_shutter_close_time)*0.5 << std::endl;

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

    m_xform_seq_stack.push_back(asr::TransformSequence());

    // top children
    for(auto i = 0; i < top.getNumChildren(); i++)
    {
        auto child = top.getChild(i);
        std::cout << std::endl << "child name: " << child.getFullName() << std::endl;
        foo(this, child);
    }

    //foo(top);

    return true;
}

void AlembicAssembly::foo(const asr::Assembly* assembly, Alembic::Abc::IObject o)
{
    const auto name = o.getFullName();

    std::cout << "full name: " << o.getFullName() << std::endl;

    auto h = o.getHeader();

    // we track if a transform sequence has been generated or not to remove it
    // after we reached every children.
    bool gen_xform = false;

    if (Alembic::AbcGeom::IXform::matches(h))
    {
        auto abc_xform = Alembic::AbcGeom::IXform(o.getParent(), h.getName());
        std::cout << "xform !!! " << std::endl;

        const auto &xform_schema = abc_xform.getSchema();

        std::cout << "xform_schema.getNumOps() " << xform_schema.getNumOps() << std::endl;

        if (xform_schema.getNumOps() > 0)
        {
            const auto num_samples = xform_schema.getNumSamples();
            const auto sample_times = schema_to_sample_times(m_shutter_open_time+m_time_offset,
                                                             m_shutter_close_time+m_time_offset,
                                                             num_samples,
                                                             xform_schema.getTimeSampling());

            //std::map<Alembic::Abc::chrono_t, asf::Matrix4d> mtx_time_map;

            asr::TransformSequence xform_seq;

            // put every xform samples founds into the TransformSequence
            for (const auto t : sample_times)
            {
                std::cout << "t: " << t << " - frame: " << t * 24.0f << std::endl;

                const auto sample_sel = Alembic::Abc::ISampleSelector(t);

                const auto xform_sample = xform_schema.getValue(sample_sel);
                const auto xform_mtx = xform_sample.getMatrix();
                const auto mtx = asf::Matrix4d(xform_mtx);
                const auto xform = asf::Transformd::from_local_to_parent(mtx);

                // as all samples are time offsetted, we offset them back
                xform_seq.set_transform(t-m_time_offset, xform);
            }

            // and put our filled xform sequence to the main xform stack
            m_xform_seq_stack.push_back(xform_seq);

            gen_xform = true;
        }

    }
    else if (Alembic::AbcGeom::IPolyMesh::matches(h))
    {
        std::cout << "poly!!!" << std::endl;

        const auto polymesh = Alembic::AbcGeom::IPolyMesh(o);

        const auto &schema = polymesh.getSchema();

        switch(schema.getTopologyVariance())
        {
            case Alembic::AbcGeom::kConstantTopology:
                // non deformed
                std::cout << "Constant topology" << std::endl;
                break;
            case Alembic::AbcGeom::kHomogeneousTopology:
                // deformed but no topology change
                std::cout << "Homogeneous topology variance" << std::endl;
                break;
            case Alembic::AbcGeom::kHeterogeneousTopology:
                // fluid geos (realflow bins, etc.)
                std::cout << "Heterogeneous topology variance" << std::endl;
                break;
            default:
                std::cout << "Unknown topology variance type" << std::endl;
                break;
        }

        const auto sample_times = schema_to_sample_times(m_shutter_open_time+m_time_offset,
                                                         m_shutter_close_time+m_time_offset,
                                                         schema.getNumSamples(),
                                                         schema.getTimeSampling());

        // compute appleseed number of motion segment
        const auto motion_segment_count = sample_times.size() - 1;

        if(motion_segment_count)
        {
            if(!linearly_sampled(m_shutter_open_time, m_shutter_close_time, sample_times))
            {
                std::cout << "not linearly sampled!" << std::endl;
                RENDERER_LOG_WARNING("Motion samples are not matching shutters.");  // TODO: logs doesn't work.
            }
        }

        std::cout << "sample_times.size() " << sample_times.size() << std::endl;
        for(const auto t : sample_times)
        {
            std::cout << "  t: " << t << " - frame: " << t * 24.0f << std::endl;
        }

        //getVelocities()

        // Create a new mesh object
        auto object = asf::auto_release_ptr<asr::MeshObject>(
            asr::MeshObjectFactory().create(
                name.c_str(),
                asr::ParamArray()));

        object->set_motion_segment_count(motion_segment_count);

        ///////////////////////////////////////////////////////////////
        // vertex positions
        ///////////////////////////////////////////////////////////////
        auto sample_id = 0;
        for(const auto sample_time : sample_times)
        {
            const auto sample_sel = Alembic::AbcGeom::ISampleSelector(sample_time);

            const auto sample = schema.getValue(sample_sel);

            const auto pos = sample.getPositions()->get();
            const auto pos_count = sample.getPositions()->size();
            std::cout << "pos_count " << pos_count << std::endl;

            // reserve space to optimize allocation
            object->reserve_vertices(pos_count);

            for(auto i = 0; i < pos_count; i++)
            {
                const auto v = asr::GVector3(pos[i][0], pos[i][1], pos[i][2]);

                if(sample_id == 0)  // first sample
                {
                    object->push_vertex(v);
                }
                else  // all other samples
                {
                    object->set_vertex_pose(i, sample_id-1, v);
                }
            }

            sample_id++;
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
        const auto sample_sel = Alembic::AbcGeom::ISampleSelector(0.0);
        const auto sample = schema.getValue(sample_sel);
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

        auto front_material_mappings = asf::StringDictionary();
                    /*.insert("white_material", "white_material")*/


        // Create an instance of the assembly.
        asf::auto_release_ptr<asr::Assembly> xform_assembly(
            asr::AssemblyFactory().create(  // TODO: Do I really have to instanciate AssemblyFactory?
                (name+"_assembly").c_str(),  // assembly instance
                asr::ParamArray()));

        // Insert the object into the assembly.
        xform_assembly->objects().insert(asf::auto_release_ptr<asr::Object>(object));

        // Create an instance of this object and insert it into the assembly.
        xform_assembly->object_instances().insert(
            asr::ObjectInstanceFactory::create(
                (name+"_inst").c_str(), // instance name
                asr::ParamArray(),
                name.c_str(), // object name
                asf::Transformd::identity(),
                front_material_mappings));

        // Create an instance of the assembly.
        asf::auto_release_ptr<asr::AssemblyInstance> xform_assembly_inst(
            asr::AssemblyInstanceFactory::create(
                (name+"_assembly_inst").c_str(),  // assembly instance
                asr::ParamArray(),
                (name+"_assembly").c_str()));

        std::cout << "xform_seq_stack " << m_xform_seq_stack.size() << std::endl;

        if(m_xform_seq_stack.size())
        {
            auto xform_seq = flatten_xform_seq(m_xform_seq_stack);

            xform_assembly_inst->transform_sequence() = xform_seq;
        }

        assembly->assemblies().insert(xform_assembly);
        assembly->assembly_instances().insert(xform_assembly_inst);
    }

    // iterate over children
    for(auto i = 0; i < o.getNumChildren(); i++)
    {
        auto child = o.getChild(i);
        foo(assembly, child);
    }

    std::cout << "gen_xform " << gen_xform << std::endl;
    std::cout << "end xform_seq_stack " << m_xform_seq_stack.size() << std::endl;
    if(gen_xform)
    {
        // remove the bottom transform as we move up
        m_xform_seq_stack.pop_back();
    }
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
