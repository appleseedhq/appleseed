
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Dorian Fevrier, The appleseedhq Organization
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
#include "renderer/api/object.h"
#include "renderer/api/log.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/triangulator.h"
#include "foundation/math/transform.h"
#include "foundation/math/scalar.h"
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
#include <algorithm>

namespace asf = foundation;
namespace asr = renderer;

namespace
{

// roll into given matrix stack and return the flatten one
asf::Matrix4d flatten_xform(const std::vector<asf::Matrix4d>& mtx_stack)
{
    // the matrix we will return
    auto out_mtx = asf::Matrix4d::identity();

    // mutiply the each matrix of the stack from bottom to top
    for (auto mtx : mtx_stack)
    {
        out_mtx = mtx * out_mtx;
    }

    return out_mtx;
}

// roll into given xform sequence stack and return the flatten one
asr::TransformSequence flatten_xform_seq(const std::vector<asr::TransformSequence>& xform_seq_stack)
{
    asr::TransformSequence out_xform_seq;

    if (xform_seq_stack.size() == 1)
    {
        // we don't need to flatten anything
        return xform_seq_stack.back();
    }
    else
    {
        for (auto xform_seq : xform_seq_stack)
        {
            out_xform_seq = xform_seq * out_xform_seq;
        }
    }
    out_xform_seq.optimize();
    out_xform_seq.prepare();

    return out_xform_seq;
}

// return if given time samples are linearly time spaced
bool are_linearly_sampled(const Alembic::Abc::chrono_t start_time,
                          const Alembic::Abc::chrono_t end_time,
                          const std::vector<Alembic::Abc::chrono_t>& samples)
{
    assert(samples.size() > 1);

    // compute time supposed to be between each samples if samples are linearly
    // sampled.
    const auto increm = (end_time - start_time) / (samples.size()-1);

    // start sample
    auto t_accum = samples[0];

    // get each sample time and compare with a linearly sampled time
    for (const auto t : samples)
    {
        if (asf::feq(t, t_accum))
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
std::vector<Alembic::Abc::chrono_t> schema_to_sample_timesOO(const float shutter_open_time,
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
        const std::pair<Alembic::Abc::index_t,
                        Alembic::Abc::chrono_t> open_pair = time_sampling->getFloorIndex(shutter_open_time, num_samples);
        const std::pair<Alembic::Abc::index_t,
                        Alembic::Abc::chrono_t> close_pair = time_sampling->getCeilIndex(shutter_close_time, num_samples);

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

// return sample indices contained in given shutter
std::vector<Alembic::Abc::index_t> schema_to_sample_times(
    const float shutter_open_time,
    const float shutter_close_time,
    const size_t num_samples,
    const Alembic::AbcCoreAbstract::TimeSamplingPtr time_sampling)
{
    assert(shutter_open_time <= shutter_close_time);

    // the index array we will return
    std::vector<Alembic::Abc::index_t> sample_indices;

    if (num_samples < 2)
    {
        // no sample, put the main sample (sample 0)
        sample_indices.push_back(0);
    }
    else  // more than 1 sample
    {
        // get floor and ceil sample indices to don't miss any
        const std::pair<Alembic::Abc::index_t,
                        Alembic::Abc::chrono_t> open_pair =
            time_sampling->getFloorIndex(shutter_open_time, num_samples);
        const std::pair<Alembic::Abc::index_t,
                        Alembic::Abc::chrono_t> close_pair =
            time_sampling->getCeilIndex(shutter_close_time, num_samples);

        sample_indices.reserve(close_pair.first-open_pair.first+1);

        // get _every_ sample indices between open and close
        for (auto i = open_pair.first; i <= close_pair.first; i++)
        {
            sample_indices.push_back(i);
        }
    }
    return sample_indices;
}



//
// Alembic Assembly
//

class AlembicAssembly
  : public asr::ProceduralAssembly
{
  public:
    // Constructor.
    AlembicAssembly(
        const char*             name,
        const asr::ParamArray&  params)
      : asr::ProceduralAssembly(name, params)
    {}

    // Return a string identifying the model of this entity.
    const char* get_model() const override
    {
        return Model;
    };

    // Delete this instance.
    void release() override
    {
        delete this;
    }

    // Expand the contents of the assembly.
    bool do_expand_contents(
        const asr::Project&     project,
        const asr::Assembly*    parent,
        asf::IAbortSwitch*      abort_switch = 0) override
    {
        ///////////////////////////////////////////////////////////////////////
        // retrieve assembly parameters
        ///////////////////////////////////////////////////////////////////////
        const auto params = get_parameters();

        // file path
        m_file_path = params.get<std::string>("file_path");

        if (m_file_path.empty())
        {
            RENDERER_LOG_WARNING("Empty 'file_path' property: \"%s\"", get_name());
            return false;
        }

        // shutters
        m_shutter_open_time = params.get_optional<float>("shutter_open_time", 0.0f);
        m_shutter_close_time = params.get_optional<float>("shutter_close_time", 0.0f);

        // time offset
        m_time_offset = params.get_optional<float>("time_offset", 0.0f);


        Alembic::AbcCoreFactory::IFactory factory;
        Alembic::AbcCoreFactory::IFactory::CoreType core_type;

        const Alembic::Abc::IArchive& archive = factory.getArchive(m_file_path, core_type);

        if (!archive.valid())
        {
            RENDERER_LOG_WARNING("Invalid archive: \"%s\"", m_file_path.c_str());
            return false;
        }

        m_xform_seq_stack.push_back(asr::TransformSequence());

        // retrieve archive root object
        const Alembic::Abc::IObject& root = archive.getTop();

        // root children
        for (auto i = 0; i < root.getNumChildren(); i++)
        {
            const Alembic::Abc::IObject& child = root.getChild(i);
            std::cout << std::endl << "child name: " << child.getFullName() << std::endl;
            foo(this, child);
        }

        //foo(top);

        return true;
    }

  private:

    void foo(const asr::Assembly* assembly, const Alembic::Abc::IObject& obj)
    {
        const auto name = obj.getFullName();

        std::cout << "full name: " << obj.getFullName() << std::endl;

        auto h = obj.getHeader();

        // we track if a transform sequence has been generated or not to remove it
        // after we reached every children.
        bool gen_xform = false;

        if (Alembic::AbcGeom::IXform::matches(h))
        {
            const auto& abc_xform = Alembic::AbcGeom::IXform(obj);

            const auto& xform_schema = abc_xform.getSchema();

            if (xform_schema.getNumOps() > 0)
            {
                const auto sample_times = schema_to_sample_timesOO(
                    m_shutter_open_time+m_time_offset,
                    m_shutter_close_time+m_time_offset,
                    xform_schema.getNumSamples(),
                    xform_schema.getTimeSampling());

                asr::TransformSequence xform_seq;

                // put every xform samples founds into the TransformSequence
                for (const auto t : sample_times)
                {
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
            const auto polymesh = Alembic::AbcGeom::IPolyMesh(obj);

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

            const auto sample_times = schema_to_sample_timesOO(m_shutter_open_time+m_time_offset,
                                                               m_shutter_close_time+m_time_offset,
                                                               schema.getNumSamples(),
                                                               schema.getTimeSampling());

            // compute appleseed number of motion segment
            const auto motion_segment_count = sample_times.size() - 1;

            if (motion_segment_count)
            {
                if (!are_linearly_sampled(m_shutter_open_time, m_shutter_close_time, sample_times))
                {
                    std::cout << "not linearly sampled!" << std::endl;
                    RENDERER_LOG_WARNING("Motion samples are not matching shutters.");  // TODO: logs doesn't work.
                }
            }

            std::cout << "sample_times.size() " << sample_times.size() << std::endl;
            for (const auto t : sample_times)
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
            for (const auto sample_time : sample_times)
            {
                const auto sample_sel = Alembic::AbcGeom::ISampleSelector(sample_time);

                const auto sample = schema.getValue(sample_sel);

                const auto pos = sample.getPositions()->get();
                const auto pos_count = sample.getPositions()->size();
                std::cout << "pos_count " << pos_count << std::endl;

                // reserve space to optimize allocation
                object->reserve_vertices(pos_count);

                for (auto i = 0; i < pos_count; i++)
                {
                    const asr::GVector3 v(pos[i][0], pos[i][1], pos[i][2]);

                    if (sample_id == 0)  // first sample
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

                    for (auto i = 0; i < uv_idxs_count; i++)
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
                auto sample_id = 0;
                for (const auto sample_time : sample_times)
                {
                    const auto sample_sel = Alembic::AbcGeom::ISampleSelector(sample_time);

                    const auto sample = schema.getValue(sample_sel);

                    switch(normal_param.getScope())
                    {
                        case Alembic::AbcGeom::kConstantScope:
                            std::cout << "normal kConstantScope" << std::endl;
                            break;
                        case Alembic::AbcGeom::kUniformScope:
                            std::cout << "normal kUniformScope" << std::endl;
                            break;
                        case Alembic::AbcGeom::kVaryingScope:
                            std::cout << "normal kVaryingScope" << std::endl;
                            break;
                        case Alembic::AbcGeom::kVertexScope:
                            // normal indices match vertex one
                            std::cout << "normal kVertexScope" << std::endl;
                            break;
                        case Alembic::AbcGeom::kFacevaryingScope:
                            // normals have their own indices
                            std::cout << "normal kFacevaryingScope" << std::endl;
                            break;
                        case Alembic::AbcGeom::kUnknownScope:
                            std::cout << "normal kUnknownScope" << std::endl;
                            break;
                        default:
                            std::cout << "invalid scope" << std::endl;
                            break;
                    }

                    auto normal_sample = normal_param.getIndexedValue();
                    //auto sample = normal_param.getExpandedValue();  // todo use this depending on the scope
                    if (normal_sample.valid())
                    {
                        // retrieve normal indices
                        const auto abc_n_idxs = normal_sample.getIndices()->get();
                        const auto n_idxs_count = normal_sample.getIndices()->size();

                        n_idxs.reserve(n_idxs_count);  // should be same size as vertices

                        for (auto i = 0; i < n_idxs_count; i++)
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

                        if (sample_id == 0)  // first sample
                        {
                            for (auto i = 0; i < normal_count; ++i)
                            {
                                object->push_vertex_normal(normals[i]);
                            }
                        }
                        else  // other samples are put in motion poses
                        {
                            for (auto i = 0; i < normal_count; ++i)
                            {
                                object->set_vertex_normal_pose(i, sample_id-1, normals[i]);
                            }
                        }
                    }
                    sample_id++;
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
            for (auto i = 0; i < face_count; i++)
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

            for (auto i = 0; i < face_count; i++)
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

                    asr::Triangle tri0(pt0, pt2, pt1);

                    if (!n_idxs.empty())
                    {
                        const auto n0 = n_idxs[n_i++];
                        const auto n1 = n_idxs[n_i++];
                        const auto n2 = n_idxs[n_i++];
                        tri0.m_n0 = n0;
                        tri0.m_n1 = n2;
                        tri0.m_n2 = n1;
                    }

                    if (!uv_idxs.empty())
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

                    asr::Triangle tri0(pt0, pt2, pt1);
                    asr::Triangle tri1(pt2, pt0, pt3);

                    if (!n_idxs.empty())
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

                    if (!uv_idxs.empty())
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

                    for (auto j = 0; j < face_size; j++)
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

                            if (!n_idxs.empty())
                            {
                                const auto n0 = n_idxs[k+0];
                                const auto n1 = n_idxs[k+1];
                                const auto n2 = n_idxs[k+2];

                                tri.m_n0 = n0;
                                tri.m_n1 = n2;
                                tri.m_n2 = n1;
                            }

                            if (!uv_idxs.empty())
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
                        if (!n_idxs.empty())
                            n_i += face_size;

                        if (!uv_idxs.empty())
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

            if (m_xform_seq_stack.size())
            {
                auto xform_seq = flatten_xform_seq(m_xform_seq_stack);

                xform_assembly_inst->transform_sequence() = xform_seq;
            }

            assembly->assemblies().insert(xform_assembly);
            assembly->assembly_instances().insert(xform_assembly_inst);
        }

        // iterate over children
        for (auto i = 0; i < obj.getNumChildren(); i++)
        {
            const auto& child = obj.getChild(i);
            foo(assembly, child);
        }

        std::cout << "gen_xform " << gen_xform << std::endl;
        std::cout << "end xform_seq_stack " << m_xform_seq_stack.size() << std::endl;
        if (gen_xform)
        {
            // remove the bottom transform as we move up
            m_xform_seq_stack.pop_back();
        }
    }

    const char* Model = "alembic_assembly";

    std::string m_file_path;
    float m_shutter_open_time;
    float m_shutter_close_time;
    float m_time_offset;
    std::vector<asr::TransformSequence> m_xform_seq_stack;
};


// returned alpha is a factor between floor and ceil index (0.0 is floor, 1.0
// if ceil).
float get_weight_and_index(
    const float time,
    const Alembic::AbcCoreAbstract::TimeSamplingPtr time_sample,
    size_t num_samples,
    Alembic::AbcCoreAbstract::index_t& floor_index,
    Alembic::AbcCoreAbstract::index_t& ceil_index)
{
    if (num_samples == 0)
        num_samples = 1;

    // find floor sample index
    std::pair<Alembic::AbcCoreAbstract::index_t,
              Alembic::AbcCoreAbstract::chrono_t> floor_pair =
        time_sample->getFloorIndex(time, num_samples);

    floor_index = floor_pair.first;
    ceil_index = floor_index;

    const Alembic::AbcCoreAbstract::chrono_t floor_time = floor_pair.second;

    if (asf::feq(time, floor_time))
        return 0.0f;

    // find ceil sample index
    std::pair<Alembic::AbcCoreAbstract::index_t,
              Alembic::AbcCoreAbstract::chrono_t> ceil_pair =
        time_sample->getCeilIndex(time, num_samples);

    if (floor_index == ceil_pair.first)
        return 0.0f;

    ceil_index = ceil_pair.first;

    const Alembic::AbcCoreAbstract::chrono_t ceil_time = ceil_pair.second;

    float alpha = (time - floor_time) / (ceil_time - floor_time);

    // we so closely match the ceiling so we'll just use it
    if (asf::feq(alpha, 1.0f))
    {
        floor_index = ceil_index;
        return 0.0f;
    }

    return alpha;
}


// move from given obj to the root storing every xform then return them as a
// sequence.
std::vector<asr::TransformSequence> xform_seq_for_obj(
    const Alembic::AbcGeom::IObject obj,
    const float shutter_open_time,
    const float shutter_close_time,
    const float time_offset = 0.0f)
{
    std::vector<asr::TransformSequence> xform_seq_stack;

    Alembic::AbcGeom::IObject cur_obj = obj;

    while(cur_obj.valid())
    {
        const Alembic::AbcGeom::ObjectHeader h = cur_obj.getHeader();

        if (Alembic::AbcGeom::IXform::matches(h))
        {
            auto abc_xform = Alembic::AbcGeom::IXform(cur_obj);

            const Alembic::AbcGeom::IXformSchema& xform_schema = abc_xform.getSchema();

            if (xform_schema.getNumOps() < 1)
                continue;

            const auto xform_time_sampling = xform_schema.getTimeSampling();
            const auto xform_num_samples = xform_schema.getNumSamples();

            const auto sample_times = schema_to_sample_times(
                shutter_open_time+time_offset,
                shutter_close_time+time_offset,
                xform_num_samples,
                xform_time_sampling);


            // we will put every xform samples founds into this xform sequence
            asr::TransformSequence xform_seq;

            for (const auto i : sample_times)
            {
                // sample time
                const auto t = xform_time_sampling->getSampleTime(i);

                // sample xform
                const auto xform_sample = xform_schema.getValue(i);
                const auto xform_mtx = xform_sample.getMatrix();
                const auto mtx = asf::Matrix4d(xform_mtx);
                const auto xform = asf::Transformd::from_local_to_parent(mtx);

                // as all samples are time offsetted, we offset them back
                xform_seq.set_transform(t-time_offset, xform);
            }

            // and put our filled xform sequence to the main xform stack
            xform_seq_stack.push_back(xform_seq);
        }

        // move to parent
        cur_obj = cur_obj.getParent();
    }

    // as we moved from bottom (object) to top (root), we have to revert the
    // xform stack before return it.
    std::reverse(std::begin(xform_seq_stack), std::end(xform_seq_stack));

    return xform_seq_stack;
}

bool path_to_obj(const Alembic::Abc::IArchive& archive,
                 const std::string& path)
{
    // retrieve root of the hierarchy
    Alembic::Abc::IObject cur_obj = archive.getTop();

    std::vector<std::string> obj_names;
    asf::split(path, "/", obj_names);


    if (obj_names.size() < 2)
    {
        RENDERER_LOG_ERROR("Can't find object '%s' in archivef '%s'",
                path.c_str(), archive.getName().c_str());
        return false;
    }

    for (size_t i = 1; i < obj_names.size(); i++)
    {
        const std::string& obj_name = obj_names[i];

        bool found = false;

        for (size_t j = 0; j < cur_obj.getNumChildren(); j++)
        {
            auto child = cur_obj.getChild(j);

            if (child.getName() == obj_name)
            {
                // we have a winner!
                cur_obj = child;
                found = true;
                break;
            }
        }
        if (!found)
        {
            RENDERER_LOG_ERROR("Can't find '%s' child of '%s'",
                    obj_name.c_str(), cur_obj.getFullName().c_str());
            return false;
        }
    }
}


//
// Factory for the new assembly model.
//

class AlembicAssemblyFactory
  : public asr::IAssemblyFactory
{
  public:
    // Delete this instance.
    void release() override
    {
        delete this;
    }

    // Return a string identifying this assembly model.
    const char* get_model() const override
    {
        return Model;
    }

    // Create a new assembly.
    asf::auto_release_ptr<asr::Assembly> create(
        const char*             name,
        const asr::ParamArray&  params) const override
    {
        return asf::auto_release_ptr<asr::Assembly>(
            new AlembicAssembly(name, params));
    }

  private:

    const char* Model = "alembic_assembly";

};


class AlembicCamera
  : public asr::Camera
{
  public:
    AlembicCamera(
        const char*             name,
        const asr::ParamArray&  params)
    : asr::Camera(name, params)
    {
        m_file_path = params.get<std::string>("file_path");

        m_obj_path = params.get<std::string>("obj_path");

        m_time_offset = params.get_optional<float>("time_offset", 0.0f);

        m_camera = asr::PinholeCameraFactory().create("cam", asr::ParamArray());
    }

    void release() override
    {
        delete this;
    }

    const char* get_model() const override
    {
        return Model;
    }

    bool on_frame_begin(
        const asr::Project&                  project,
        const asr::BaseGroup*                parent,
        asr::OnFrameBeginRecorder&           recorder,
        asf::IAbortSwitch*       abort_switch = nullptr) override
    {
        m_camera->on_frame_begin(project, parent, recorder, abort_switch);
    }

    bool on_render_begin(
        const asr::Project&      project,
        asf::IAbortSwitch*       abort_switch) override
    {
        if (m_file_path.empty())
        {
            RENDERER_LOG_ERROR("Empty 'file_path' property: \"%s\"", get_name());
            return false;
        }

        if (m_obj_path.empty())
        {
            RENDERER_LOG_ERROR("Empty 'obj_path' property: \"%s\"", get_name());
            return false;
        }

        // retrieve alembic camera parameters
        const asr::ParamArray& params = get_parameters();

        // shutters
        float shutter_open_time = params.get_optional<float>("shutter_open_time", 0.0f);
        float shutter_close_time = params.get_optional<float>("shutter_close_time", 1.0f);

        if (shutter_close_time < shutter_open_time)
        {
            shutter_close_time = shutter_open_time;

            RENDERER_LOG_WARNING(
                "Shutter close (%f) is smaller than shutter open (%f) "
                "for camera \"%s\", only shutter open will be use.",
                    shutter_close_time, shutter_open_time, get_path().c_str());
        }

        // alembic archive
        Alembic::AbcCoreFactory::IFactory factory;
        Alembic::AbcCoreFactory::IFactory::CoreType core_type;

        Alembic::Abc::IArchive archive = factory.getArchive(m_file_path, core_type);

        if (!archive.valid())
        {
            RENDERER_LOG_ERROR("Invalid alembic archive: \"%s\"", m_file_path.c_str());
            return false;
        }

        // retrieve root of the hierarchy
        Alembic::Abc::IObject cur_obj = archive.getTop();

        std::vector<std::string> obj_names;
        asf::split(m_obj_path, "/", obj_names);

        for (size_t i = 1; i < obj_names.size(); i++)
        {
            const std::string& obj_name = obj_names[i];

            bool found = false;

            for (size_t j = 0; j < cur_obj.getNumChildren(); j++)
            {
                auto child = cur_obj.getChild(j);

                if (child.getName() == obj_name)
                {
                    // we have a winner!
                    cur_obj = child;
                    found = true;
                    break;
                }
            }
            if (!found)
            {
                RENDERER_LOG_ERROR("Can't find '%s' child of '%s'",
                        obj_name.c_str(), cur_obj.getFullName().c_str());
                return false;
            }
        }

        // we have our object, get its schema
        const Alembic::AbcGeom::ObjectHeader cam_header = cur_obj.getHeader();

        if (!Alembic::AbcGeom::ICamera::matches(cam_header))
        {
            RENDERER_LOG_ERROR("Not a camera; '%s'", cur_obj.getFullName().c_str());
            return false;
        }

        ///////////////////////////////////////////////////////////////////////////
        // Static values
        ///////////////////////////////////////////////////////////////////////////
        const auto abc_cam = Alembic::AbcGeom::ICamera(cur_obj);

        const Alembic::AbcGeom::ICameraSchema& cam_schema = abc_cam.getSchema();

        // we need middle shutter to find the best value for non-motion blurred
        // variables.
        const float mid_shutter_time = (0.5f * (shutter_open_time+shutter_close_time))+m_time_offset;

        // get floor and ceil sample indices from mid shutter
        Alembic::AbcCoreAbstract::index_t floor_index;
        Alembic::AbcCoreAbstract::index_t ceil_index;

        const float alpha = get_weight_and_index(
            mid_shutter_time,
            cam_schema.getTimeSampling(),
            cam_schema.getNumSamples(),
            floor_index,
            ceil_index);

        // now get floor and ceil samples
        Alembic::AbcGeom::CameraSample floor_sample;
        Alembic::AbcGeom::CameraSample ceil_sample;

        cam_schema.get(floor_sample, floor_index);
        cam_schema.get(ceil_sample, ceil_index);

        // and interpolate sample camera values
        const float mid_focal_length = asf::lerp<float>(
            floor_sample.getFocalLength(),
            ceil_sample.getFocalLength(),
            alpha);

        const float mid_f_stop = asf::lerp<float>(
            floor_sample.getFStop(),
            ceil_sample.getFStop(),
            alpha);

        const float mid_focal_distance = asf::lerp<float>(
            floor_sample.getFocusDistance(),
            ceil_sample.getFocusDistance(),
            alpha);

        const float mid_film_width = asf::lerp<float>(
            floor_sample.getHorizontalAperture(),
            ceil_sample.getHorizontalAperture(),
            alpha);

        const float mid_film_height = asf::lerp<float>(
            floor_sample.getVerticalAperture(),
            ceil_sample.getVerticalAperture(),
            alpha);

        // maya related
        /*const float mid_aspect_ratio = asf::lerp<float>(
            floor_sample.getLensSqueezeRatio(),
            ceil_sample.getLensSqueezeRatio(),
            alpha);*/

        const float mid_near_z = asf::lerp<float>(
            floor_sample.getNearClippingPlane(),
            ceil_sample.getNearClippingPlane(),
            alpha);

        asr::ParamArray& cam_params = m_camera->get_parameters();

        cam_params.insert("focal_length", mid_focal_length/1000.0f);  // to mm
        cam_params.insert("f_stop", mid_f_stop);
        cam_params.insert("focal_distance", mid_focal_distance);
        cam_params.insert("film_width", mid_film_width / 100.0f);
        cam_params.insert("film_height", mid_film_height / 100.0f);
        cam_params.insert("near_z", -mid_near_z / 100.0f);

        cam_params.insert("shutter_open_time", shutter_open_time);
        cam_params.insert("shutter_close_time", shutter_close_time);

        ///////////////////////////////////////////////////////////////////////////
        // xform
        ///////////////////////////////////////////////////////////////////////////
        // generate xform sequence stack from our camera
        std::vector<asr::TransformSequence> xform_seq_stack = xform_seq_for_obj(
            cur_obj,
            shutter_open_time,
            shutter_close_time,
            m_time_offset);

        const asr::TransformSequence xform_seq = flatten_xform_seq(xform_seq_stack);

        m_camera->transform_sequence() = xform_seq;

        // of course, we don't forget to call the camera method
        return m_camera->on_render_begin(project, abort_switch);
    }

    void spawn_ray(
        asr::SamplingContext&    sampling_context,
        const asf::Dual2d&       ndc,
        asr::ShadingRay&         ray) const override
    {
        m_camera->spawn_ray(sampling_context, ndc, ray);
    }

    bool connect_vertex(
        asr::SamplingContext&    sampling_context,
        const float         time,
        const asf::Vector3d&     point,
        asf::Vector2d&           ndc,
        asf::Vector3d&           outgoing,
        float&              importance) const override
    {
        m_camera->connect_vertex(sampling_context, time, point, ndc, outgoing, importance);
    }

    bool project_camera_space_point(
        const asf::Vector3d&     point,
        asf::Vector2d&           ndc) const override
    {
        m_camera->project_camera_space_point(point, ndc);
    }

    bool project_segment(
        const float         time,
        const asf::Vector3d&     a,
        const asf::Vector3d&     b,
        asf::Vector2d&           a_ndc,
        asf::Vector2d&           b_ndc) const override
    {
        m_camera->project_segment(time, a, b, a_ndc, b_ndc);
    }

  private:
    // Parameters.
    std::string m_file_path;
    std::string m_obj_path;
    float m_time_offset;

    const char* Model = "alembic_camera";

    asf::auto_release_ptr<asr::Camera> m_camera;
};


//
// Alembic camera factory.
//

class AlembicCameraFactory
  : public asr::ICameraFactory
{
  public:
    // Delete this instance.
    void release() override
    {
        delete this;
    }

    // Return a string identifying this camera model.
    const char* get_model() const override
    {
        return Model;
    }

    // Return metadata for this camera model.
    asf::Dictionary get_model_metadata() const override
    {
        return
            asf::Dictionary()
                .insert("name", Model)
                .insert("label", "Alembic Camera")
                .insert("default_model", "true");
    }

    // Return metadata for the inputs of this camera model.
    asf::DictionaryArray get_input_metadata() const override
    {
        asf::DictionaryArray metadata = asr::CameraFactory::get_input_metadata();

        metadata.push_back(
            asf::Dictionary()
                .insert("name", "file_path")
                .insert("label", "File path")
                .insert("type", "text")
                .insert("use", "required"));

        metadata.push_back(
            asf::Dictionary()
                .insert("name", "obj_path")
                .insert("label", "Object path")
                .insert("type", "text")
                .insert("use", "required"));

        metadata.push_back(
            asf::Dictionary()
                .insert("name", "time_offset")
                .insert("label", "Time offset")
                .insert("type", "numeric")
                .insert("default", "0.0")
                .insert("min",
                    asf::Dictionary()
                        .insert("value", "-1.0")
                        .insert("type", "soft"))
                .insert("max",
                    asf::Dictionary()
                        .insert("value", "1.0")
                        .insert("type", "soft"))
                .insert("use", "optional"));

        return metadata;
    }

    // Create a new camera instance.
    asf::auto_release_ptr<asr::Camera> create(
        const char*              name,
        const asr::ParamArray&   params) const override
    {
        return asf::auto_release_ptr<asr::Camera>(
            new AlembicCamera(name, params));
    }

  private:

    const char* Model = "alembic_camera";
};

}  // namespace


//
// Plugin entry point.
//

extern "C"
{
    APPLESEED_DLL_EXPORT asr::IAssemblyFactory* appleseed_create_assembly_factory()
    {
        return new AlembicAssemblyFactory();
    }

    APPLESEED_DLL_EXPORT asr::ICameraFactory* appleseed_create_camera_factory()
    {
        return new AlembicCameraFactory();
    }
}
