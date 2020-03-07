
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Dorian Fevrier, The appleseedhq Organization
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
#include "renderer/api/camera.h"
#include "renderer/api/log.h"
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/rasterization.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/triangulator.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/otherwise.h"

// appleseed.main headers.
#include "main/dllvisibility.h"

// Alembic headers.
#include <Alembic/AbcCoreFactory/All.h>
#include <Alembic/AbcGeom/All.h>

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <string>
#include <vector>

namespace
{
    // Shortcuts to save space.
    namespace asf = foundation;
    namespace asr = renderer;

    namespace Abc        = Alembic::Abc;
    namespace AbcCoreFac = Alembic::AbcCoreFactory;
    namespace AbcGeom    = Alembic::AbcGeom;
    namespace AbcUtil    = Alembic::Util;

    typedef Alembic::Abc::index_t  AbcIndex_t;
    typedef Alembic::Abc::chrono_t AbcChrono_t;

    // Roll into given matrix stack and return the flatten one.
    asf::Matrix4d flatten_xform(const std::vector<asf::Matrix4d>& mtx_stack)
    {
        // The matrix we will return.
        asf::Matrix4d out_mtx = mtx_stack.front();

        // Multiply the each matrix of the stack from bottom to top.
        for (size_t i = 1; i < mtx_stack.size(); i++)
        {
            out_mtx = mtx_stack[i] * out_mtx;
        }

        return out_mtx;
    }

    // Roll into given xform sequence stack and return the flatten one.
    asr::TransformSequence flatten_xform_seq(const std::vector<asr::TransformSequence>& xform_seq_stack)
    {
        assert(!xform_seq_stack.empty());

        if (xform_seq_stack.size() == 1)
        {
            // one item, we don't need to flatten anything
            return xform_seq_stack.back();
        }
        else
        {
            // xform sequence we will return
            asr::TransformSequence out_xform_seq;

            for (const auto& xform_seq : xform_seq_stack)
            {
                out_xform_seq = xform_seq * out_xform_seq;
            }

            out_xform_seq.optimize();
            out_xform_seq.prepare();

            return out_xform_seq;
        }
    }

    // Return if given time samples are linearly time spaced between start_time
    // and end_time.
    bool are_linearly_sampled(
        const AbcChrono_t start_time,
        const AbcChrono_t end_time,
        const std::vector<AbcChrono_t>& samples)
    {
        assert(samples.size() > 1);

        // Compute time supposed to be between each samples if samples are
        // linearly sampled.
        const AbcChrono_t increm = (end_time - start_time) / (samples.size()-1);

        // Start sample.
        AbcChrono_t t_accum = samples[0];

        // Get each sample time and compare with a linearly sampled time.
        for (const AbcChrono_t t : samples)
        {
            if (!asf::feq(t, t_accum))
            {
                return false;
            }

            t_accum += increm;
        }
        return true;
    }


    // Return sample indices (with border samples) in given shutter.
    //                           open          close
    // shutter times       ---------|--------------|------------->
    //                  (floor)  ,<-'              '-->,  (ceil)
    // returned samples    ------|-----------|---------|--------->
    std::vector<AbcIndex_t> schema_to_sample_times(
        const float                shutter_open,
        const float                shutter_close,
        const size_t               num_samples,
        const Abc::TimeSamplingPtr time_sampling)
    {
        assert(shutter_open <= shutter_close);

        // The index array we will return.
        std::vector<AbcIndex_t> sample_indices;

        if (num_samples < 2)
        {
            // No sample, put the main sample (sample 0).
            sample_indices.push_back(0);
        }
        else  // More than 1 sample.
        {
            // Get floor and ceil sample indices to not miss any.
            const std::pair<AbcIndex_t, AbcChrono_t>& open_pair =
                time_sampling->getFloorIndex(shutter_open, num_samples);

            const std::pair<AbcIndex_t, AbcChrono_t>& close_pair =
                time_sampling->getCeilIndex(shutter_close, num_samples);

            sample_indices.reserve(close_pair.first-open_pair.first + 1);

            // Get _every_ sample indices between open and close.
            for (AbcIndex_t i = open_pair.first; i <= close_pair.first; i++)
            {
                sample_indices.push_back(i);
            }
        }
        return sample_indices;
    }

    // Extract an appleseed xform sequence from given alembic xform schema.
    asr::TransformSequence extract_xform_seq(
        const AbcGeom::IXformSchema& schema,
        const AbcChrono_t shutter_open,
        const AbcChrono_t shutter_close,
        const AbcChrono_t time_offset)
    {
        assert(schema.getNumOps() > 0);
        assert(shutter_open <= shutter_close);

        const Abc::TimeSamplingPtr time_sampling = schema.getTimeSampling();

        // Find samples in shutter.
        const auto sample_indices = schema_to_sample_times(
            shutter_open  + time_offset,
            shutter_close + time_offset,
            schema.getNumSamples(),
            time_sampling);

        // Create and return the xform sequence from sample founds.
        asr::TransformSequence xform_seq;

        for (const AbcIndex_t i : sample_indices)
        {
            // Time.
            const AbcChrono_t t = time_sampling->getSampleTime(i);

            // Convert alembic xform to appleseed xform.
            const auto sample = schema.getValue(i);
            const auto abc_mtx = sample.getMatrix();
            const auto mtx = asf::Matrix4d(abc_mtx);
            const auto xform = asf::Transformd::from_local_to_parent(mtx);

            // As all samples are time offset, we offset them back.
            xform_seq.set_transform(t-time_offset, xform);
        }

        return xform_seq;
    }

    // Log information for given alembic archive.
    void log_archive(
        Abc::IArchive& archive,
        AbcCoreFac::IFactory::CoreType core_type)
    {
        RENDERER_LOG_INFO("archive name: %s\n"
                          "        time sampling count: %d\n"
                          "        version: %d",
            archive.getName().c_str(),
            archive.getNumTimeSamplings(),
            archive.getArchiveVersion());
        switch (core_type)
        {
            case AbcCoreFac::IFactory::kHDF5:
                RENDERER_LOG_INFO("        core type: HDF5");
                break;
            case AbcCoreFac::IFactory::kOgawa:
                RENDERER_LOG_INFO("        core type: Ogawa");
                break;
            case AbcCoreFac::IFactory::kLayer:
                RENDERER_LOG_INFO("        core type: Layer");
                break;
            case AbcCoreFac::IFactory::kUnknown:
                RENDERER_LOG_WARNING("        core type: Unknown");
                break;
            assert_otherwise;
        }
    }

    // Log information for given alembic object.
    void log_obj(const Abc::IObject& obj)
    {
        RENDERER_LOG_INFO("%s\n"
                          "  children count: " FMT_SIZE_T,
            obj.getFullName().c_str(),
            obj.getNumChildren());

        const Abc::MetaData& metadata = obj.getMetaData();

        RENDERER_LOG_INFO("  metadata size: " FMT_SIZE_T, metadata.size());

        if (metadata.size())
        {
            for (const auto& it : metadata)
            {
                const std::string& key   = it.first;
                const std::string& value = it.second;

                RENDERER_LOG_INFO("           %s = %s",
                        key.c_str(),
                        value.c_str());
            }
        }
    }

    void log_data_type(const Abc::DataType& data_type)
    {
        switch (data_type.getPod())
        {
            case AbcUtil::kBooleanPOD:
                RENDERER_LOG_INFO("                     data type POD type: boolean");
                break;
            case AbcUtil::kUint8POD:
                RENDERER_LOG_INFO("                     data type POD type: 8bit unsigned integer");
                break;
            case AbcUtil::kInt8POD:
                RENDERER_LOG_INFO("                     data type POD type: 8bit integer");
                break;
            case AbcUtil::kUint16POD:
                RENDERER_LOG_INFO("                     data type POD type: 16bit unsigned integer");
                break;
            case AbcUtil::kInt16POD:
                RENDERER_LOG_INFO("                     data type POD type: 16bit integer");
                break;
            case AbcUtil::kUint32POD:
                RENDERER_LOG_INFO("                     data type POD type: 32bit unsigned integer");
                break;
            case AbcUtil::kInt32POD:
                RENDERER_LOG_INFO("                     data type POD type: 32bit integer");
                break;
            case AbcUtil::kFloat16POD:
                RENDERER_LOG_INFO("                     data type POD type: 16bit float");
                break;
            case AbcUtil::kFloat32POD:
                RENDERER_LOG_INFO("                     data type POD type: 32bit float");
                break;
            case AbcUtil::kFloat64POD:
                RENDERER_LOG_INFO("                     data type POD type: 64bit float");
                break;
            case AbcUtil::kStringPOD:
                RENDERER_LOG_INFO("                     data type POD type: string pointer");
                break;
            case AbcUtil::kWstringPOD:
                RENDERER_LOG_INFO("                     data type POD type: wide string pointer");
                break;
            case AbcUtil::kNumPlainOldDataTypes:
                RENDERER_LOG_INFO("                     data type POD type: number of POD");
                break;
            case AbcUtil::kUnknownPOD:
                RENDERER_LOG_WARNING("                  data type POD type: unknown");
                break;
            assert_otherwise;
        }

        RENDERER_LOG_INFO("                            extent: %d", data_type.getExtent());
        //RENDERER_LOG_INFO("                            byte count (POD type size * extent): %zd", data_type.getNumBytes());
    }

    void log_polymesh_schema(const AbcGeom::IPolyMeshSchema& schema)
    {
        RENDERER_LOG_INFO("  schema sample count: " FMT_SIZE_T, schema.getNumSamples());
        RENDERER_LOG_INFO("         property count: " FMT_SIZE_T, schema.getNumProperties());

        switch (schema.getTopologyVariance())
        {
            case AbcGeom::kConstantTopology:
                RENDERER_LOG_INFO("         topology variance: constant (non deformed)");
                break;
            case AbcGeom::kHomogeneousTopology:
                RENDERER_LOG_INFO("         topology variance: homogeneous (deformed but no topology change)");
                break;
            case AbcGeom::kHeterogeneousTopology:
                RENDERER_LOG_INFO("         topology variance: heterogeneous (fluid geos like realflow bins, etc.)");
                break;
            default:
                RENDERER_LOG_WARNING("         topology variance: unknown");
                break;
        }

        for (auto i = 0; i < schema.getNumProperties(); i++)
        {
            const Abc::PropertyHeader& prop_header = schema.getPropertyHeader(i);

            RENDERER_LOG_INFO("         property name: %s", prop_header.getName().c_str());
            //RENDERER_LOG_INFO("                  num samples: %zd", prop_header.getNumSamples());

            switch (prop_header.getPropertyType())
            {
                case Abc::kCompoundProperty:
                    RENDERER_LOG_INFO("                  type: compound");
                    break;
                case Abc::kScalarProperty:
                    RENDERER_LOG_INFO("                  type: scalar");
                    break;
                case Abc::kArrayProperty:
                    RENDERER_LOG_INFO("                  type: array");
                    break;
            }

            log_data_type(prop_header.getDataType());
        }
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
        }

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
            ///////////////////////////////////////////////////////////////////
            // Retrieve assembly parameters.
            ///////////////////////////////////////////////////////////////////
            const auto params = get_parameters();

            // File path.
            m_file_path = params.get<std::string>("file_path");

            if (m_file_path.empty())
            {
                RENDERER_LOG_ERROR("empty 'file_path' property: %s", get_name());
                return false;
            }

            // Shutters.
            m_shutter_open = params.get_optional<float>("shutter_open_time", 0.0f);
            m_shutter_close = params.get_optional<float>("shutter_close_time", 0.0f);

            // Time offset.
            m_time_offset = params.get_optional<float>("time_offset", 0.0f);

            // Verbose mode.
            m_verbose = params.get_optional<bool>("verbose", false);

            if (m_verbose)
            {
                RENDERER_LOG_INFO("file_path: %s\n"
                                  "shutter_open: %f\n"
                                  "shutter_close: %f\n"
                                  "time_offset: %f\n"
                                  "verbose: %s",
                    m_file_path.c_str(),
                    m_shutter_open,
                    m_shutter_close,
                    m_time_offset,
                    m_verbose ? "true" : "false");
            }

            AbcCoreFac::IFactory factory;
            AbcCoreFac::IFactory::CoreType core_type;

            Abc::IArchive archive = factory.getArchive(m_file_path, core_type);

            if (!archive.valid())
            {
                RENDERER_LOG_WARNING("invalid archive: %s", m_file_path.c_str());
                return false;
            }

            if (m_verbose)
                log_archive(archive, core_type);

            m_xform_seq_stack.push_back(asr::TransformSequence());

            // Retrieve archive root object.
            const Abc::IObject& root = archive.getTop();

            if (!root.valid())
            {
                RENDERER_LOG_WARNING("invalid root: %s", archive.getName().c_str());
                return false;
            }

            if (m_verbose)
                log_obj(root);

            // Root children.
            const size_t children_count = root.getNumChildren();

            if (children_count)
            {
                for (size_t i = 0, e = root.getNumChildren(); i < e; i++)
                {
                    const Abc::IObject& child = root.getChild(i);
                    walk(child);
                }
            }
            else
            {
                RENDERER_LOG_WARNING("no children for root: %s",
                                     archive.getName().c_str());
            }

            return true;
        }

      private:

        // The recursive method entering every children of the tree and keep a
        // xform stack.
        void walk(const Abc::IObject& obj)
        {
            if (!obj.valid())
            {
                RENDERER_LOG_WARNING("invalid object detected!");
                return;  // Stop here.
            }

            if (m_verbose)
                log_obj(obj);

            const std::string obj_name = obj.getFullName();

            const Abc::ObjectHeader& h = obj.getHeader();

            // We track if a xform sequence has been generated or not to remove
            // it after we reached every children.
            bool gen_xform = false;

            if (AbcGeom::IXform::matches(h))
            {
                const auto& abc_xform = AbcGeom::IXform(obj);

                const AbcGeom::IXformSchema& xform_schema = abc_xform.getSchema();

                if (xform_schema.getNumOps() > 0 &&
                    !xform_schema.isConstantIdentity())
                {
                    asr::TransformSequence xform_seq = extract_xform_seq(
                        xform_schema,
                        m_shutter_open,
                        m_shutter_close,
                        m_time_offset);

                    m_xform_seq_stack.push_back(xform_seq);

                    gen_xform = true;
                }

            }
            else if (AbcGeom::IPolyMesh::matches(h))
            {
                const auto polymesh = AbcGeom::IPolyMesh(obj);

                const AbcGeom::IPolyMeshSchema& schema = polymesh.getSchema();

                if (m_verbose)
                    log_polymesh_schema(schema);

                /*const auto sample_times = schema_to_sample_timesOO(m_shutter_open+m_time_offset,
                                                                   m_shutter_close+m_time_offset,
                                                                   schema.getNumSamples(),
                                                                   schema.getTimeSampling());*/


                const auto sample_times = schema_to_sample_times(m_shutter_open+m_time_offset,
                                                                 m_shutter_close+m_time_offset,
                                                                 schema.getNumSamples(),
                                                                 schema.getTimeSampling());

                // Compute appleseed number of motion segment
                const auto motion_segment_count = sample_times.size() - 1;

                /*if (motion_segment_count &&
                    !are_linearly_sampled(m_shutter_open,
                                          m_shutter_close,
                                          sample_times))
                {
                    RENDERER_LOG_WARNING("polymesh motion samples does not match shutters: \"%s\"",
                                         obj_name.c_str());
                }*/

                // Create a new appleseed mesh object
                auto as_obj = asf::auto_release_ptr<asr::MeshObject>(
                    asr::MeshObjectFactory().create(
                        obj_name.c_str(),
                        asr::ParamArray()));

                as_obj->set_motion_segment_count(motion_segment_count);

                ///////////////////////////////////////////////////////////////
                // Vertex positions.
                ///////////////////////////////////////////////////////////////
                size_t mb_segment_id = 0;

                for (const AbcIndex_t i : sample_times)
                {
                    const AbcGeom::IPolyMeshSchema::Sample& sample = schema.getValue(i);

                    const auto pos = sample.getPositions()->get();
                    const auto pos_count = sample.getPositions()->size();

                    // Reserve space to optimize allocation.
                    as_obj->reserve_vertices(pos_count);

                    for (auto i = 0; i < pos_count; i++)
                    {
                        const asr::GVector3 v(pos[i][0], pos[i][1], pos[i][2]);

                        if (mb_segment_id == 0)  // First sample.
                        {
                            as_obj->push_vertex(v);
                        }
                        else  // All other samples.
                        {
                            as_obj->set_vertex_pose(i, mb_segment_id-1, v);
                        }
                    }

                    mb_segment_id++;
                }

                ///////////////////////////////////////////////////////////////
                // UVs.
                ///////////////////////////////////////////////////////////////
                std::vector<size_t> uv_idxs;  // UV indices.

                const auto uv_param = schema.getUVsParam();
                if (uv_param.valid())
                {
                    auto uv_sample = uv_param.getIndexedValue();
                    if (uv_sample.valid())
                    {
                        // Retrieve UV indices.
                        const auto abc_uv_idxs = uv_sample.getIndices()->get();
                        const auto uv_idxs_count = uv_sample.getIndices()->size();

                        uv_idxs.reserve(uv_idxs_count);

                        for (auto i = 0; i < uv_idxs_count; i++)
                        {
                            uv_idxs.push_back(abc_uv_idxs[i]);
                        }

                        // UV vectors.
                        const auto uvs = uv_sample.getVals()->get();
                        const auto uv_count = uv_sample.getVals()->size();

                        // Reserve for optimization purpose.
                        as_obj->reserve_tex_coords(uv_count);

                        for (auto i = 0; i < uv_count; ++i)
                        {
                            const asf::Vector2f uv(uvs[i]);
                            as_obj->push_tex_coords(uv);
                        }
                    }
                }

                ///////////////////////////////////////////////////////////////
                // Normals.
                ///////////////////////////////////////////////////////////////
                std::vector<size_t> n_idxs;  // Normal indices.

                const auto n_param = schema.getNormalsParam();

                if (n_param.valid())
                {
                    switch (n_param.getScope())
                    {
                        case AbcGeom::kConstantScope:
                            std::cout << "normal kConstantScope" << std::endl;
                            break;
                        case AbcGeom::kUniformScope:
                            std::cout << "normal kUniformScope" << std::endl;
                            break;
                        case AbcGeom::kVaryingScope:
                            std::cout << "normal kVaryingScope" << std::endl;
                            break;
                        case AbcGeom::kVertexScope:
                            // Normal indices match vertex one.
                            std::cout << "normal kVertexScope" << std::endl;
                            break;
                        case AbcGeom::kFacevaryingScope:
                            // Normals have their own indices.
                            std::cout << "normal kFacevaryingScope" << std::endl;
                            break;
                        case AbcGeom::kUnknownScope:
                            std::cout << "normal kUnknownScope" << std::endl;
                            break;
                        default:
                            std::cout << "invalid scope" << std::endl;
                            break;
                    }

                    size_t mb_segment_id = 0;
                    for (const AbcIndex_t i : sample_times)
                    {
                        auto n_sample = n_param.getIndexedValue();
                        //auto sample = n_param.getExpandedValue();  // TODO use this depending on the scope.
                        if (n_sample.valid())
                        {
                            // Retrieve normal indices.
                            const auto abc_n_idxs = n_sample.getIndices()->get();
                            const auto n_idxs_count = n_sample.getIndices()->size();

                            // Should be same size as vertices.
                            n_idxs.reserve(n_idxs_count);

                            for (auto i = 0; i < n_idxs_count; i++)
                            {
                                n_idxs.push_back(abc_n_idxs[i]);
                            }

                            // Normal vectors.
                            const auto normals = n_sample.getVals()->get();
                            const auto n_count = n_sample.getVals()->size();

                            // Reserve space to optimize allocation.
                            as_obj->reserve_vertex_normals(n_count);

                            if (mb_segment_id == 0)  // First sample.
                            {
                                for (auto i = 0; i < n_count; ++i)
                                {
                                    as_obj->push_vertex_normal(normals[i]);
                                }
                            }
                            else  // Other samples are put in motion poses.
                            {
                                for (auto i = 0; i < n_count; ++i)
                                {
                                    as_obj->set_vertex_normal_pose(i, mb_segment_id-1, normals[i]);
                                }
                            }
                        }
                        mb_segment_id++;
                    }
                }

                ///////////////////////////////////////////////////////////////
                // Triangles.
                ///////////////////////////////////////////////////////////////
                const AbcGeom::IPolyMeshSchema::Sample& sample = schema.getValue(0);

                const Abc::Int32ArraySamplePtr face_count_ptr = sample.getFaceCounts();  // [3,4,4,3,...]
                const int32_t*                 face_sizes = face_count_ptr->get();  // Access raw pointer.
                const size_t                   face_count = face_count_ptr->size();

                const auto face_indices = sample.getFaceIndices()->get();
                //const size_t                     face_indices_count = face_indices.size();

                // Compute the number of triangles to reserve proper space.
                size_t tri_count = 0;
                for (size_t i = 0; i < face_count; i++)
                {
                    tri_count += face_sizes[i] - 2;
                }

                // Reserve for optimization purpose.
                as_obj->reserve_triangles(tri_count);

                // Iterators.
                size_t f_i = 0;  // Face indices.
                size_t n_i = 0;  // Normal indices.
                size_t uv_i = 0;  // UV indices.

                for (auto i = 0; i < face_count; i++)
                {
                    // 3 or 4, maybe more.
                    const auto face_size = face_sizes[i];

                    if (face_size < 3)
                    {
                        RENDERER_LOG_WARNING("face with less than 3 points "
                                             "detected: \"%s\"",
                                             obj_name.c_str());
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
                            const size_t n0 = n_idxs[n_i++];
                            const size_t n1 = n_idxs[n_i++];
                            const size_t n2 = n_idxs[n_i++];
                            tri0.m_n0 = n0;
                            tri0.m_n1 = n2;
                            tri0.m_n2 = n1;
                        }

                        if (!uv_idxs.empty())
                        {
                            const size_t uv0 = uv_idxs[uv_i++];
                            const size_t uv1 = uv_idxs[uv_i++];
                            const size_t uv2 = uv_idxs[uv_i++];

                            tri0.m_a0 = uv0;
                            tri0.m_a1 = uv2;
                            tri0.m_a2 = uv1;
                        }

                        as_obj->push_triangle(tri0);
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
                            const size_t n0 = n_idxs[n_i++];
                            const size_t n1 = n_idxs[n_i++];
                            const size_t n2 = n_idxs[n_i++];
                            const size_t n3 = n_idxs[n_i++];

                            tri0.m_n0 = n0;
                            tri0.m_n1 = n2;
                            tri0.m_n2 = n1;

                            tri1.m_n0 = n2;
                            tri1.m_n1 = n0;
                            tri1.m_n2 = n3;
                        }

                        if (!uv_idxs.empty())
                        {
                            const size_t uv0 = uv_idxs[uv_i++];
                            const size_t uv1 = uv_idxs[uv_i++];
                            const size_t uv2 = uv_idxs[uv_i++];
                            const size_t uv3 = uv_idxs[uv_i++];

                            tri0.m_a0 = uv0;
                            tri0.m_a1 = uv2;
                            tri0.m_a2 = uv1;

                            tri1.m_a0 = uv2;
                            tri1.m_a1 = uv0;
                            tri1.m_a2 = uv3;
                        }

                        as_obj->push_triangle(tri0);
                        as_obj->push_triangle(tri1);
                    }
                    else  // Arbitrary sized polygon.
                    {
                        // We create a polygon and will store it's various
                        // positions.
                        asf::Triangulator<float>::Polygon3 polygon;

                        for (auto j = 0; j < face_size; j++)
                        {
                            // Get vertex id.
                            const auto id = face_indices[f_i++];

                            // Put the vertex position to the polygon.
                            polygon.emplace_back(as_obj->get_vertex(id));
                        }

                        asf::Triangulator<float> triangulator;
                        asf::Triangulator<float>::IndexArray tris;

                        const bool success = triangulator.triangulate(polygon, tris);

                        if (success)
                        {
                            // Insert all triangles of the triangulation into
                            // the mesh.
                            for (asf::Triangulator<float>::IndexArray::size_type k = 0;
                                 k < tris.size();
                                 k += 3)
                            {
                                auto tri = asr::Triangle(tris[k+0],
                                                         tris[k+1],
                                                         tris[k+2]);

                                if (!n_idxs.empty())
                                {
                                    const size_t n0 = n_idxs[k+0];
                                    const size_t n1 = n_idxs[k+1];
                                    const size_t n2 = n_idxs[k+2];

                                    tri.m_n0 = n0;
                                    tri.m_n1 = n2;
                                    tri.m_n2 = n1;
                                }

                                if (!uv_idxs.empty())
                                {
                                    const size_t uv0 = uv_idxs[k+0];
                                    const size_t uv1 = uv_idxs[k+1];
                                    const size_t uv2 = uv_idxs[k+2];

                                    tri.m_a0 = uv0;
                                    tri.m_a1 = uv2;
                                    tri.m_a2 = uv1;
                                }

                                // And finally push the triangle.
                                as_obj->push_triangle(tri);
                            }

                            // Important step, increment iterators.
                            if (!n_idxs.empty())
                                n_i += face_size;

                            if (!uv_idxs.empty())
                                uv_i += face_size;
                        }
                        else
                        {
                            RENDERER_LOG_WARNING("triangulator failed, "
                                                 "invalid polymesh: \"%s\"",
                                                 obj_name.c_str());
                        }
                    }
                }

                // TODO: For now, no material are assigned...
                auto front_material_mappings = asf::StringDictionary();

                // Create an instance of the assembly.
                asf::auto_release_ptr<asr::Assembly> xform_assembly(
                    asr::AssemblyFactory().create(  // TODO: Do I really have to instantiate AssemblyFactory?
                        (obj_name+"_assembly").c_str(),  // Assembly instance.
                        asr::ParamArray()));

                // Insert the object into the assembly.
                xform_assembly->objects().insert(
                    asf::auto_release_ptr<asr::Object>(as_obj));

                // Create an instance of this object and insert it into the assembly.
                xform_assembly->object_instances().insert(
                    asr::ObjectInstanceFactory::create(
                        (obj_name+"_inst").c_str(), // Instance name.
                        asr::ParamArray(),
                        obj_name.c_str(), // Object name.
                        asf::Transformd::identity(),
                        front_material_mappings));

                // Create an instance of the assembly.
                asf::auto_release_ptr<asr::AssemblyInstance> xform_assembly_inst(
                    asr::AssemblyInstanceFactory::create(
                        (obj_name+"_assembly_inst").c_str(),  // Assembly instance.
                        asr::ParamArray(),
                        (obj_name+"_assembly").c_str()));

                if (m_xform_seq_stack.size())
                {
                    auto xform_seq = flatten_xform_seq(m_xform_seq_stack);

                    xform_assembly_inst->transform_sequence() = xform_seq;
                }

                this->assemblies().insert(xform_assembly);
                this->assembly_instances().insert(xform_assembly_inst);

            }  // IPolyMesh.

            // Iterate over children.
            for (size_t i = 0; i < obj.getNumChildren(); i++)
            {
                const Abc::IObject& child = obj.getChild(i);
                walk(child);
            }

            if (gen_xform)
            {
                // Remove the bottom transform as we move up.
                m_xform_seq_stack.pop_back();
            }
        }

        const char* Model = "alembic_assembly";

        std::string m_file_path;
        float m_shutter_open;
        float m_shutter_close;
        float m_time_offset;
        bool m_verbose;
        std::vector<asr::TransformSequence> m_xform_seq_stack;
    };


    // Returned alpha is a factor between floor and ceil index (0.0 is floor,
    // 1.0 if ceil).
    float get_weight_and_index(
        const float time,
        const Abc::TimeSamplingPtr time_sample,
        size_t num_samples,
        AbcIndex_t& floor_index,
        AbcIndex_t& ceil_index)
    {
        if (num_samples == 0)
            num_samples = 1;

        // Find floor sample index.
        std::pair<AbcIndex_t,
                  AbcChrono_t> floor_pair =
            time_sample->getFloorIndex(time, num_samples);

        floor_index = floor_pair.first;
        ceil_index = floor_index;

        const AbcChrono_t floor_time = floor_pair.second;

        if (asf::feq(time, floor_time))
            return 0.0f;

        // Find ceil sample index.
        std::pair<AbcIndex_t,
                  AbcChrono_t> ceil_pair =
            time_sample->getCeilIndex(time, num_samples);

        if (floor_index == ceil_pair.first)
            return 0.0f;

        ceil_index = ceil_pair.first;

        const AbcChrono_t ceil_time = ceil_pair.second;

        float alpha = (time - floor_time) / (ceil_time - floor_time);

        // We so closely match the ceiling so we'll just use it.
        if (asf::feq(alpha, 1.0f))
        {
            floor_index = ceil_index;
            return 0.0f;
        }

        return alpha;
    }


    // Move from given obj to the root storing every xform then return them as
    // a sequence array (returned array can be empty).
    std::vector<asr::TransformSequence> xform_seq_for_obj(
        const AbcGeom::IObject& obj,
        const AbcChrono_t shutter_open,
        const AbcChrono_t shutter_close,
        const AbcChrono_t time_offset = 0.0f)
    {
        // Xform sequence stack we will return.
        std::vector<asr::TransformSequence> xform_seq_stack;

        // Start of the reverse recursion.
        AbcGeom::IObject cur_obj = obj;

        while (cur_obj.valid())
        {
            const AbcGeom::ObjectHeader h = cur_obj.getHeader();

            if (AbcGeom::IXform::matches(h))
            {
                auto abc_xform = AbcGeom::IXform(cur_obj);

                const AbcGeom::IXformSchema& xform_schema = abc_xform.getSchema();

                if ((xform_schema.getNumOps() > 0) &&
                     !xform_schema.isConstantIdentity())
                {
                    asr::TransformSequence xform_seq = extract_xform_seq(
                            xform_schema,
                            shutter_open,
                            shutter_close,
                            time_offset);

                    xform_seq_stack.push_back(xform_seq);
                }
            }

            // Move to parent.
            cur_obj = cur_obj.getParent();
        }

        if (xform_seq_stack.size())
        {
            // As we moved from bottom (object) to top (root), we have to
            // revert the xform stack before return it.
            std::reverse(std::begin(xform_seq_stack), std::end(xform_seq_stack));
        }

        return xform_seq_stack;
    }

    // Return object in given archive from given path.
    bool path_to_obj(
        const Abc::IArchive& archive,
        const std::string& path,
        Abc::IObject& obj)
    {
        // Retrieve root of the hierarchy.
        Abc::IObject cur_obj = archive.getTop();

        std::vector<std::string> obj_names;
        asf::split(path, "/", obj_names);


        if (obj_names.size() < 2)
        {
            RENDERER_LOG_ERROR("can't find object '%s' in archive '%s'",
                    path.c_str(), archive.getName().c_str());
            return false;
        }

        for (size_t i = 1; i < obj_names.size(); i++)
        {
            const std::string& obj_name = obj_names[i];

            bool found = false;

            for (size_t j = 0; j < cur_obj.getNumChildren(); j++)
            {
                Abc::IObject child = cur_obj.getChild(j);

                if (child.getName() == obj_name)
                {
                    // We have a winner!
                    cur_obj = child;
                    found = true;
                    break;
                }
            }
            if (!found)
            {
                RENDERER_LOG_ERROR("can't find '%s' child of '%s'",
                        obj_name.c_str(), cur_obj.getFullName().c_str());
                return false;
            }
        }

        obj = cur_obj;

        return true;
    }


    //
    // Factory for alembic assembly model.
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

        // Return metadata for this assembly model.
        asf::Dictionary get_model_metadata() const override
        {
            return
                asf::Dictionary()
                    .insert("name", Model)
                    .insert("label", "Alembic Assembly");
        }

        // Return metadata for the inputs of this object model.
        asf::DictionaryArray get_input_metadata() const override
        {
            asf::DictionaryArray metadata;

            metadata.push_back(
                asf::Dictionary()
                    .insert("name", "file_path")
                    .insert("label", "File path")
                    .insert("type", "text")
                    .insert("use", "required"));

            metadata.push_back(
                asf::Dictionary()
                    .insert("name", "shutter_open_time")
                    .insert("label", "Shutter Open Time")
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

            metadata.push_back(
                asf::Dictionary()
                    .insert("name", "shutter_close_time")
                    .insert("label", "Shutter Close Time")
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

            metadata.push_back(
                asf::Dictionary()
                    .insert("name", "time_offset")
                    .insert("label", "Time Offset")
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

            metadata.push_back(
                asf::Dictionary()
                    .insert("name", "verbose")
                    .insert("label", "Verbose")
                    .insert("type", "bool")
                    .insert("default", "false")
                    .insert("use", "optional"));

            return metadata;
        }

        // Create an alembic assembly.
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

        void print_settings() const override
        {
        }

        bool on_frame_begin(
            const asr::Project&         project,
            const asr::BaseGroup*       parent,
            asr::OnFrameBeginRecorder&  recorder,
            asf::IAbortSwitch*          abort_switch = nullptr) override
        {
            return m_camera->on_frame_begin(project, parent, recorder, abort_switch);
        }

        bool on_render_begin(
            const asr::Project&         project,
            const asr::BaseGroup*       parent,
            asr::OnRenderBeginRecorder& recorder,
            asf::IAbortSwitch*          abort_switch = nullptr) override
        {
            if (m_file_path.empty())
            {
                RENDERER_LOG_ERROR("empty 'file_path' property: \"%s\"", get_name());
                return false;
            }

            if (m_obj_path.empty())
            {
                RENDERER_LOG_ERROR("empty 'obj_path' property: \"%s\"", get_name());
                return false;
            }

            // Retrieve alembic camera parameters.
            const asr::ParamArray& params = get_parameters();

            // Shutters.
            float shutter_open = params.get_optional<float>("shutter_open_time", 0.0f);
            float shutter_close = params.get_optional<float>("shutter_close_time", 1.0f);

            if (shutter_close < shutter_open)
            {
                shutter_close = shutter_open;

                RENDERER_LOG_WARNING(
                    "shutter close (%f) is smaller than shutter open (%f) "
                    "for camera \"%s\", only shutter open will be used.",
                        shutter_close, shutter_open, get_path().c_str());
            }

            // Time offset.
            const float time_offset = params.get_optional<float>("time_offset", 0.0f);

            // Alembic archive.
            AbcCoreFac::IFactory factory;
            AbcCoreFac::IFactory::CoreType core_type;

            Abc::IArchive archive = factory.getArchive(m_file_path, core_type);

            if (!archive.valid())
            {
                RENDERER_LOG_ERROR("invalid alembic archive: \"%s\"", m_file_path.c_str());
                return false;
            }

            Abc::IObject cam_obj;

            // Find alembic object at given path.
            if (!path_to_obj(archive, m_obj_path, cam_obj))
            {
                return false;
            }

            // We have our object, check its a camera.
            if (!AbcGeom::ICamera::matches(cam_obj.getHeader()))
            {
                RENDERER_LOG_ERROR("not a camera; '%s'", cam_obj.getFullName().c_str());
                return false;
            }

            ///////////////////////////////////////////////////////////////////
            // Static values.
            ///////////////////////////////////////////////////////////////////
            const auto abc_cam = AbcGeom::ICamera(cam_obj);

            const AbcGeom::ICameraSchema& cam_schema = abc_cam.getSchema();

            // We need middle shutter to find the best value for non-motion
            // blurred variables.
            const float mid_shutter_time = (0.5f * (shutter_open+shutter_close))+time_offset;

            // Get floor and ceil sample indices from mid shutter.
            AbcIndex_t floor_index;
            AbcIndex_t ceil_index;

            const float alpha = get_weight_and_index(
                mid_shutter_time,
                cam_schema.getTimeSampling(),
                cam_schema.getNumSamples(),
                floor_index,
                ceil_index);

            // Now get floor and ceil samples.
            AbcGeom::CameraSample floor_sample;
            AbcGeom::CameraSample ceil_sample;

            cam_schema.get(floor_sample, floor_index);
            cam_schema.get(ceil_sample, ceil_index);

            // And interpolate sample camera values.
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

            // Maya related. TODO: This has a purpose, we have to find which
            // one.
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

            cam_params.insert("shutter_open_time", shutter_open);
            cam_params.insert("shutter_close_time", shutter_close);

            ///////////////////////////////////////////////////////////////////
            // Xform.
            ///////////////////////////////////////////////////////////////////
            // Generate xform sequence stack from our camera.
            const std::vector<asr::TransformSequence> xform_seq_stack = xform_seq_for_obj(
                cam_obj,
                shutter_open,
                shutter_close,
                time_offset);

            if (xform_seq_stack.size())
            {
                m_camera->transform_sequence() = flatten_xform_seq(xform_seq_stack);
            }

            // Of course, we don't forget to call the camera method.
            return m_camera->on_render_begin(project, parent, recorder, abort_switch);
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
            const float              time,
            const asf::Vector3d&     point,
            asf::Vector2d&           ndc,
            asf::Vector3d&           outgoing,
            float&                   importance) const override
        {
            m_camera->connect_vertex(sampling_context, time, point, ndc,
                                     outgoing, importance);
        }

        bool project_camera_space_point(
            const asf::Vector3d&     point,
            asf::Vector2d&           ndc) const override
        {
            m_camera->project_camera_space_point(point, ndc);
        }

        bool project_segment(
            const float              time,
            const asf::Vector3d&     a,
            const asf::Vector3d&     b,
            asf::Vector2d&           a_ndc,
            asf::Vector2d&           b_ndc) const override
        {
            m_camera->project_segment(time, a, b, a_ndc, b_ndc);
        }

        asr::RasterizationCamera get_rasterization_camera() const override
        {
            return m_camera->get_rasterization_camera();
        }

      private:
        // Parameters.
        std::string m_file_path;
        std::string m_obj_path;

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
                    .insert("label", "Alembic Camera");
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
