
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Fedor Matantsev, The appleseedhq Organization
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
#include "embreescene.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/math/area.h"
#include "foundation/math/fp.h"
#include "foundation/math/intersection/raytrianglemt.h"
#include "foundation/math/minmax.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/platform/sse.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"

using namespace foundation;
using namespace renderer;
using namespace std;

namespace renderer
{

class EmbreeGeometryData
  : public NonCopyable
{
  public:
    // Vertex data.
    GVector3*               m_vertices;
    unsigned int            m_vertices_count;
    unsigned int            m_vertices_stride;

    // Primitive data.
    uint32*                 m_primitives;
    size_t                  m_primitives_count;
    size_t                  m_primitives_stride;

    // Instance data.
    size_t                  m_object_instance_idx;
    uint32                  m_vis_flags;
    unsigned int            m_motion_steps_count;

    RTCGeometryType         m_geometry_type;
    RTCGeometry             m_geometry_handle;

    EmbreeGeometryData()
      : m_vertices(nullptr)
      , m_primitives(nullptr)
      , m_geometry_handle(nullptr)
    {}

    ~EmbreeGeometryData()
    {
        delete[] m_vertices;
        delete[] m_primitives;
        rtcReleaseGeometry(m_geometry_handle);
    }
};

namespace
{
    void collect_triangle_data(
        const ObjectInstance&   object_instance,
        EmbreeGeometryData&     geometry_data)
    {
        assert(geometry_data.m_geometry_type == RTC_GEOMETRY_TYPE_TRIANGLE);

        // Retrieve object space -> assembly space transform for the object instance.
        const Transformd& transform = object_instance.get_transform();

        // Retrieve the object.
        Object& object = object_instance.get_object();

        const MeshObject& mesh = static_cast<const MeshObject&>(object);
        const StaticTriangleTess& tess = mesh.get_static_triangle_tess();

        const unsigned int motion_steps_count = static_cast<unsigned int>(tess.get_motion_segment_count()) + 1;
        geometry_data.m_motion_steps_count = motion_steps_count;

        //
        // Retrieve per vertex data.
        //
        const unsigned int vertices_count = static_cast<unsigned int>(tess.m_vertices.size());
        geometry_data.m_vertices_count = vertices_count;
        geometry_data.m_vertices_stride = sizeof(GVector3);

        // Allocate memory for the vertices. Keep one extra vertex for padding.
        geometry_data.m_vertices = new GVector3[vertices_count * motion_steps_count + 1];

        // Retrieve assembly space vertices.
        for (size_t i = 0; i < vertices_count; ++i)
        {
            const GVector3& vertex_os = tess.m_vertices[i];
            geometry_data.m_vertices[i] = transform.point_to_parent(vertex_os);
        }

        for (size_t m = 1; m < motion_steps_count; ++m)
        {
            for (size_t i = 0; i < vertices_count; ++i)
            {
                const GVector3& vertex_os = tess.get_vertex_pose(i, m - 1);
                geometry_data.m_vertices[vertices_count * m + i] = transform.point_to_parent(vertex_os);
            }
        }

        //
        // Retrieve per primitive data.
        //
        const size_t primitives_count = tess.m_primitives.size();

        geometry_data.m_primitives = new uint32[primitives_count * 3];
        geometry_data.m_primitives_stride = sizeof(uint32) * 3;
        geometry_data.m_primitives_count = primitives_count;

        for (size_t i = 0; i < primitives_count; ++i)
        {
            geometry_data.m_primitives[i * 3] = tess.m_primitives[i].m_v0;
            geometry_data.m_primitives[i * 3 + 1] = tess.m_primitives[i].m_v1;
            geometry_data.m_primitives[i * 3 + 2] = tess.m_primitives[i].m_v2;
        }
    };

    void collect_curve_data(
        const ObjectInstance&   object_instance,
        EmbreeGeometryData&     geometry_data)
    {
        //const Transformd& transform = object_instance.get_transform();

        switch(geometry_data.m_geometry_type)
        {
        case RTC_GEOMETRY_TYPE_FLAT_BEZIER_CURVE:
            //
            // [Note: Girish] Retrieve data from CurveObject here and push it to geometry_data.
            //

            break;

        default:
            assert(!"Unsupported geometry type.");
            break;
        }
    }

    // Returns minimal tnear needed to compensate double to float transition of ray fields.
    float get_tnear_offset(const RTCRay& ray)
    {
        float max_dir_component = max(abs(ray.dir_x), abs(ray.dir_y), abs(ray.dir_z));
        uint32 max_origin_exp = max(
            FP<float>::exponent(ray.org_x),
            FP<float>::exponent(ray.org_y),
            FP<float>::exponent(ray.org_z));

        // Calculate exponent-adaptive offset.
        // Note: float is represented in memory
        // as 1 sign bit, 8 exponent bits and 23 mantissa bits.
        // Higher 24th bit is always 1 in normalized form, hence it's ommited.
        // Mantissa of constructed float will overlap no more than 11 last bits of
        // origin components due to exponent shift.
        // Mantissa of constructed float is just a
        // sequence of 11 ones followed by zeroes.

        const float offset = FP<float>::construct(
            0,
            max(static_cast<int32>(max_origin_exp - 23 + 11), 0),
            2047UL << (23 - 11));

        // Divide by max_dir_component to compensate inverse operation
        // during intersection search. (Actual start point is org + dir * tnear)
        return offset / max_dir_component;
    }

    void shading_ray_to_embree_ray(
        const ShadingRay&       shading_ray,
        RTCRay&                 embree_ray)
    {
        embree_ray.org_x = static_cast<float>(shading_ray.m_org.x);
        embree_ray.org_y = static_cast<float>(shading_ray.m_org.y);
        embree_ray.org_z = static_cast<float>(shading_ray.m_org.z);

        embree_ray.dir_x = static_cast<float>(shading_ray.m_dir.x);
        embree_ray.dir_y = static_cast<float>(shading_ray.m_dir.y);
        embree_ray.dir_z = static_cast<float>(shading_ray.m_dir.z);

        embree_ray.tfar = static_cast<float>(shading_ray.m_tmax);
        embree_ray.time = static_cast<float>(shading_ray.m_time.m_normalized);
        embree_ray.mask = shading_ray.m_flags;

        const float tnear_offset = get_tnear_offset(embree_ray);

        embree_ray.tnear = static_cast<float>(shading_ray.m_tmin) + tnear_offset;
    }
}


//
//  EmbreeDevice class implementation.
//

EmbreeDevice::EmbreeDevice()
{
    // todo: set number of threads.
    m_device = rtcNewDevice(nullptr);
};

EmbreeDevice::~EmbreeDevice()
{
    rtcReleaseDevice(m_device);
}


//
//  EmbreeScene class implementation.
//

EmbreeScene::EmbreeScene(const EmbreeScene::Arguments& arguments)
{
    // Start stopwatch.
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    Statistics statistics;

    m_device = arguments.m_device.m_device;
    m_scene = rtcNewScene(m_device);

    rtcSetSceneBuildQuality(
        m_scene,
        RTCBuildQuality::RTC_BUILD_QUALITY_HIGH);

    const ObjectInstanceContainer& instance_container = arguments.m_assembly.object_instances();

    const size_t instance_count = instance_container.size();

    m_geometry_container.reserve(instance_count);

    for (size_t instance_idx = 0; instance_idx < instance_count; ++instance_idx)
    {
        const ObjectInstance* object_instance = instance_container.get_by_index(instance_idx);
        assert(object_instance);

        RTCGeometry geometry_handle;

        // Set per instance data.
        unique_ptr<EmbreeGeometryData> geometry_data(new EmbreeGeometryData());
        geometry_data->m_object_instance_idx = instance_idx;
        geometry_data->m_vis_flags = object_instance->get_vis_flags();

        //
        // Collect geometry data for the instance.
        //
        const char* object_model = object_instance->get_object().get_model();

        if (strcmp(object_model, MeshObjectFactory().get_model()) == 0)
        {
            geometry_data->m_geometry_type = RTC_GEOMETRY_TYPE_TRIANGLE;

            // Retrieve triangle data.
            collect_triangle_data(*object_instance, *geometry_data);

            geometry_handle = rtcNewGeometry(
                m_device,
                RTC_GEOMETRY_TYPE_TRIANGLE);

            rtcSetGeometryBuildQuality(
                geometry_handle,
                RTCBuildQuality::RTC_BUILD_QUALITY_HIGH);

            rtcSetGeometryTimeStepCount(
                geometry_handle,
                geometry_data->m_motion_steps_count);

            geometry_data->m_geometry_handle = geometry_handle;

            const unsigned int vertices_count = geometry_data->m_vertices_count;
            const unsigned int vertices_stride = geometry_data->m_vertices_stride;

            for (unsigned int m = 0; m < geometry_data->m_motion_steps_count; ++m)
            {
                // Byte offset for the current motion segment.
                const unsigned int vertices_offset = m * vertices_count * vertices_stride;

                // Set vertices.
                rtcSetSharedGeometryBuffer(
                    geometry_handle,                            // geometry
                    RTC_BUFFER_TYPE_VERTEX,                     // buffer type
                    m,                                          // slot
                    RTC_FORMAT_FLOAT3,                          // format
                    geometry_data->m_vertices,                  // buffer
                    vertices_offset,                            // byte offset
                    vertices_stride,                            // byte stride
                    vertices_count);                            // item count
            }

            // Set vertex indices.
            rtcSetSharedGeometryBuffer(
                geometry_handle,                                // geometry
                RTC_BUFFER_TYPE_INDEX,                          // buffer type
                0,                                              // slot
                RTC_FORMAT_UINT3,                               // format
                geometry_data->m_primitives,                    // buffer
                0,                                              // byte offset
                geometry_data->m_primitives_stride,             // byte stride
                geometry_data->m_primitives_count);             // item count

            rtcSetGeometryMask(
                geometry_handle,
                geometry_data->m_vis_flags);

            rtcCommitGeometry(geometry_handle);
        }
        else if (strcmp(object_model, CurveObjectFactory().get_model()) == 0)
        {
            geometry_data->m_geometry_type = RTC_GEOMETRY_TYPE_FLAT_BEZIER_CURVE;

            // Retrieve curve data.
            collect_curve_data(*object_instance, *geometry_data);

            geometry_handle = rtcNewGeometry(
                m_device,
                RTC_GEOMETRY_TYPE_FLAT_BEZIER_CURVE);

            rtcSetGeometryBuildQuality(
                geometry_handle,
                RTCBuildQuality::RTC_BUILD_QUALITY_HIGH);

            // Set vertices. (x_pos, y_pos, z_pos, radii)
            rtcSetSharedGeometryBuffer(
                geometry_handle,                                // geometry
                RTC_BUFFER_TYPE_INDEX,                          // buffer type
                0,                                              // slot
                RTC_FORMAT_FLOAT4,                              // format
                geometry_data->m_vertices,                      // buffer
                0,                                              // byte offset
                geometry_data->m_vertices_stride,               // byte stride
                geometry_data->m_vertices_count);               // item count

            // Set vertex indices.
            rtcSetSharedGeometryBuffer(
                geometry_handle,                                // geometry
                RTC_BUFFER_TYPE_INDEX,                          // buffer type
                0,                                              // slot
                RTC_FORMAT_UINT4,                               // format
                geometry_data->m_primitives,                    // buffer
                0,                                              // byte offset
                geometry_data->m_primitives_stride,             // byte stride
                geometry_data->m_primitives_count);             // item count

        }
        else
        {
            // Unsupported object type.
            continue;
        }

        rtcAttachGeometryByID(m_scene, geometry_handle, static_cast<unsigned int>(instance_idx));
        m_geometry_container.push_back(std::move(geometry_data));
    }

    rtcCommitScene(m_scene);

    statistics.insert_time("total build time", stopwatch.measure().get_seconds());

    RENDERER_LOG_DEBUG("%s",
        StatisticsVector::make(
            "Embree scene #" + to_string(arguments.m_assembly.get_uid()) + " statistics",
            statistics).to_string().c_str());
}

EmbreeScene::~EmbreeScene()
{
    rtcReleaseScene(m_scene);
}

void EmbreeScene::intersect(ShadingPoint& shading_point) const
{
    RTCIntersectContext context;
    rtcInitIntersectContext(&context);

    RTCRayHit rayhit;
    shading_ray_to_embree_ray(shading_point.get_ray(), rayhit.ray);

    rayhit.hit.geomID = RTC_INVALID_GEOMETRY_ID;

    rtcIntersect1(m_scene, &context, &rayhit);

    if (rayhit.hit.geomID != RTC_INVALID_GEOMETRY_ID)
    {
        assert(rayhit.hit.geomID < m_geometry_container.size());

        const auto& geometry_data = m_geometry_container[rayhit.hit.geomID];
        assert(geometry_data);

        shading_point.m_bary[0] = rayhit.hit.u;
        shading_point.m_bary[1] = rayhit.hit.v;

        shading_point.m_object_instance_index = geometry_data->m_object_instance_idx;
        // TODO: remove regions
        shading_point.m_primitive_index = rayhit.hit.primID;
        shading_point.m_primitive_type = ShadingPoint::PrimitiveTriangle;
        shading_point.m_ray.m_tmax = rayhit.ray.tfar;

        const uint32 v0_idx = geometry_data->m_primitives[rayhit.hit.primID * 3];
        const uint32 v1_idx = geometry_data->m_primitives[rayhit.hit.primID * 3 + 1];
        const uint32 v2_idx = geometry_data->m_primitives[rayhit.hit.primID * 3 + 2];

        if (geometry_data->m_motion_steps_count > 1)
        {
            const uint32 last_motion_step_idx = geometry_data->m_motion_steps_count - 1;

            const uint32 motion_step_begin_idx = static_cast<uint32>(rayhit.ray.time * last_motion_step_idx);
            const uint32 motion_step_end_idx = motion_step_begin_idx + 1;

            const uint32 motion_step_begin_offset = motion_step_begin_idx * geometry_data->m_vertices_count;
            const uint32 motion_step_end_offset = motion_step_end_idx * geometry_data->m_vertices_count;

            const float motion_step_begin_time = static_cast<float>(motion_step_begin_idx) / last_motion_step_idx;

            // Linear interpolation coefficients.
            const float p = (rayhit.ray.time - motion_step_begin_time) * last_motion_step_idx;
            const float q = 1.0f - p;

            assert(p > 0.0f && p <= 1.0f);

            const TriangleType triangle(
                Vector3d(
                    geometry_data->m_vertices[motion_step_begin_offset + v0_idx] * q
                    + geometry_data->m_vertices[motion_step_end_offset + v0_idx] * p),
                Vector3d(
                    geometry_data->m_vertices[motion_step_begin_offset + v1_idx] * q
                    + geometry_data->m_vertices[motion_step_end_offset + v1_idx] * p),
                Vector3d(
                    geometry_data->m_vertices[motion_step_begin_offset + v2_idx] * q
                    + geometry_data->m_vertices[motion_step_end_offset + v2_idx] * p));

            shading_point.m_triangle_support_plane.initialize(triangle);
        }
        else
        {
            const TriangleType triangle(
                Vector3d(geometry_data->m_vertices[v0_idx]),
                Vector3d(geometry_data->m_vertices[v1_idx]),
                Vector3d(geometry_data->m_vertices[v2_idx]));

            shading_point.m_triangle_support_plane.initialize(triangle);
        }
    }
}

bool EmbreeScene::occlude(const ShadingRay& shading_ray) const
{
    RTCIntersectContext context;
    rtcInitIntersectContext(&context);

    RTCRay ray;
    shading_ray_to_embree_ray(shading_ray, ray);

    rtcOccluded1(
        m_scene,
        &context,
        &ray);

    if (ray.tfar < signed_min<float>())
        return true;

    return false;
}

EmbreeSceneFactory::EmbreeSceneFactory(const EmbreeScene::Arguments& arguments)
  : m_arguments(arguments)
{
}

unique_ptr<EmbreeScene> EmbreeSceneFactory::create()
{
    return unique_ptr<EmbreeScene>(new EmbreeScene(m_arguments));
}

}   // namespace renderer
