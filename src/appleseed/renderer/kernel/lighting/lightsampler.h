
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/transform.h"

// Standard headers.
#include <vector>

// Forward declarations.
namespace renderer      { class Assembly; }
namespace renderer      { class AssemblyInstance; }
namespace renderer      { class EDF; }
namespace renderer      { class Light; }
namespace renderer      { class Scene; }

namespace renderer
{

//
// Non-physical light.
//

class NonPhysicalLightInfo
{
  public:
    const AssemblyInstance*     m_assembly_instance;
    const Light*                m_light;
};


//
// Light-emitting triangle.
//

class EmittingTriangle
{
  public:
    const AssemblyInstance*     m_assembly_instance;
    size_t                      m_object_instance_index;
    size_t                      m_region_index;
    size_t                      m_triangle_index;
    foundation::Vector3d        m_v0, m_v1, m_v2;               // world space vertices of the triangle
    foundation::Vector3d        m_n0, m_n1, m_n2;               // world space vertex normals
    foundation::Vector3d        m_geometric_normal;             // world space geometric normal, unit-length
    TriangleSupportPlaneType    m_triangle_support_plane;       // support plane of the triangle in assembly space
    double                      m_rcp_area;                     // world space triangle area reciprocal
    const EDF*                  m_edf;
};


//
// Light sample: the result of sampling the sets of non-physical lights and light-emitting triangles.
//

class LightSample
{
  public:
    // Data for a light-emitting triangle sample.
    const EmittingTriangle*     m_triangle;
    foundation::Vector2d        m_bary;                         // barycentric coordinates of the sample
    foundation::Vector3d        m_point;                        // world space position of the sample
    foundation::Vector3d        m_shading_normal;               // world space shading normal at the sample, unit-length
    foundation::Vector3d        m_geometric_normal;             // world space geometric normal at the sample, unit-length

    // Data for a non-physical light sample.
    const Light*                m_light;
    foundation::Transformd      m_asm_inst_transform;

    // Data common to all sample types.
    double                      m_probability;                  // probability of this sample to be chosen
};


//
// The light sampler collects all the light-emitting entities (non-physical lights, mesh lights)
// and allows to sample them.
//

class LightSampler
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit LightSampler(
        const Scene&                    scene);

    // Return the number of non-physical lights in the scene.
    size_t get_non_physical_light_count() const;

    // Return the number of emitting triangles in the scene.
    size_t get_emitting_triangle_count() const;

    // Return true if the scene contains at least one light or emitting triangle.
    bool has_lights_or_emitting_triangles() const;

    // Sample the set of non-physical lights.
    void sample_non_physical_lights(
        const size_t                    light_index,
        const double                    time,
        const foundation::Vector2d&     s,
        LightSample&                    sample) const;

    // Sample the set of emitting triangles.
    void sample_emitting_triangles(
        const double                    time,
        const foundation::Vector3d&     s,
        LightSample&                    sample) const;

    // Sample the sets of non-physical lights and emitting triangles.
    void sample(
        const double                    time,
        const foundation::Vector3d&     s,
        LightSample&                    sample) const;

    // Compute the probability density in area measure of a given light sample.
    double evaluate_pdf(const ShadingPoint& result) const;

  private:
    typedef std::vector<NonPhysicalLightInfo> NonPhysicalLightVector;
    typedef std::vector<EmittingTriangle> EmittingTriangleVector;
    typedef foundation::CDF<size_t, double> EmitterCDF;

    NonPhysicalLightVector              m_non_physical_lights;
    size_t                              m_non_physical_light_count;

    EmittingTriangleVector              m_emitting_triangles;
    double                              m_total_emissive_area;
    double                              m_rcp_total_emissive_area;
    double                              m_rcp_emitting_triangle_count;

    EmitterCDF                          m_emitter_cdf;
    EmitterCDF                          m_emitting_triangle_cdf;

    // Collect non-physical lights from a given assembly instance.
    void collect_non_physical_lights(
        const Assembly&                 assembly,
        const AssemblyInstance&         assembly_instance);

    // Collect emitting triangles from a given assembly instance.
    void collect_emitting_triangles(
        const Assembly&                 assembly,
        const AssemblyInstance&         assembly_instance);

    // Sample a given non-physical light.
    void sample_non_physical_light(
        const double                    time,
        const foundation::Vector2d&     s,
        const size_t                    light_index,
        const double                    light_prob,
        LightSample&                    sample) const;

    // Sample a given emitting triangle.
    void sample_emitting_triangle(
        const double                    time,
        const foundation::Vector2d&     s,
        const size_t                    triangle_index,
        const double                    triangle_prob,
        LightSample&                    sample) const;
};


//
// LightSampler class implementation.
//

inline size_t LightSampler::get_non_physical_light_count() const
{
    return m_non_physical_light_count;
}

inline size_t LightSampler::get_emitting_triangle_count() const
{
    return m_emitting_triangles.size();
}

inline bool LightSampler::has_lights_or_emitting_triangles() const
{
    return m_emitter_cdf.valid();
}

inline double LightSampler::evaluate_pdf(const ShadingPoint& result) const
{
    const foundation::Vector3d& v0 = result.get_vertex(0);
    const foundation::Vector3d& v1 = result.get_vertex(1);
    const foundation::Vector3d& v2 = result.get_vertex(2);
    const foundation::Vector3d n = foundation::cross(v1 - v0, v2 - v0);
    const double pdf_point = 2.0 / foundation::norm(n);
    return pdf_point * m_rcp_emitting_triangle_count;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H
