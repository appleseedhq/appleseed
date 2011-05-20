
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/modeling/input/inputparams.h"

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
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Light-emitting triangle.
//

struct EmittingTriangle
{
    foundation::UniqueID        m_assembly_instance_uid;
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
// Light sample.
//

struct LightSample
{
    size_t                      m_triangle_index;               // index of the emitting triangle, ~size_t(0) if none
    InputParams                 m_input_params;                 // parameters for input evaluation
    const EDF*                  m_edf;                          // EDF at the position of the light sample
    double                      m_probability;                  // probability of this sample to be chosen
};

typedef std::vector<LightSample> LightSampleVector;


//
// Light sampler.
//

class LightSampler
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit LightSampler(
        const Scene&                    scene);

    // Return true if the scene has at least one light or emitting triangle.
    bool has_lights() const;

    // Sample the set of emitters.
    // Return true if one or more samples could be taken, false otherwise.
    bool sample(
        const foundation::Vector3d&     s,
        LightSample&                    sample) const;
    bool sample(
        SamplingContext&                sampling_context,
        LightSample&                    sample) const;
    bool sample(
        SamplingContext&                sampling_context,
        const size_t                    sample_count,
        LightSampleVector&              samples) const;

    // Compute the probability density in area measure of a given light sample.
    double evaluate_pdf(const ShadingPoint& result) const;

    // Retrieve a light-emitting triangle.
    const EmittingTriangle& get_emitting_triangle(const size_t triangle_index) const;

  private:
    typedef std::vector<const Light*> LightVector;
    typedef std::vector<EmittingTriangle> EmittingTriangleVector;
    typedef foundation::CDF<size_t, double> LightCDF;

    LightVector                 m_lights;
    size_t                      m_light_count;

    EmittingTriangleVector      m_emitting_triangles;
    double                      m_total_emissive_area;
    double                      m_rcp_total_emissive_area;

    LightCDF                    m_light_cdf;

    // Collect all lights from a given scene.
    void collect_lights(const Scene& scene);

    // Collect lights from a given assembly instance.
    void collect_lights(const AssemblyInstance& assembly_instance);

    // Collect all emitting triangles from a given scene.
    void collect_emitting_triangles(const Scene& scene);

    // Collect emitting triangles from a given assembly instance.
    void collect_emitting_triangles(
        const AssemblyInstance&         assembly_instance,
        const Assembly&                 assembly);

    // Sample the set of emitters.
    void sample_emitters(
        const foundation::Vector3d&     s,
        LightSample&                    sample) const;

    // Sample a given light.
    void sample_light(
        const foundation::Vector2d&     s,
        const size_t                    light_index,
        const double                    light_prob,
        LightSample&                    sample) const;

    // Sample a given emitting triangle.
    void sample_emitting_triangle(
        const foundation::Vector2d&     s,
        const size_t                    triangle_index,
        const double                    triangle_prob,
        LightSample&                    sample) const;
};


//
// LightSampler class implementation.
//

inline bool LightSampler::has_lights() const
{
    return m_light_cdf.valid();
}

inline double LightSampler::evaluate_pdf(const ShadingPoint& /*result*/) const
{
    //
    // The probability density of a given triangle is
    //
    //                   triangle area
    //   p_triangle = -------------------
    //                total emissive area
    //
    // The probability density of a given point on a given triangle is
    //
    //                  1.0
    //   p_point = -------------
    //             triangle area
    //
    // The probability density of a given light sample is thus
    //
    //                                             1.0
    //   p_sample = p_triangle * p_point = -------------------
    //                                     total emissive area
    //

    return m_rcp_total_emissive_area;
}

inline const EmittingTriangle& LightSampler::get_emitting_triangle(const size_t triangle_index) const
{
    assert(triangle_index < m_emitting_triangles.size());
    return m_emitting_triangles[triangle_index];
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H
