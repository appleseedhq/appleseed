
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
// Light sample.
//

struct LightSample
{
    InputParams             m_input_params;                 // parameters for input evaluation
    foundation::Vector3d    m_outgoing;                     // world space outgoing direction, unit-length
    double                  m_square_distance;              // square distance from light sample to point
    const EDF*              m_edf;
    double                  m_probability;                  // probability of this sample to be chosen
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

    // Sample the set of emitters.
    void sample(
        const SamplingContext&          sampling_context,
        const foundation::Vector3d&     point,              // world space point where lighting is to be computed
        const foundation::Vector3d&     normal,             // world space normal at that point, unit-length
        const size_t                    sample_count,
        LightSampleVector&              samples) const;

    // Compute the probability density in area measure of a given light sample.
    double evaluate_pdf(const ShadingPoint& result) const;

  private:
    struct EmittingTriangle
    {
        foundation::Vector3d    m_v0, m_v1, m_v2;           // world space vertices of the triangle
        foundation::Vector3d    m_n0, m_n1, m_n2;           // world space vertex normals
        foundation::Vector3d    m_geometric_normal;         // world space geometric normal, unit-length
        double                  m_rcp_area;                 // world space triangle area reciprocal
        const EDF*              m_edf;

        // Constructor.
        EmittingTriangle(
            const foundation::Vector3d&     v0,
            const foundation::Vector3d&     v1,
            const foundation::Vector3d&     v2,
            const foundation::Vector3d&     n0,
            const foundation::Vector3d&     n1,
            const foundation::Vector3d&     n2,
            const foundation::Vector3d&     geometric_normal,
            const double                    rcp_area,
            const EDF*                      edf);
    };

    typedef std::vector<const Light*> LightVector;
    typedef std::vector<EmittingTriangle> EmittingTriangleVector;
    typedef foundation::CDF<size_t, double> LightCDF;

    LightVector                 m_lights;

    EmittingTriangleVector      m_emitting_triangles;
    double                      m_total_emissive_area;
    double                      m_rcp_total_emissive_area;

    LightCDF                    m_light_cdf;

    // Collect lights from a given assembly instance.
    void collect_lights(
        const AssemblyInstance&         assembly_instance,
        const Assembly&                 assembly);

    // Collect emitting triangles from a given assembly instance.
    void collect_emitting_triangles(
        const AssemblyInstance&         assembly_instance,
        const Assembly&                 assembly);

    // Generate a sample for a given light.
    void sample_light(
        const SamplingContext&          sampling_context,
        const size_t                    light_index,
        const double                    light_prob,
        LightSample&                    sample) const;

    // Generate a sample on the surface of a given emitting triangle.
    void sample_emitting_triangle(
        const SamplingContext&          sampling_context,
        const foundation::Vector3d&     point,
        const size_t                    triangle_index,
        const double                    triangle_prob,
        LightSample&                    sample) const;
};


//
// LightSampler class implementation.
//

// Compute the probability density in area measure of a given light sample.
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

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H
