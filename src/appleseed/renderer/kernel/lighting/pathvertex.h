
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/backwardlightsampler.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace renderer  { class BSDF; }
namespace renderer  { class BSSRDF; }
namespace renderer  { class EDF; }
namespace renderer  { class Material; }
namespace renderer  { class ShadingContext; }

namespace renderer
{

//
// A path vertex, built by the generic path tracer (renderer/kernel/lighting/pathtracer.h)
// and passed to path visitors.
//

class PathVertex
  : public foundation::NonCopyable
{
  public:
    SamplingContext&            m_sampling_context;

    // Path properties.
    size_t                      m_path_length;
    int                         m_scattering_modes;
    Spectrum                    m_throughput;   // cumulative throughput (percent of incoming light which is reflected as outgoing light) of this path, up to but excluding this vertex

    // Current vertex properties.
    const ShadingPoint*         m_shading_point;
    const ShadingPoint*         m_parent_shading_point;
    foundation::Dual3d          m_outgoing;
    double                      m_cos_on;       // cos(outgoing direction, shading normal)
    const EDF*                  m_edf;
    const BSDF*                 m_bsdf;
    const void*                 m_bsdf_data;
    const BSSRDF*               m_bssrdf;
    const void*                 m_bssrdf_data;
    const void*                 m_volume_data;
    bool                        m_crossing_interface;

    // Properties of the scattering event leading to this vertex.
    ScatteringMode::Mode        m_prev_mode;
    float                       m_prev_prob;

    // AOV properties.
    ScatteringMode::Mode        m_aov_mode;

    // Constructor.
    explicit PathVertex(SamplingContext& sampling_context);

    // Forward the most useful methods to the shading point.
    const ShadingRay& get_ray() const;
    const ShadingRay::Time& get_time() const;
    const foundation::Vector2f& get_uv(const size_t uvset) const;
    const foundation::Vector3d& get_point() const;
    const foundation::Vector3d& get_geometric_normal() const;
    const foundation::Vector3d& get_shading_normal() const;
    const foundation::Basis3d& get_shading_basis() const;
    const Material* get_material() const;

    // Compute the radiance emitted at this vertex. Only call when there is an EDF (when m_edf is set).
    void compute_emitted_radiance(
        const ShadingContext&   shading_context,
        Spectrum&               radiance) const;

    // Return the probability density wrt. surface area measure of reaching this vertex via BSDF sampling.
    float get_bsdf_prob_area() const;

    // Return the probability density wrt. surface area measure of reaching this vertex via light sampling.
    float get_light_prob_area(const BackwardLightSampler& light_sampler) const;
};


//
// PathVertex class implementation.
//

inline PathVertex::PathVertex(SamplingContext& sampling_context)
  : m_sampling_context(sampling_context)
{
}

inline const ShadingRay& PathVertex::get_ray() const
{
    return m_shading_point->get_ray();
}

inline const ShadingRay::Time& PathVertex::get_time() const
{
    return m_shading_point->get_time();
}

inline const foundation::Vector2f& PathVertex::get_uv(const size_t uvset) const
{
    return m_shading_point->get_uv(uvset);
}

inline const foundation::Vector3d& PathVertex::get_point() const
{
    return m_shading_point->get_point();
}

inline const foundation::Vector3d& PathVertex::get_geometric_normal() const
{
    return m_shading_point->get_geometric_normal();
}

inline const foundation::Vector3d& PathVertex::get_shading_normal() const
{
    return m_shading_point->get_shading_normal();
}

inline const foundation::Basis3d& PathVertex::get_shading_basis() const
{
    return m_shading_point->get_shading_basis();
}

inline const Material* PathVertex::get_material() const
{
    return m_shading_point->get_material();
}

inline float PathVertex::get_bsdf_prob_area() const
{
    // Make sure we're coming from a valid scattering event.
    assert(m_prev_mode != ScatteringMode::None);
    assert(m_prev_prob > 0.0f);

    // Veach: 8.2.2.2 eq. 8.10.
    const double d = m_shading_point->get_distance();
    const float g = static_cast<float>(m_cos_on / (d * d));
    return m_prev_prob * g;
}

inline float PathVertex::get_light_prob_area(const BackwardLightSampler& light_sampler) const
{
    return light_sampler.evaluate_pdf(*m_shading_point, *m_parent_shading_point);
}

}   // namespace renderer
