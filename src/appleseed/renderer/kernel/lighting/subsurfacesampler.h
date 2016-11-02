
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_SUBSURFACESAMPLER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_SUBSURFACESAMPLER_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/basis.h"
#include "foundation/math/mis.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace renderer
{

//
// Subsurface sampler.
//

#define SUBSURFACESAMPLER_BASIS_ROTATION

class SubsurfaceSampler
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit SubsurfaceSampler(
        const ShadingContext&       shading_context);

    template <typename Visitor>
    void sample(
        SamplingContext&            sampling_context,
        const ShadingPoint&         outgoing_point,
        const BSSRDF&               bssrdf,
        const void*                 bssrdf_data,
        Visitor&                    visitor) const;

  private:
    enum Axis { NAxis, UAxis, VAxis };

    const ShadingContext& m_shading_context;

    static void pick_sampling_basis(
        const foundation::Basis3d&  shading_basis,
        const foundation::Vector2d& s,
        Axis&                       axis,
        foundation::Basis3d&        basis,
        float&                      basis_pdf);

    static float compute_mis_weight(
        const BSSRDF&               bssrdf,
        const void*                 data,
        const size_t                channel,
        const foundation::Basis3d&  basis,
        const Axis                  axis,
        const float                 sample_pdf,
        const foundation::Vector3d& outgoing_point,
        const foundation::Vector3d& incoming_point,
        const foundation::Vector3d& incoming_normal);
};


//
// SubsurfaceSampler class implementation.
//

inline SubsurfaceSampler::SubsurfaceSampler(
    const ShadingContext&           shading_context)
  : m_shading_context(shading_context)
{
}

template <typename Visitor>
void SubsurfaceSampler::sample(
    SamplingContext&                sampling_context,
    const ShadingPoint&             outgoing_point,
    const BSSRDF&                   bssrdf,
    const void*                     bssrdf_data,
    Visitor&                        visitor) const
{
    // Sample the BSSRDF.
    BSSRDFSample bssrdf_sample;
    if (!bssrdf.sample(sampling_context, bssrdf_data, bssrdf_sample))
        return;

    // Reject points too far away.
    // This introduces negligible bias in comparison to the other approximations.
    const foundation::Vector2f& point = bssrdf_sample.m_point;
    const float radius2 = foundation::square_norm(point);
    const float rmax2 = bssrdf_sample.m_rmax2;
    if (radius2 > rmax2)
        return;

    // Apply OSL bump / normal mapping.
    if (bssrdf_sample.m_shading_basis)
        outgoing_point.set_shading_basis(foundation::Basis3d(*bssrdf_sample.m_shading_basis));

    // Evaluate the PDF of the BSSRDF sample.
    // todo: integrate into BSSRDF sampling.
    const float radius = std::sqrt(radius2);
    const float bssrdf_sample_pdf =
        bssrdf.evaluate_pdf(bssrdf_data, bssrdf_sample.m_channel, radius);
    if (bssrdf_sample_pdf == 0.0f)
        return;

    // Pick a sampling basis.
    sampling_context.split_in_place(2, 1);
    Axis sampling_axis;
    foundation::Basis3d sampling_basis;
    float sampling_basis_pdf;
    pick_sampling_basis(
        outgoing_point.get_shading_basis(),
        sampling_context.next2<foundation::Vector2d>(),
        sampling_axis,
        sampling_basis,
        sampling_basis_pdf);

    // Compute height of sample point on (positive) hemisphere of radius Rmax.
    assert(rmax2 >= radius2);
    const double h = std::sqrt(rmax2 - radius2);

    // Compute sphere entry and exit points.
    foundation::Vector3d entry_point, exit_point;
    entry_point = exit_point = outgoing_point.get_point();
    entry_point += sampling_basis.transform_to_parent(foundation::Vector3d(point[0], +h, point[1]));
    exit_point += sampling_basis.transform_to_parent(foundation::Vector3d(point[0], -h, point[1]));
    assert(foundation::feq(foundation::norm(exit_point - entry_point), 2.0 * h, 1.0e-6));

    // Build a probe ray inscribed inside the sphere of radius Rmax.
    ShadingRay probe_ray(
        entry_point,
        -sampling_basis.get_normal(),
        0.0,
        2.0 * h,
        outgoing_point.get_time(),
        VisibilityFlags::ProbeRay,
        outgoing_point.get_ray().m_depth + 1);

    const Material* outgoing_material = outgoing_point.get_material();
    ShadingPoint shading_points[2];
    size_t shading_point_index = 0;
    ShadingPoint* parent_shading_point = 0;

    // Trace the ray and visit all intersections found inside the sphere.
    while (true)
    {
        // Continue tracing the ray.
        ShadingPoint& incoming_point = shading_points[shading_point_index];
        incoming_point.clear();
        if (!m_shading_context.get_intersector().trace(
                probe_ray,
                incoming_point,
                parent_shading_point))
            break;

        // Only consider hit points with the same material as the outgoing point.
        if (incoming_point.get_material() == outgoing_material ||
            incoming_point.get_opposite_material() == outgoing_material)
        {
            const float dot_nn =
                static_cast<float>(
                    std::abs(foundation::dot(
                        sampling_basis.get_normal(),
                        incoming_point.get_shading_normal())));
            if (dot_nn > 1.0e-6f)
            {
                // Compute sample probability.
                float probability = sampling_basis_pdf * bssrdf_sample_pdf * dot_nn;

                // Weight sample contribution using multiple importance sampling.
                probability /=
                    compute_mis_weight(
                        bssrdf,
                        bssrdf_data,
                        bssrdf_sample.m_channel,
                        sampling_basis,
                        sampling_axis,
                        probability,
                        outgoing_point.get_point(),
                        incoming_point.get_point(),
                        incoming_point.get_shading_normal());

                // Pass incoming point to visitor.
                if (!visitor.visit(bssrdf_sample, incoming_point, probability))
                    break;
            }
            else
            {
                // The ray is tangent to the surface. It will be difficult to move away
                // from that intersection point without risking looping forever. Let's
                // simply ignore this case.
                break;
            }
        }

        // Move the ray's origin past the hit surface.
        probe_ray.m_org = incoming_point.get_point();
        probe_ray.m_tmax = foundation::norm(exit_point - probe_ray.m_org);

        // Swap the current and parent shading points.
        parent_shading_point = &incoming_point;
        shading_point_index = 1 - shading_point_index;
    }
}

inline void SubsurfaceSampler::pick_sampling_basis(
    const foundation::Basis3d&      shading_basis,
    const foundation::Vector2d&     s,
    Axis&                           axis,
    foundation::Basis3d&            basis,
    float&                          basis_pdf)
{
#ifdef SUBSURFACESAMPLER_BASIS_ROTATION
    const foundation::Vector3d& n = shading_basis.get_normal();
    const foundation::Quaterniond q =
        foundation::Quaterniond::make_rotation(n, s[0] * foundation::Pi<double>());         // todo: would [0, Pi/2] rotations be enough?
    const foundation::Vector3d u = foundation::rotate(q, shading_basis.get_tangent_u());
    const foundation::Vector3d v = foundation::rotate(q, shading_basis.get_tangent_v());
#else
    const foundation::Vector3d& n = shading_basis.get_normal();
    const foundation::Vector3d& u = shading_basis.get_tangent_u();
    const foundation::Vector3d& v = shading_basis.get_tangent_v();
#endif

    if (s[1] < 0.5)
    {
        // Project the sample along N.
        axis = NAxis;
        basis = foundation::Basis3d(n, u, v);
        basis_pdf = 0.5f;
    }
    else if (s[1] < 0.75)
    {
        // Project the sample along U.
        axis = UAxis;
        basis = foundation::Basis3d(u, v, n);
        basis_pdf = 0.25f;
    }
    else
    {
        // Project the sample along V.
        axis = VAxis;
        basis = foundation::Basis3d(v, n, u);
        basis_pdf = 0.25f;
    }
}

inline float SubsurfaceSampler::compute_mis_weight(
    const BSSRDF&                   bssrdf,
    const void*                     data,
    const size_t                    channel,
    const foundation::Basis3d&      basis,
    const Axis                      axis,
    const float                     sample_pdf,
    const foundation::Vector3d&     outgoing_point,
    const foundation::Vector3d&     incoming_point,
    const foundation::Vector3d&     incoming_normal)
{
    const foundation::Vector3d d = incoming_point - outgoing_point;
    const float du = static_cast<float>(foundation::norm(foundation::project(d, basis.get_tangent_u())));
    const float dv = static_cast<float>(foundation::norm(foundation::project(d, basis.get_tangent_v())));
    const float dot_un = static_cast<float>(std::abs(foundation::dot(basis.get_tangent_u(), incoming_normal)));
    const float dot_vn = static_cast<float>(std::abs(foundation::dot(basis.get_tangent_v(), incoming_normal)));
    const float pdf_u = bssrdf.evaluate_pdf(data, channel, du) * dot_un;
    const float pdf_v = bssrdf.evaluate_pdf(data, channel, dv) * dot_vn;

    switch (axis)
    {
      case NAxis:
      {
          // We chose N: the original U is at U and the original V is at V.
          return foundation::mis_power2(sample_pdf, 0.25f * pdf_u, 0.25f * pdf_v);
      }

      case UAxis:
      {
          // We chose U: the original V is at U and the original N is at V.
          return foundation::mis_power2(sample_pdf, 0.25f * pdf_u, 0.5f * pdf_v);
      }

      case VAxis:
      {
          // We chose V: the original N is at U and the original U is at V.
          return foundation::mis_power2(sample_pdf, 0.5f * pdf_u, 0.25f * pdf_v);
      }
    }

    APPLESEED_UNREACHABLE;
    return -1.0f;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SUBSURFACESAMPLER_H
