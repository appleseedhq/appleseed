
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "subsurfacesampler.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/mis.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    enum Axis { NAxis, UAxis, VAxis };

    void pick_sampling_basis(
        const Basis3d&      shading_basis,
        const double        s,
        Axis&               axis,
        Basis3d&            basis,
        double&             basis_pdf)
    {
        const Vector3d& n = shading_basis.get_normal();
        const Vector3d& u = shading_basis.get_tangent_u();
        const Vector3d& v = shading_basis.get_tangent_v();

        if (s <= 0.5)
        {
            // Project the sample along N.
            axis = NAxis;
            basis = Basis3d(n, u, v);
            basis_pdf = 0.5;
        }
        else if (s <= 0.75)
        {
            // Project the sample along U.
            axis = UAxis;
            basis = Basis3d(u, v, n);
            basis_pdf = 0.25;
        }
        else
        {
            // Project the sample along V.
            axis = VAxis;
            basis = Basis3d(v, n, u);
            basis_pdf = 0.25;
        }
    }

    double compute_mis_weight(
        const BSSRDF&       bssrdf,
        const void*         data,
        const size_t        channel,
        const Basis3d&      basis,
        const Axis          axis,
        const double        sample_pdf,
        const Vector3d&     outgoing_point,
        const Vector3d&     incoming_point,
        const Vector3d&     incoming_normal)
    {
        const Vector3d& n = basis.get_normal();
        const Vector3d& u = basis.get_tangent_u();
        const Vector3d& v = basis.get_tangent_v();
        const Vector3d d = incoming_point - outgoing_point;

        // todo: not sure about the 2.0 factors.

        switch (axis)
        {
            case NAxis:
            {
                const double pdf_u = 0.25 * bssrdf.evaluate_pdf(data, channel, norm(project(d, u))) * abs(dot(u, incoming_normal));
                const double pdf_v = 0.25 * bssrdf.evaluate_pdf(data, channel, norm(project(d, v))) * abs(dot(v, incoming_normal));
                return mis_power2(2.0 * sample_pdf, pdf_u, pdf_v);
            }

            case UAxis:
            {
                const double pdf_n = 0.5  * bssrdf.evaluate_pdf(data, channel, norm(project(d, n))) * abs(dot(n, incoming_normal));
                const double pdf_v = 0.25 * bssrdf.evaluate_pdf(data, channel, norm(project(d, v))) * abs(dot(v, incoming_normal));
                return mis_power2(sample_pdf, 2.0 * pdf_n, pdf_v);
            }

            case VAxis:
            {
                const double pdf_n = 0.5  * bssrdf.evaluate_pdf(data, channel, norm(project(d, n))) * abs(dot(n, incoming_normal));
                const double pdf_u = 0.25 * bssrdf.evaluate_pdf(data, channel, norm(project(d, u))) * abs(dot(u, incoming_normal));
                return mis_power2(sample_pdf, 2.0 * pdf_n, pdf_u);
            }
        }

        assert(!"Should never happen.");
        return 0.0;
    }
}

SubsurfaceSampler::SubsurfaceSampler(
    const ShadingContext&   shading_context)
  : m_shading_context(shading_context)
{
}

size_t SubsurfaceSampler::sample(
    SamplingContext&        sampling_context,
    const ShadingPoint&     outgoing_point,
    const BSSRDF&           bssrdf,
    const void*             bssrdf_data,
    SubsurfaceSample        samples[],
    const size_t            max_sample_count)
{
    assert(max_sample_count > 0);

    // Sample the diffusion profile.
    BSSRDFSample bssrdf_sample(sampling_context);
    if (!bssrdf.sample(bssrdf_data, bssrdf_sample))
        return 0;

    // Reject points too far away.
    // This introduces negligible bias in comparison to the other approximations.
    const Vector2d& point(bssrdf_sample.get_point());
    const double radius2 = square_norm(point);
    const double rmax2 = bssrdf_sample.get_rmax2();
    if (radius2 > rmax2)
        return 0;

    // Evaluate the PDF of the diffusion profile.
    const double radius = sqrt(radius2);
    const double bssrdf_sample_pdf =
        bssrdf.evaluate_pdf(bssrdf_data, bssrdf_sample.get_channel(), radius);

    // Pick a sampling basis.
    sampling_context.split_in_place(1, 1);
    Axis sampling_axis;
    Basis3d sampling_basis;
    double sampling_basis_pdf;
    pick_sampling_basis(
        outgoing_point.get_shading_basis(),
        sampling_context.next_double2(),
        sampling_axis,
        sampling_basis,
        sampling_basis_pdf);

    // Compute height of sample point on (positive) hemisphere of radius Rmax.
    assert(rmax2 >= radius2);
    const double h = sqrt(rmax2 - radius2);

    // Compute sphere entry and exit points.
    Vector3d entry_point, exit_point;
    entry_point = exit_point = outgoing_point.get_point();
    entry_point += sampling_basis.transform_to_parent(Vector3d(point[0], +h, point[1]));
    exit_point += sampling_basis.transform_to_parent(Vector3d(point[0], -h, point[1]));
    assert(feq(norm(exit_point - entry_point), 2.0 * h, 1.0e-9));

    // Build a probe ray inscribed inside the sphere of radius Rmax.
    ShadingRay probe_ray(
        entry_point,
        -sampling_basis.get_normal(),
        0.0,
        2.0 * h,
        outgoing_point.get_time(),
        VisibilityFlags::ProbeRay,
        outgoing_point.get_ray().m_depth + 1);

    ShadingPoint shading_points[2];
    size_t shading_point_index = 0;
    ShadingPoint* parent_shading_point = 0;

    size_t sample_count = 0;

    // Trace the ray and return all intersections (or up to max_sample_count of them) found inside the sphere.
    while (true)
    {
        // Continue tracing the ray.
        shading_points[shading_point_index].clear();
        if (!m_shading_context.get_intersector().trace(
                probe_ray,
                shading_points[shading_point_index],
                parent_shading_point))
            break;

        // Only consider points lying on surfaces with the same material as the outgoing point.
        if (shading_points[shading_point_index].get_material() == outgoing_point.get_material())
        {
            SubsurfaceSample& sample = samples[sample_count++];
            sample.m_point = shading_points[shading_point_index];

            // Compute sample probability.
            sample.m_probability =
                  bssrdf_sample_pdf
                * sampling_basis_pdf
                * abs(dot(sampling_basis.get_normal(), sample.m_point.get_geometric_normal()));     // todo: or shading normal?

            // Weight sample probability with multiple importance sampling.
            sample.m_probability /=
                compute_mis_weight(
                    bssrdf,
                    bssrdf_data,
                    bssrdf_sample.get_channel(),
                    sampling_basis,
                    sampling_axis,
                    sample.m_probability,
                    outgoing_point.get_point(),
                    sample.m_point.get_point(),
                    sample.m_point.get_geometric_normal());     // todo: or shading normal?

            // Return the relative index of refraction.
            sample.m_eta = bssrdf_sample.get_eta();

            if (sample_count == max_sample_count)
                break;
        }

        // Move the ray's origin past the hit surface.
        probe_ray.m_org = shading_points[shading_point_index].get_point();
        probe_ray.m_tmax = norm(exit_point - probe_ray.m_org);

        // Swap the current and parent shading points.
        parent_shading_point = &shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;
    }

    return sample_count;
}

}   // namespace renderer
