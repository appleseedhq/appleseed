
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "separablebssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/cdf.h"
#include "foundation/math/dual.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/mis.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>

// Forward declarations.
namespace renderer  { class Material; }

using namespace foundation;

namespace renderer
{

//
// SeparableBSSRDF class implementation.
//

namespace
{
    //
    // References:
    //
    //   BSSRDF Importance Sampling
    //   http://library.imageworks.com/pdfs/imageworks-library-BSSRDF-sampling.pdf
    //   https://www.solidangle.com/research/s2013_bssrdf_slides.pdf
    //

    enum Axis { NAxis, UAxis, VAxis };

    const float ProbNAxis = 0.50f;
    const float ProbUAxis = 0.25f;
    const float ProbVAxis = 0.25f;

    void pick_projection_axis(
        const Basis3d&          shading_basis,
        const float             s,
        Axis&                   axis,
        float&                  axis_prob,
        Basis3d&                basis)
    {
        const Vector3d& n = shading_basis.get_normal();
        const Vector3d& u = shading_basis.get_tangent_u();
        const Vector3d& v = shading_basis.get_tangent_v();

        if (s < ProbNAxis)
        {
            // Project the sample along N.
            axis = NAxis;
            axis_prob = ProbNAxis;
            basis = Basis3d(n, u, v);
        }
        else if (s < ProbNAxis + ProbUAxis)
        {
            // Project the sample along U.
            axis = UAxis;
            axis_prob = ProbUAxis;
            basis = Basis3d(u, v, n);
        }
        else
        {
            // Project the sample along V.
            axis = VAxis;
            axis_prob = ProbVAxis;
            basis = Basis3d(v, n, u);
        }
    }

    float compute_mis_weight(
        const SeparableBSSRDF&  bssrdf,
        const void*             bssrdf_data,
        const Basis3d&          basis,
        const Axis              axis,
        const float             sample_pdf,
        const Vector3d&         outgoing_point,
        const Vector3d&         incoming_point,
        const Vector3d&         incoming_normal)
    {
        const Vector3d d = incoming_point - outgoing_point;
        const float du = static_cast<float>(norm(project(d, basis.get_tangent_u())));
        const float dv = static_cast<float>(norm(project(d, basis.get_tangent_v())));
        const float dot_un = static_cast<float>(std::abs(dot(basis.get_tangent_u(), incoming_normal)));
        const float dot_vn = static_cast<float>(std::abs(dot(basis.get_tangent_v(), incoming_normal)));
        const float pdf_u = bssrdf.evaluate_profile_pdf(bssrdf_data, du) * dot_un;
        const float pdf_v = bssrdf.evaluate_profile_pdf(bssrdf_data, dv) * dot_vn;

        switch (axis)
        {
          case NAxis:
          {
              // We chose N: the original U is at U and the original V is at V.
              return mis_power2(sample_pdf, ProbUAxis * pdf_u, ProbVAxis * pdf_v);
          }

          case UAxis:
          {
              // We chose U: the original V is at U and the original N is at V.
              return mis_power2(sample_pdf, ProbVAxis * pdf_u, ProbNAxis * pdf_v);
          }

          case VAxis:
          {
              // We chose V: the original N is at U and the original U is at V.
              return mis_power2(sample_pdf, ProbNAxis * pdf_u, ProbUAxis * pdf_v);
          }
        }

        APPLESEED_UNREACHABLE;
        return -1.0f;
    }

    bool find_incoming_point(
        const ShadingContext&   shading_context,
        SamplingContext&        sampling_context,
        const ShadingPoint&     outgoing_point,
        const SeparableBSSRDF&  bssrdf,
        const void*             bssrdf_data,
        const float             max_disk_radius,
        const size_t            channel,
        ShadingPoint&           incoming_point,
        float&                  incoming_point_prob)
    {
        sampling_context.split_in_place(3, 1);
        const Vector3f u = sampling_context.next2<Vector3f>();

        // Sample a radius.
        const float disk_radius = bssrdf.sample_profile(bssrdf_data, channel, u[0]);

        // Reject the limit case where there is no subsurface travel.
        if (disk_radius == 0.0f)
            return false;

        // Reject points outside the sampling disk.
        // This introduces negligible bias in comparison to the other approximations.
        if (disk_radius > max_disk_radius)
            return false;

        // Compute the position of the point on the disk.
        const float phi = TwoPi<float>() * u[1];
        const Vector2f disk_point(disk_radius * std::cos(phi), disk_radius * std::sin(phi));
        const float disk_point_prob = bssrdf.evaluate_profile_pdf(bssrdf_data, disk_radius);
        assert(disk_point_prob > 0.0f);

        // Choose a projection axis.
        Axis projection_axis;
        float projection_axis_prob;
        Basis3d projection_basis;
        pick_projection_axis(
            outgoing_point.get_shading_basis(),
            u[2],
            projection_axis,
            projection_axis_prob,
            projection_basis);

        // Compute the height of the point on the hemisphere above the sampling disk.
        assert(disk_radius <= max_disk_radius);
        const float h = std::sqrt(square(max_disk_radius) - square(disk_radius));
        const Vector3d hn = static_cast<double>(h) * projection_basis.get_normal();

        // Compute sphere entry and exit points.
        Vector3d entry_point, exit_point;
        entry_point = outgoing_point.get_point();
        entry_point += static_cast<double>(disk_point[0]) * projection_basis.get_tangent_u();
        entry_point += static_cast<double>(disk_point[1]) * projection_basis.get_tangent_v();
        exit_point = entry_point;
        entry_point += hn;
        exit_point -= hn;
        assert(feq(norm(exit_point - entry_point), 2.0 * h, 1.0e-6));

        // Build a probe ray inscribed inside the sphere around the sampling disk.
        ShadingRay probe_ray(
            entry_point,
            -projection_basis.get_normal(),
            0.0,
            2.0 * h,
            outgoing_point.get_time(),
            VisibilityFlags::ProbeRay,
            outgoing_point.get_ray().m_depth + 1);

        const ObjectInstance& outgoing_object_instance = outgoing_point.get_object_instance();
        const Material* outgoing_material = outgoing_point.get_material();
        assert(outgoing_material != 0);

        const size_t MaxIntersectionCount = 1000;
        const size_t MaxCandidateCount = 16;
        ShadingPoint shading_points[MaxCandidateCount];
        size_t sample_count = 0;

        // Trace the ray and store all intersections found inside the sphere.
        for (size_t i = 0; sample_count < MaxCandidateCount && i < MaxIntersectionCount; ++i)
        {
            // Continue tracing the ray.
            ShadingPoint& incoming_point = shading_points[sample_count];
            if (!shading_context.get_intersector().trace(probe_ray, incoming_point))
                break;

            // Move the ray's origin past the hit surface.
            probe_ray.m_org = incoming_point.get_point();
            probe_ray.m_tmin = 1.0e-6;
            probe_ray.m_tmax = norm(exit_point - probe_ray.m_org);

            //
            // Only consider hit points with the same material as the outgoing point and belonging
            // to the same SSS set.
            //
            // Also check whether the chosen axis at the outcoming point and the surface normal at
            // the incoming point are not orthogonal. Excluding such cases makes the calculation
            // of sample contributions more robust.
            //

            const Material* incoming_material = incoming_point.get_material();
            const Material* incoming_opposite_material = incoming_point.get_opposite_material();

            const bool same_material =
                incoming_material == outgoing_material ||
                incoming_opposite_material == outgoing_material;

            const bool same_sss_set =
                incoming_point.get_object_instance().is_in_same_sss_set(outgoing_object_instance);

            const float dot_nn =
                static_cast<float>(
                    std::abs(dot(projection_basis.get_normal(), incoming_point.get_shading_normal())));

            if (same_material && same_sss_set && dot_nn > 1.0e-6f)
            {
                // Make sure the incoming point is on the front side of the surface.
                // There is no such thing as subsurface scattering seen "from the inside".
                if (incoming_point.get_side() == ObjectInstance::BackSide)
                    incoming_point.flip_side();

                // Update the number of found incoming points. This invalidates 'incoming_point'.
                ++sample_count;
            }
            else
            {
                // Reset the shading point before it gets reused in the next iteration.
                incoming_point.clear();
            }
        }

        // Bail out if no incoming point could be found.
        if (sample_count == 0)
            return false;

        if (sample_count > 1)
        {
            // Found multiple incoming points: choose one.
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();
            const size_t i = truncate<size_t>(s * sample_count);
            incoming_point = shading_points[i];
        }
        else
        {
            // Found a single incomint point.
            incoming_point = shading_points[0];
        }

        // Compute the PDF of this incoming point.
        const float dot_nn =
            static_cast<float>(
                std::abs(dot(projection_basis.get_normal(), incoming_point.get_shading_normal())));
        incoming_point_prob = projection_axis_prob * disk_point_prob * dot_nn;

        // Weight the sample contribution with multiple importance sampling.
        const float mis_weight =
            compute_mis_weight(
                bssrdf,
                bssrdf_data,
                projection_basis,
                projection_axis,
                incoming_point_prob,
                outgoing_point.get_point(),
                incoming_point.get_point(),
                incoming_point.get_shading_normal());

        // Multiplying the contribution by mis_weight is equivalent to dividing the probability by it.
        incoming_point_prob /= mis_weight;

        // Account for the probability of choosing this incoming point among those found.
        incoming_point_prob /= sample_count;

        return true;
    }
}

SeparableBSSRDF::SeparableBSSRDF(
    const char*             name,
    const ParamArray&       params)
  : BSSRDF(name, params)
{
    const std::string brdf_name = std::string(name) + "_brdf";
    m_brdf = LambertianBRDFFactory().create(brdf_name.c_str(), ParamArray()).release();
    m_brdf_data.m_reflectance.set(1.0f);
    m_brdf_data.m_reflectance_multiplier = 1.0f;
}

SeparableBSSRDF::~SeparableBSSRDF()
{
    delete m_brdf;
}

bool SeparableBSSRDF::do_sample(
    const ShadingContext&   shading_context,
    SamplingContext&        sampling_context,
    const void*             data,
    const InputValues&      values,
    const ShadingPoint&     outgoing_point,
    const Vector3f&         outgoing_dir,
    const int               modes,
    BSSRDFSample&           bssrdf_sample,
    BSDFSample&             bsdf_sample) const
{
    if (values.m_weight == 0.0f)
        return false;

    if (!ScatteringMode::has_diffuse(modes))
        return false;

    // Choose a channel.
    sampling_context.split_in_place(1, 1);
    const size_t channel =
        sample_cdf(
            &values.m_channel_cdf[0],
            &values.m_channel_cdf[0] + values.m_channel_cdf.size(),
            sampling_context.next2<float>());

    // Find an incoming point.
    if (!find_incoming_point(
            shading_context,
            sampling_context,
            outgoing_point,
            *this,
            data,
            values.m_max_disk_radius,
            channel,
            bssrdf_sample.m_incoming_point,
            bssrdf_sample.m_probability))
        return false;

    // Set the BSDF at the incoming point.
    bssrdf_sample.m_brdf = m_brdf;
    bssrdf_sample.m_brdf_data = &m_brdf_data;

    // Sample the BSDF at the incoming point.
    BSDF::LocalGeometry local_geometry;
    local_geometry.m_shading_point = &bssrdf_sample.m_incoming_point;
    local_geometry.m_geometric_normal = Vector3f(bssrdf_sample.m_incoming_point.get_geometric_normal());
    local_geometry.m_shading_basis = Basis3f(bssrdf_sample.m_incoming_point.get_shading_basis());
    bssrdf_sample.m_brdf->sample(
        sampling_context,
        bssrdf_sample.m_brdf_data,
        false,
        true,
        local_geometry,
        Dual3f(outgoing_dir),   // chosen arbitrarily (no outgoing direction at the incoming point)
        ScatteringMode::All,
        bsdf_sample);

    // Evaluate the BSSRDF.
    evaluate(
        data,
        outgoing_point,
        outgoing_dir,
        bssrdf_sample.m_incoming_point,
        bsdf_sample.m_incoming.get_value(),
        modes,
        bssrdf_sample.m_value);

    return true;
}

void SeparableBSSRDF::do_evaluate(
    const void*             data,
    const InputValues&      values,
    const ShadingPoint&     outgoing_point,
    const Vector3f&         outgoing_dir,
    const ShadingPoint&     incoming_point,
    const Vector3f&         incoming_dir,
    const int               modes,
    Spectrum&               value) const
{
    //
    // The diffusion term of a separable BSSRDF is given in [1] equation 1:
    //
    //   Sd(xi, wi, xo, wo) = 1 / (Pi * C) * Ft(eta, wi) * Rd(||xi - xo||) * Ft(eta, wo)
    //
    // where C is a normalization factor equal to
    //
    //   C = 1 - 2 * C1(1/eta)
    //
    // and C1 is the first moment of the Fresnel equation.
    //
    // Note that [2] equation 5 is missing the normalization factor.
    //
    // Rd(r) is the spatially resolved diffuse reflectance for a normally
    // incident beam of light on a planar semi-infinite medium ([3] equation 27):
    //
    //            /
    //            |
    //   Rd(x0) = |  Sd(xi, wi, xo, wo) (wo . no) dwo
    //            |
    //            / 2 Pi
    //
    // If Sd does not depend on wo, then Rd only depends on ||xi - xo|| and we get:
    //
    //   Rd = Pi Sd    <=>    Sd = 1/Pi Rd
    //
    // A note regarding the computation of the Fresnel factors:
    //
    //   Since the direction in the outside medium--be it the outgoing or the incoming
    //   direction--is always fixed, we always need to figure out a refracted direction
    //   in the inside medium, and thus we compute both Fresnel factors at the
    //   incoming and outgoing points using eta (defined as outside IOR / inside IOR).
    //
    // [1] A Better Dipole
    //     http://www.eugenedeon.com/wp-content/uploads/2014/04/betterdipole.pdf
    //
    // [2] A Practical Model for Subsurface Light Transport
    //     https://graphics.stanford.edu/papers/bssrdf/bssrdf.pdf
    //
    // [3] Directional Dipole Model for Subsurface Scattering
    //     Jeppe Revall Frisvad, Toshiya Hachisuka, Thomas Kim Kjeldsen
    //     http://www.ci.i.u-tokyo.ac.jp/~hachisuka/dirpole.pdf
    //

    if (!ScatteringMode::has_diffuse(modes))
    {
        value.set(0.0f);
        return;
    }

    // The profile function evalutes to zero outside the sampling disk.
    const float square_radius =
        static_cast<float>(
            square_norm(outgoing_point.get_point() - incoming_point.get_point()));
    if (square_radius > square(values.m_max_disk_radius))
    {
        value.set(0.0f);
        return;
    }

    // Spatial term.
    evaluate_profile(
        data,
        outgoing_point,
        outgoing_dir,
        incoming_point,
        incoming_dir,
        value);

    float fo, fi;

    if (values.m_fresnel_weight == 0.0f)
        fo = fi = 1.0f;
    else
    {
        // Fresnel factor at outgoing direction.
        const Vector3f outgoing_normal(outgoing_point.get_shading_normal());
        const float cos_on = std::min(std::abs(dot(outgoing_dir, outgoing_normal)), 1.0f);
        fresnel_transmittance_dielectric(fo, values.m_eta, cos_on);
        fo = lerp(1.0f, fo, values.m_fresnel_weight);

        // Fresnel factor at incoming direction.
        const Vector3f incoming_normal(incoming_point.get_shading_normal());
        const float cos_in = std::min(std::abs(dot(incoming_dir, incoming_normal)), 1.0f);
        fresnel_transmittance_dielectric(fi, values.m_eta, cos_in);
        fi = lerp(1.0f, fi, values.m_fresnel_weight);
    }

    // Normalization constant.
    const float c = 1.0f - fresnel_first_moment_x2(values.m_eta);

    // Final value. The 1/Pi factor present in the BSSRDF expression in most
    // papers is the one from the Lambertian BRDF at the incoming point.
    value *= values.m_weight * fo * fi / c;
}

}   // namespace renderer
