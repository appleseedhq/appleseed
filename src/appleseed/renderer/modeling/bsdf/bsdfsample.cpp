
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "bsdfsample.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"

// Standard headers.
#include <cmath>

using namespace foundation;

namespace renderer
{

namespace
{
    void compute_normal_derivatives(
        const BSDF::LocalGeometry&  local_geometry,
        const Vector3f&             o,
        const Vector3f&             dodx,
        const Vector3f&             dody,
        foundation::Vector3f&       dndx,
        foundation::Vector3f&       dndy,
        float&                      ddndx,
        float&                      ddndy)
    {
        //
        // Reference:
        //
        //   Physically Based Rendering, first edition, page 513.
        //

        const Vector3f dndu(local_geometry.m_shading_point->get_dndu(0));
        const Vector3f dndv(local_geometry.m_shading_point->get_dndv(0));
        const Vector2f duvdx(local_geometry.m_shading_point->get_duvdx(0));
        const Vector2f duvdy(local_geometry.m_shading_point->get_duvdy(0));

        dndx = dndu * duvdx[0] + dndv * duvdx[1];
        dndy = dndu * duvdy[0] + dndv * duvdy[1];

        const Vector3f& n = local_geometry.m_shading_basis.get_normal();

        ddndx = dot(dodx, n) + dot(o, dndx);
        ddndy = dot(dody, n) + dot(o, dndy);
    }
}


//
// BSDFSample class implementation.
//

void BSDFSample::compute_specular_reflected_differentials(
    const BSDF::LocalGeometry&  local_geometry,
    const Dual3f&               outgoing)
{
    if (outgoing.has_derivatives())
    {
        //
        // Reference:
        //
        //   Physically Based Rendering, first edition, page 513.
        //

        const Vector3f dodx = -(outgoing.get_dx() - outgoing.get_value());
        const Vector3f dody = -(outgoing.get_dy() - outgoing.get_value());

        Vector3f dndx, dndy;
        float ddndx, ddndy;
        compute_normal_derivatives(
            local_geometry,
            outgoing.get_value(),
            dodx,
            dody,
            dndx,
            dndy,
            ddndx,
            ddndy);

        const Vector3f& n = local_geometry.m_shading_basis.get_normal();
        const float dot_on = dot(outgoing.get_value(), n);

        const Vector3f& i = m_incoming.get_value();
        m_incoming.set_derivatives(
            i - dodx + 2.0f * Vector3f(dot_on * dndx + ddndx * n),
            i - dody + 2.0f * Vector3f(dot_on * dndy + ddndy * n));
    }
}

void BSDFSample::compute_specular_transmitted_differentials(
    const BSDF::LocalGeometry&  local_geometry,
    const float                 eta,
    const bool                  is_entering,
    const Dual3f&               outgoing)
{
    if (outgoing.has_derivatives())
    {
        const Vector3f dodx = -(outgoing.get_dx() - outgoing.get_value());
        const Vector3f dody = -(outgoing.get_dy() - outgoing.get_value());

        Vector3f dndx, dndy;
        float ddndx, ddndy;
        compute_normal_derivatives(
            local_geometry,
            outgoing.get_value(),
            dodx,
            dody,
            dndx,
            dndy,
            ddndx,
            ddndy);

        const Vector3f& n = local_geometry.m_shading_basis.get_normal();
        const float dot_on = dot(-outgoing.get_value(), n);
        const float dot_in = std::abs(dot(m_incoming.get_value(), n));
        const float mu = eta * dot_on - dot_in;

        const float a = eta - (square(eta) * dot_on) / dot_in;
        const float dmudx = a * ddndx;
        const float dmudy = a * ddndy;

        const Vector3f& i = m_incoming.get_value();
        m_incoming.set_derivatives(
            i - eta * dodx - Vector3f(mu * dndx + dmudx * n),
            i - eta * dody - Vector3f(mu * dndy + dmudy * n));
    }
}

void BSDFSample::compute_glossy_reflected_differentials(
    const BSDF::LocalGeometry&  local_geometry,
    const float                 roughness,
    const Dual3f&               outgoing)
{
    // TODO: scale differentials based on roughness.
    compute_specular_reflected_differentials(local_geometry, outgoing);
}

void BSDFSample::compute_glossy_transmitted_differentials(
    const BSDF::LocalGeometry&  local_geometry,
    const float                 eta,
    const float                 roughness,
    const bool                  is_entering,
    const Dual3f&               outgoing)
{
    // TODO: scale differentials based on roughness.
    compute_specular_transmitted_differentials(
        local_geometry,
        eta,
        is_entering,
        outgoing);
}

void BSDFSample::compute_diffuse_differentials(const Dual3f& outgoing)
{
    if (outgoing.has_derivatives())
    {
        // 1/25 of the hemisphere.
        // Reference: https://www.pbrt.org/texcache.pdf
        const auto& i = m_incoming.get_value();
        const Basis3f basis(m_incoming.get_value());
        m_incoming.set_derivatives(
            normalize(i + 0.2f * basis.get_tangent_u()),
            normalize(i + 0.2f * basis.get_tangent_v()));
    }
}

}   // namespace renderer
