
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

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// BSDFSample class implementation.
//

void BSDFSample::compute_reflected_differentials()
{
    if (m_outgoing.has_derivatives())
    {
        //
        // Physically Based Rendering, first edition, page 513.
        //

        Vector3f dndx, dndy;
        float ddndx, ddndy;
        compute_normal_derivatives(dndx, dndy, ddndx, ddndy);

        const Vector3f& n = m_shading_basis.get_normal();
        const float dot_on = dot(m_outgoing.get_value(), n);

        m_incoming.set_derivatives(
            -m_outgoing.get_dx() + 2.0f * Vector3f(dot_on * dndx + ddndx * n),
            -m_outgoing.get_dy() + 2.0f * Vector3f(dot_on * dndy + ddndy * n));

        if (m_probability != BSDF::DiracDelta)
            apply_pdf_differentials_heuristic();
    }
}

void BSDFSample::compute_transmitted_differentials(const float eta)
{
    if (m_outgoing.has_derivatives())
    {
        Vector3f dndx, dndy;
        float ddndx, ddndy;
        compute_normal_derivatives(dndx, dndy, ddndx, ddndy);

        const Vector3f& n = m_shading_basis.get_normal();
        const float dot_on = dot(-m_outgoing.get_value(), n);
        const float dot_in = dot(m_incoming.get_value(), n);
        const float mu = eta * dot_on - dot_in;

        const float a = eta - (square(eta) * dot_on) / dot_in;
        const float dmudx = a * ddndx;
        const float dmudy = a * ddndy;

        m_incoming.set_derivatives(
            eta * m_outgoing.get_dx() - Vector3f(mu * dndx + dmudx * n),
            eta * m_outgoing.get_dy() - Vector3f(mu * dndy + dmudy * n));

        if (m_probability != BSDF::DiracDelta)
            apply_pdf_differentials_heuristic();
    }
}

void BSDFSample::compute_normal_derivatives(
    Vector3f&   dndx,
    Vector3f&   dndy,
    float&      ddndx,
    float&      ddndy) const
{
    //
    // Physically Based Rendering, first edition, page 513.
    //

    const Vector3f dndu(m_shading_point->get_dndu(0));
    const Vector3f dndv(m_shading_point->get_dndv(0));

    const Vector2f duvdx(m_shading_point->get_duvdx(0));
    const Vector2f duvdy(m_shading_point->get_duvdy(0));

    dndx = dndu * duvdx[0] + dndv * duvdx[1];
    dndy = dndu * duvdy[0] + dndv * duvdy[1];

    const Vector3f& n = m_shading_basis.get_normal();

    ddndx = dot(m_outgoing.get_dx(), n) + dot(m_outgoing.get_value(), dndx);
    ddndy = dot(m_outgoing.get_dy(), n) + dot(m_outgoing.get_value(), dndy);
}

void BSDFSample::apply_pdf_differentials_heuristic()
{
    //
    // Reference:
    //
    //   https://renderman.pixar.com/resources/RenderMan_20/integratorRef.html#about-ray-differentials-ray-spreads
    //

    assert(m_incoming.has_derivatives());
    assert(m_probability > 0.0f);

    const float pdf_spread = 1.0f / (8.0f * sqrt(m_probability));

    const float rx_spread = norm(m_incoming.get_dx());
    const float ry_spread = norm(m_incoming.get_dy());

    const float sx = max(pdf_spread, rx_spread) / rx_spread;
    const float sy = max(pdf_spread, ry_spread) / ry_spread;

    m_incoming =
        Dual3f(
            m_incoming.get_value(),
            m_incoming.get_dx() * sx,
            m_incoming.get_dy() * sy);
}

}   // namespace renderer
