
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"

// Standard headers.
#include <algorithm>
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

        Vector3d dndx, dndy;
        double ddndx, ddndy;
        compute_normal_derivatives(dndx, dndy, ddndx, ddndy);

        const Vector3d& normal = m_shading_point.get_shading_normal();
        const double dot_on = dot(m_outgoing.get_value(), normal);

        m_incoming =
            Dual3d(
                m_incoming.get_value(),
                -m_outgoing.get_dx() + 2.0 * Vector3d(dot_on * dndx + ddndx * normal),
                -m_outgoing.get_dy() + 2.0 * Vector3d(dot_on * dndy + ddndy * normal));

        if (m_probability != BSDF::DiracDelta)
            apply_pdf_differentials_heuristic();
    }
}

void BSDFSample::compute_transmitted_differentials(const double eta)
{
    if (m_outgoing.has_derivatives())
    {
        Vector3d dndx, dndy;
        double ddndx, ddndy;
        compute_normal_derivatives(dndx, dndy, ddndx, ddndy);

        const Vector3d& normal = m_shading_point.get_shading_normal();

        const double dot_on = dot(-m_outgoing.get_value(), normal);
        const double dot_in = dot(m_incoming.get_value(), normal);
        const double mu = eta * dot_on - dot_in;

        const double a = eta - (square(eta) * dot_on) / dot_in;
        const double dmudx = a * ddndx;
        const double dmudy = a * ddndy;

        m_incoming =
            Dual3d(
                m_incoming.get_value(),
                eta * m_outgoing.get_dx() - Vector3d(mu * dndx + dmudx * normal),
                eta * m_outgoing.get_dy() - Vector3d(mu * dndy + dmudy * normal));

        if (m_probability != BSDF::DiracDelta)
            apply_pdf_differentials_heuristic();
    }
}

void BSDFSample::compute_normal_derivatives(
    Vector3d&   dndx,
    Vector3d&   dndy,
    double&     ddndx,
    double&     ddndy) const
{
    //
    // Physically Based Rendering, first edition, page 513.
    //

    const Vector3d& dndu = m_shading_point.get_dndu(0);
    const Vector3d& dndv = m_shading_point.get_dndv(0);
    const Vector2d& duvdx = m_shading_point.get_duvdx(0);
    const Vector2d& duvdy = m_shading_point.get_duvdy(0);

    dndx = dndu * duvdx[0] + dndv * duvdx[1];
    dndy = dndu * duvdy[0] + dndv * duvdy[1];

    const Vector3d& normal = m_shading_point.get_shading_normal();

    ddndx = dot(m_outgoing.get_dx(), normal) + dot(m_outgoing.get_value(), dndx);
    ddndy = dot(m_outgoing.get_dy(), normal) + dot(m_outgoing.get_value(), dndy);
}

void BSDFSample::apply_pdf_differentials_heuristic()
{
    //
    // Reference:
    //
    //   http://renderman.pixar.com/resources/current/RenderMan/integratorRef.html#about-ray-differentials-ray-spreads
    //

    assert(m_incoming.has_derivatives());
    assert(m_probability > 0.0);

    const double pdf_spread = 1.0 / (8.0 * sqrt(m_probability));

    const double rx_spread = norm(m_incoming.get_dx());
    const double ry_spread = norm(m_incoming.get_dy());

    const double sx = max(pdf_spread, rx_spread) / rx_spread;
    const double sy = max(pdf_spread, ry_spread) / ry_spread;

    m_incoming =
        Dual3d(
            m_incoming.get_value(),
            m_incoming.get_dx() * sx,
            m_incoming.get_dy() * sy);
}

}   // namespace renderer
