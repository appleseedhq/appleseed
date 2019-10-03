
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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
#include "phasefunction.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

namespace foundation
{

namespace
{
    inline float henyey_pdf(const float g, const float sqr_g, const float cosine)
    {
        //
        // p(x) = 1/2 * (1 - g^2) / (1 + g^2 - 2gx)^(3/2),
        // where x is cos(phi) and g is the average cosine parameter.
        // Additionally divide by TwoPi, because we sample over the sphere.
        //

        const float numerator = (1.0f - sqr_g);
        const float denominator = std::pow(1.0f + sqr_g - 2.0f * g * cosine, -1.5f);

        return RcpFourPi<float>() * numerator * denominator;
    }
}


//
// HenyeyPhaseFunction class implementation.
//

HenyeyPhaseFunction::HenyeyPhaseFunction(const float g)
  : m_g(g)
{
}

float HenyeyPhaseFunction::evaluate(const Vector3f& outgoing, const Vector3f& incoming) const
{
    return henyey_pdf(m_g, m_g * m_g, dot(incoming, outgoing));
}

float HenyeyPhaseFunction::sample(const Vector3f& outgoing, const Vector2f& s, Vector3f& incoming) const
{
    //
    // x = 1/(2g) * (1 + g^2 - [(1 - g^2) / (1 + g*s)]^2),
    // where x is cos(phi) and s is a uniform random sample from [-1, 1).
    //

    const float sqr_g = m_g * m_g;
    const float t = 2.0f * s[0] - 1.0f;

    float cosine;
    if (std::abs(m_g) < 1.0e-5f)
    {
        cosine = t; // isotropic
    }
    else
    {
        const float p = (1.0f - sqr_g) / (1.0f + m_g * t);
        cosine = 0.5f / m_g * (1.0f + sqr_g - p * p);
    }

    const float sine = std::sqrt(std::max(1.0f - cosine * cosine, 0.0f));
    const Vector2f tangent = sample_circle_uniform(s[1]);
    const Basis3f basis(outgoing);

    incoming =
        basis.get_tangent_u() * tangent.x * sine +
        basis.get_tangent_v() * tangent.y * sine +
        basis.get_normal()    * cosine;

    assert(feq(norm(incoming), 1.0f));

    // Evaluate PDF.
    return henyey_pdf(m_g, sqr_g, cosine);
}


//
// IsotropicPhaseFunction class implementation.
//

float IsotropicPhaseFunction::evaluate(const Vector3f& outgoing, const Vector3f& incoming) const
{
    return RcpFourPi<float>();
}

float IsotropicPhaseFunction::sample(const Vector3f& outgoing, const Vector2f& s, Vector3f& incoming) const
{
    incoming = sample_sphere_uniform(s);
    return RcpFourPi<float>();
}

}   // namespace foundation
