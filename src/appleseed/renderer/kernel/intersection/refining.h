
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <cstdint>

namespace renderer
{

//
// Intersection point refining functions.
//
// Reference:
//
//   Quasi-Monte Carlo light transport simulation by efficient ray tracing
//   http://vts.uni-ulm.de/docs/2008/6265/vts_6265_8393.pdf
//

// Refine the location of a point on a surface.
template <typename IntersectionFunction>
foundation::Vector3d refine(
    const foundation::Vector3d& point,
    const foundation::Vector3d& direction,
    const IntersectionFunction& intersect);

// Compute a front and a back point for a point on a surface by offsetting the surface
// point by its normal and inversed normal using a fixed offset.
void fixed_offset(
    const foundation::Vector3d& p,
    foundation::Vector3d        n,
    foundation::Vector3d&       front,
    foundation::Vector3d&       back);

// Compute a front and a back point for a point on a surface by offsetting the surface
// point by its normal and inversed normal using an adaptive offset.
template <typename IntersectionFunction>
void adaptive_offset(
    const foundation::Vector3d& p,
    foundation::Vector3d        n,
    foundation::Vector3d&       front,
    foundation::Vector3d&       back,
    const IntersectionFunction& intersect);

// Offset a point by the smallest possible amount using its normal.
foundation::Vector3d adaptive_offset_point_step(
    const foundation::Vector3d& p,
    const foundation::Vector3d& n,
    const std::int64_t          mag);

// Offset a point away from a surface represented by its normal using an adaptive offset.
template <typename IntersectionFunction>
foundation::Vector3d adaptive_offset_point(
    const foundation::Vector3d& p,
    const foundation::Vector3d& n,
    const std::int64_t          initial_mag,
    const IntersectionFunction& intersect);


//
// Intersection point refining functions implementation.
//

template <typename IntersectionFunction>
inline foundation::Vector3d refine(
    const foundation::Vector3d& point,
    const foundation::Vector3d& direction,
    const IntersectionFunction& intersect)
{
    foundation::Vector3d result = point;

    const std::size_t RefinementSteps = 2;

    for (std::size_t i = 0; i < RefinementSteps; ++i)
    {
        const double t = intersect(result, direction);
        result += t * direction;
    }

    return result;
}

inline void fixed_offset(
    const foundation::Vector3d& p,
    foundation::Vector3d        n,
    foundation::Vector3d&       front,
    foundation::Vector3d&       back)
{
    // Offset parameters.
    const double Threshold = 1.0e-25;
    const int EpsMag = 8;
    static const int EpsLut[2] = { EpsMag, -EpsMag };

    // Check which components of p are close to the origin.
    const bool is_small[3] =
    {
        std::abs(p[0]) < Threshold,
        std::abs(p[1]) < Threshold,
        std::abs(p[2]) < Threshold
    };

    // If any of the components of p is close to the origin, we need to normalize n.
    if (is_small[0] | is_small[1] | is_small[2])
        n = foundation::normalize(n);

    // Compute the offset points.
    for (std::size_t i = 0; i < 3; ++i)
    {
        if (is_small[i])
        {
            const double shift = n[i] * Threshold;
            front[i] = p[i] + shift;
            back[i] = p[i] - shift;
        }
        else
        {
            const auto pi = foundation::binary_cast<std::uint64_t>(p[i]);
            const int shift = EpsLut[(pi ^ foundation::binary_cast<std::uint64_t>(n[i])) >> 63];
            front[i] = foundation::binary_cast<double>(pi + shift);
            back[i] = foundation::binary_cast<double>(pi - shift);
        }
    }
}

template <typename IntersectionFunction>
inline void adaptive_offset(
    const foundation::Vector3d& p,
    foundation::Vector3d        n,
    foundation::Vector3d&       front,
    foundation::Vector3d&       back,
    const IntersectionFunction& intersect)
{
    const std::int64_t InitialMag = 8;

    n = foundation::normalize(n);

    front = adaptive_offset_point(p, n, InitialMag, intersect);
    back = adaptive_offset_point(p, -n, InitialMag, intersect);
}

template <typename IntersectionFunction>
inline foundation::Vector3d adaptive_offset_point(
    const foundation::Vector3d& p,
    const foundation::Vector3d& n,
    const std::int64_t          initial_mag,
    const IntersectionFunction& intersect)
{
    std::int64_t mag = initial_mag;
    foundation::Vector3d result = p;

    for (std::size_t i = 0; i < 64; ++i)
    {
        result = adaptive_offset_point_step(result, n, mag);

        if (intersect(result, n) < 0.0)
            break;

        mag *= 2;
    }

    return result;
}

inline foundation::Vector3d adaptive_offset_point_step(
    const foundation::Vector3d& p,
    const foundation::Vector3d& n,
    const std::int64_t          mag)
{
    const double Threshold = 1.0e-25;
    const std::int64_t eps_lut[2] = { mag, -mag };

    foundation::Vector3d result;

    for (std::size_t i = 0; i < 3; ++i)
    {
        if (std::abs(p[i]) < Threshold)
            result[i] = p[i] + n[i] * Threshold;
        else
        {
            const auto pi = foundation::binary_cast<std::uint64_t>(p[i]);
            const std::int64_t eps_lut_index =
                (pi ^ foundation::binary_cast<std::uint64_t>(n[i])) >> 63;
            result[i] = foundation::binary_cast<double>(pi + eps_lut[eps_lut_index]);
        }
    }

    return result;
}

}   // namespace renderer
