
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_SAMPLING_EQUIANGULARSAMPLER_H
#define APPLESEED_FOUNDATION_MATH_SAMPLING_EQUIANGULARSAMPLER_H

// appleseed.foundation headers.
#include "foundation/math/ray.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>

template <typename SamplingContext>
class EquiangularSampler
{
  public:
    EquiangularSampler(
        const foundation::Vector3d&     center,
        const foundation::Ray3d&        volume_ray,
        SamplingContext&                sampling_context)
        : m_sampling_context(sampling_context)
    {
        // Direction from the ray origin to the light center.
        const foundation::Vector3f origin_to_light(center - volume_ray.m_org);

        // Signed distance from the ray origin
        // to the light projection onto the ray.
        m_origin_to_projection = foundation::dot(origin_to_light, foundation::Vector3f(volume_ray.m_dir));

        // Distance from the projection point to the light center (height).
        m_projection_to_light =
            std::sqrt(square_norm(origin_to_light) - foundation::square(m_origin_to_projection));

        m_ray_length = static_cast<float>(volume_ray.m_tmax);

        m_near_angle = std::atan2(-m_origin_to_projection, m_projection_to_light);

        m_far_angle =
            volume_ray.is_finite() ?
            std::atan2(
                m_ray_length - m_origin_to_projection,
                m_projection_to_light) :
            foundation::HalfPi<float>();
    }

    float sample() const
    {
        m_sampling_context.split_in_place(1, 1);

        const float distance = m_origin_to_projection +
            foundation::sample_equiangular_distribution(
                m_sampling_context.next2<float>(),
                m_near_angle,
                m_far_angle,
                m_projection_to_light);

        return foundation::clamp(distance, 0.0f, m_ray_length);
    }

    float evaluate(const float distance_sample) const
    {
        return foundation::equiangular_distribution_pdf(
            distance_sample - m_origin_to_projection,
            m_near_angle,
            m_far_angle,
            m_projection_to_light);
    }

  private:
    SamplingContext&        m_sampling_context;

    float                   m_origin_to_projection;
    float                   m_projection_to_light;
    float                   m_ray_length;
    float                   m_near_angle;
    float                   m_far_angle;
};

#endif // APPLESEED_FOUNDATION_MATH_SAMPLING_EQUIANGULARSAMPLER_H