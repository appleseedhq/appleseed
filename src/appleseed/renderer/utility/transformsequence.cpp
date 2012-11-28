
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

TransformSequence::TransformSequence()
  : m_capacity(0)
  , m_size(0)
  , m_keys(0)
  , m_interpolators(0)
{
}

TransformSequence::TransformSequence(const TransformSequence& rhs)
{
    copy_from(rhs);
}

TransformSequence::~TransformSequence()
{
    clear();
}

TransformSequence& TransformSequence::operator=(const TransformSequence& rhs)
{
    copy_from(rhs);
    return *this;
}

void TransformSequence::clear()
{
    m_capacity = 0;
    m_size = 0;

    delete [] m_keys;
    m_keys = 0;

    delete [] m_interpolators;
    m_interpolators = 0;
}

void TransformSequence::set_transform(
    const double        time,
    const Transformd&   transform)
{
    assert(m_size <= m_capacity);

    for (size_t i = 0; i < m_size; ++i)
    {
        if (m_keys[i].m_time == time)
        {
            m_keys[i].m_transform = transform;
            return;
        }
    }

    if (m_size == m_capacity)
    {
        m_capacity += m_capacity > 0 ? m_capacity : 1;

        TransformKey* new_keys = new TransformKey[m_capacity];

        for (size_t i = 0; i < m_size; ++i)
            new (&new_keys[i]) TransformKey(m_keys[i]);

        delete [] m_keys;
        m_keys = new_keys;
    }

    m_keys[m_size].m_time = time;
    m_keys[m_size].m_transform = transform;

    ++m_size;
}

void TransformSequence::get_transform(
    const size_t        index,
    double&             time,
    Transformd&         transform) const
{
    assert(index < m_size);

    time = m_keys[index].m_time;
    transform = m_keys[index].m_transform;
}

Transformd& TransformSequence::earliest_transform()
{
    assert(m_size > 0);

    double earliest_time = m_keys[0].m_time;
    size_t earliest_index = 0;

    for (size_t i = 1; i < m_size; ++i)
    {
        const double time = m_keys[i].m_time;

        if (earliest_time > time)
        {
            earliest_time = time;
            earliest_index = i;
        }
    }

    return m_keys[earliest_index].m_transform;
}

namespace
{
    const Transformd Identity = Transformd::identity();
}

const Transformd& TransformSequence::earliest_transform() const
{
    return
        m_size == 0
            ? Identity
            : const_cast<TransformSequence*>(this)->earliest_transform();
}

bool TransformSequence::prepare()
{
    delete [] m_interpolators;
    m_interpolators = 0;

    bool success = true;

    if (m_size > 1)
    {
        sort(m_keys, m_keys + m_size);

        m_interpolators = new TransformInterpolatord[m_size - 1];

        for (size_t i = 0; i < m_size - 1; ++i)
        {
            success = success &&
                m_interpolators[i].set_transforms(
                    m_keys[i].m_transform,
                    m_keys[i + 1].m_transform);
        }
    }

    return success;
}

TransformSequence TransformSequence::operator*(const TransformSequence& rhs) const
{
    TransformSequence result;

    size_t lhs_i = 0, rhs_i = 0;
    Transformd tmp;

    while (lhs_i < m_size && rhs_i < rhs.m_size)
    {
        const double lhs_t = m_keys[lhs_i].m_time;
        const double rhs_t = rhs.m_keys[rhs_i].m_time;

        if (lhs_t == rhs_t)
        {
            result.set_transform(lhs_t, m_keys[lhs_i].m_transform * rhs.m_keys[rhs_i].m_transform);
            ++lhs_i;
            ++rhs_i;
        }
        else if (lhs_t < rhs_t)
        {
            result.set_transform(lhs_t, m_keys[lhs_i].m_transform * rhs.evaluate(lhs_t, tmp));
            ++lhs_i;
        }
        else
        {
            result.set_transform(rhs_t, evaluate(rhs_t, tmp) * rhs.m_keys[rhs_i].m_transform);
            ++rhs_i;
        }
    }

    while (lhs_i < m_size)
    {
        const double lhs_t = m_keys[lhs_i].m_time;
        result.set_transform(lhs_t, m_keys[lhs_i].m_transform * rhs.evaluate(lhs_t, tmp));
        ++lhs_i;
    }

    while (rhs_i < rhs.m_size)
    {
        const double rhs_t = rhs.m_keys[rhs_i].m_time;
        result.set_transform(rhs_t, evaluate(rhs_t, tmp) * rhs.m_keys[rhs_i].m_transform);
        ++rhs_i;
    }

    return result;
}

void TransformSequence::copy_from(const TransformSequence& rhs)
{
    m_capacity = rhs.m_size;    // shrink to size on copy
    m_size = rhs.m_size;

    if (rhs.m_keys)
    {
        m_keys = new TransformKey[m_size];
        memcpy(m_keys, rhs.m_keys, m_size * sizeof(TransformKey));
    }
    else m_keys = 0;

    if (rhs.m_interpolators)
    {
        m_interpolators = new TransformInterpolatord[m_size - 1];
        memcpy(m_interpolators, rhs.m_interpolators, (m_size - 1) * sizeof(TransformInterpolatord));
    }
    else m_interpolators = 0;
}

void TransformSequence::interpolate(
    const double        time,
    Transformd&         result) const
{
    assert(m_size > 0);

    size_t begin = 0;
    size_t end = m_size;

    while (end - begin > 1)
    {
        const size_t mid = (begin + end) / 2;
        if (time < m_keys[mid].m_time)
            end = mid;
        else begin = mid;
    }

    const double begin_time = m_keys[begin].m_time;
    const double end_time = m_keys[end].m_time;

    assert(end_time > begin_time);

    const double t = (time - begin_time) / (end_time - begin_time);

    m_interpolators[begin].evaluate(t, result);
}

AABB3d TransformSequence::compute_motion_segment_bbox(
    const AABB3d&       bbox,
    const Transformd&   from,
    const Transformd&   to) const
{
    //
    // Reference:
    //
    //   http://gruenschloss.org/motion-blur/motion-blur.pdf page 11.
    //

    AABB3d result = from.to_parent(bbox);

    // Compute the relative rotation between 'from' and 'to'.
    TransformInterpolatord interpolator;
    if (!interpolator.set_transforms(from, to))
        return result;
    const Quaterniond q =
        interpolator.get_q1() * conjugate(interpolator.get_q0());

    // Transform the relative rotation to the axis-angle representation.
    Vector3d axis;
    double angle;
    q.extract_axis_angle(axis, angle);
    if (angle == 0.0)
        return result;

    // Compute the rotation required to align the rotation axis with the Z axis.
    const Vector3d Z(0.0, 0.0, 1.0);
    const Vector3d perp = cross(Z, axis);
    const double perp_norm = norm(perp);
    const Transformd axis_to_z(
        perp_norm == 0.0
            ? Matrix4d::identity()
            : Matrix4d::rotation(
                  perp / perp_norm,
                  asin(clamp(perp_norm, -1.0, 1.0))));
    if (axis == -Z)
        angle = -angle;

    // Consider each corner point of the bounding box.
    for (size_t c = 0; c < 8; ++c)
    {
        // Compute the position of this corner at 'from'.
        const Vector3d p0 = axis_to_z.point_to_local(bbox.compute_corner(c));

        // Compute the four rotations that place the corner at an extremum in X or Y.
        const double theta_maxx = atan2(-p0.y, p0.x);
        const double theta_maxy = atan2(p0.x, p0.y);
        const double thetas[4] =
        {
            mod(theta_maxy, TwoPi),
            mod(theta_maxx + Pi, TwoPi),
            mod(theta_maxy + Pi, TwoPi),
            mod(theta_maxx, TwoPi)
        };

        // Compute the quadrant the corner belongs to at 'from'.
        const int qinit = p0.y > 0 ? (p0.x > 0 ? 0 : 1) : (p0.x < 0 ? 2 : 3);

        for (int i = 0; i < 4; ++i)
        {
            // Compute the orientation of the next extremum.
            const int theta_index = angle > 0.0 ? qinit + i : qinit - i - 1;
            const double theta = thetas[mod(theta_index, 4)];

            // Don't consider extrema past the full rotation.
            if ((angle > 0.0 && theta > angle) ||
                (angle < 0.0 && theta - TwoPi < angle))
                break;

            // Compute the position of this extrema.
            const double cos_theta = cos(theta);
            const double sin_theta = sin(theta);
            const Vector3d extremum(
                p0.x * cos_theta - p0.y * sin_theta,
                p0.x * sin_theta + p0.y * cos_theta,
                p0.z);

            // Include the extrema in the final bounding box.
            result.insert(axis_to_z.point_to_parent(extremum));
        }
    }

    return result;
}

}   // namespace renderer
