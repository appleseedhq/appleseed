
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

// Interface header.
#include "transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/root.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstring>

using namespace foundation;

namespace renderer
{

TransformSequence::TransformSequence()
  : m_capacity(0)
  , m_size(0)
  , m_keys(nullptr)
  , m_interpolators(nullptr)
  , m_can_swap_handedness(false)
  , m_all_swap_handedness(false)
{
}

TransformSequence::TransformSequence(const TransformSequence& rhs)
{
    copy_from(rhs);
}

TransformSequence::TransformSequence(TransformSequence&& rhs) APPLESEED_NOEXCEPT
  : m_capacity(rhs.m_capacity)
  , m_size(rhs.m_size)
  , m_keys(rhs.m_keys)
  , m_interpolators(rhs.m_interpolators)
  , m_can_swap_handedness(rhs.m_can_swap_handedness)
  , m_all_swap_handedness(rhs.m_all_swap_handedness)
{
    rhs.m_keys = nullptr;
    rhs.m_interpolators = nullptr;
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

TransformSequence& TransformSequence::operator=(TransformSequence&& rhs) APPLESEED_NOEXCEPT
{
    m_capacity = rhs.m_capacity;
    m_size = rhs.m_size;
    std::swap(m_keys, rhs.m_keys);
    std::swap(m_interpolators, rhs.m_interpolators);
    m_can_swap_handedness = rhs.m_can_swap_handedness;
    m_all_swap_handedness = rhs.m_all_swap_handedness;
    rhs.clear();
    return *this;
}

void TransformSequence::clear()
{
    m_capacity = 0;
    m_size = 0;

    delete[] m_keys;
    m_keys = nullptr;

    delete[] m_interpolators;
    m_interpolators = nullptr;

    m_can_swap_handedness = false;
    m_all_swap_handedness = false;
}

void TransformSequence::set_transform(
    const float         time,
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

        delete[] m_keys;
        m_keys = new_keys;
    }

    m_keys[m_size].m_time = time;
    m_keys[m_size].m_transform = transform;

    ++m_size;
}

void TransformSequence::get_transform(
    const size_t        index,
    float&              time,
    Transformd&         transform) const
{
    assert(index < m_size);

    time = m_keys[index].m_time;
    transform = m_keys[index].m_transform;
}

const Transformd& TransformSequence::get_earliest_transform() const
{
    if (m_size == 0)
        return Transformd::identity();

    float earliest_time = m_keys[0].m_time;
    size_t earliest_index = 0;

    for (size_t i = 1; i < m_size; ++i)
    {
        const float time = m_keys[i].m_time;

        if (earliest_time > time)
        {
            earliest_time = time;
            earliest_index = i;
        }
    }

    return m_keys[earliest_index].m_transform;
}

void TransformSequence::optimize()
{
    if (m_size > 1)
    {
        for (size_t i = 0; i < m_size; ++i)
        {
            const bool same_left =
                i == 0 ||
                feq(m_keys[i - 1].m_transform, m_keys[i].m_transform);

            const bool same_right =
                i == m_size - 1 ||
                feq(m_keys[i + 1].m_transform, m_keys[i].m_transform);

            if (same_left && same_right)
            {
                memmove(&m_keys[i], &m_keys[i + 1], (m_size - 1 - i) * sizeof(TransformKey));
                --m_size;
            }
        }
    }
}

bool TransformSequence::prepare()
{
    delete[] m_interpolators;
    m_interpolators = nullptr;

    bool success = true;

    if (m_size > 1)
    {
        std::sort(m_keys, m_keys + m_size);

        m_interpolators = new TransformInterpolatord[m_size - 1];

        for (size_t i = 0; i < m_size - 1; ++i)
        {
            success = success &&
                m_interpolators[i].set_transforms(
                    m_keys[i].m_transform,
                    m_keys[i + 1].m_transform);
        }
    }

    m_can_swap_handedness = false;
    m_all_swap_handedness = true;

    for (size_t i = 0; i < m_size; ++i)
    {
        const bool swaps = m_keys[i].m_transform.swaps_handedness();

        if (swaps)
            m_can_swap_handedness = true;

        m_all_swap_handedness = m_all_swap_handedness && swaps;

        if (m_can_swap_handedness && !m_all_swap_handedness)
            break;
    }

    return success;
}

bool TransformSequence::swaps_handedness(const Transformd& xform) const
{
    if (!m_can_swap_handedness)
        return false;

    if (m_all_swap_handedness)
        return true;

    return xform.swaps_handedness();
}

TransformSequence TransformSequence::operator*(const TransformSequence& rhs) const
{
    TransformSequence result;

    size_t lhs_i = 0, rhs_i = 0;
    Transformd scratch;

    while (lhs_i < m_size && rhs_i < rhs.m_size)
    {
        const float lhs_t = m_keys[lhs_i].m_time;
        const float rhs_t = rhs.m_keys[rhs_i].m_time;

        if (lhs_t == rhs_t)
        {
            result.set_transform(lhs_t, m_keys[lhs_i].m_transform * rhs.m_keys[rhs_i].m_transform);
            ++lhs_i;
            ++rhs_i;
        }
        else if (lhs_t < rhs_t)
        {
            result.set_transform(lhs_t, m_keys[lhs_i].m_transform * rhs.evaluate(lhs_t, scratch));
            ++lhs_i;
        }
        else
        {
            result.set_transform(rhs_t, evaluate(rhs_t, scratch) * rhs.m_keys[rhs_i].m_transform);
            ++rhs_i;
        }
    }

    while (lhs_i < m_size)
    {
        const float lhs_t = m_keys[lhs_i].m_time;
        result.set_transform(lhs_t, m_keys[lhs_i].m_transform * rhs.evaluate(lhs_t, scratch));
        ++lhs_i;
    }

    while (rhs_i < rhs.m_size)
    {
        const float rhs_t = rhs.m_keys[rhs_i].m_time;
        result.set_transform(rhs_t, evaluate(rhs_t, scratch) * rhs.m_keys[rhs_i].m_transform);
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

        for (size_t i = 0; i < m_size; ++i)
            m_keys[i] = rhs.m_keys[i];
    }
    else m_keys = nullptr;

    if (rhs.m_interpolators)
    {
        m_interpolators = new TransformInterpolatord[m_size - 1];

        for (size_t i = 0; i < m_size - 1; ++i)
            m_interpolators[i] = rhs.m_interpolators[i];
    }
    else m_interpolators = nullptr;

    m_can_swap_handedness = rhs.m_can_swap_handedness;
    m_all_swap_handedness = rhs.m_all_swap_handedness;
}

void TransformSequence::interpolate(
    const float         time,
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

    const float begin_time = m_keys[begin].m_time;
    const float end_time = m_keys[end].m_time;

    assert(end_time > begin_time);

    const float t = (time - begin_time) / (end_time - begin_time);

    m_interpolators[begin].evaluate(static_cast<double>(t), result);
}

namespace
{
    struct LinearFunction
    {
        const double    m_s0;
        const double    m_s1;
        const double    m_rcp_max_theta;
        const double    m_d;

        LinearFunction(const double s0, const double s1, const double max_theta)
          : m_s0(s0)
          , m_s1(s1)
          , m_rcp_max_theta(1.0 / max_theta)
          , m_d((m_s1 - m_s0) * m_rcp_max_theta)
        {
        }

        double f(const double theta) const
        {
            return lerp(m_s0, m_s1, theta * m_rcp_max_theta);
        }

        double d(const double theta) const
        {
            return m_d;
        }
    };

    struct TrajectoryX
    {
        const LinearFunction&   m_sx;
        const LinearFunction&   m_sy;
        const Vector2d&         m_p;

        TrajectoryX(const LinearFunction& sx, const LinearFunction& sy, const Vector2d& p)
          : m_sx(sx)
          , m_sy(sy)
          , m_p(p)
        {
        }

        double f(const double theta) const
        {
            return m_sx.f(theta) * std::cos(theta) * m_p.x - m_sy.f(theta) * std::sin(theta) * m_p.y;
        }

        double d(const double theta) const
        {
            return (m_sx.d(theta) * m_p.x - m_sy.f(theta) * m_p.y) * std::cos(theta) -
                   (m_sx.f(theta) * m_p.x + m_sy.d(theta) * m_p.y) * std::sin(theta);
        }

        double dd(const double theta) const
        {
            const double a = m_sx.d(theta) * m_p.x - m_sy.f(theta) * m_p.y;
            const double b = m_sx.f(theta) * m_p.x + m_sy.d(theta) * m_p.y;
            const double ap = -m_sy.d(theta) * m_p.y;
            const double bp = m_sx.d(theta) * m_p.x;
            return (ap - b) * std::cos(theta) - (bp + a) * std::sin(theta);
        }
    };

    struct TrajectoryY
    {
        const LinearFunction&   m_sx;
        const LinearFunction&   m_sy;
        const Vector2d&         m_p;

        TrajectoryY(const LinearFunction& sx, const LinearFunction& sy, const Vector2d& p)
          : m_sx(sx)
          , m_sy(sy)
          , m_p(p)
        {
        }

        double f(const double theta) const
        {
            return m_sx.f(theta) * std::sin(theta) * m_p.x + m_sy.f(theta) * std::cos(theta) * m_p.y;
        }

        double d(const double theta) const
        {
            return (m_sx.f(theta) * m_p.x + m_sy.d(theta) * m_p.y) * std::cos(theta) +
                   (m_sx.d(theta) * m_p.x - m_sy.f(theta) * m_p.y) * std::sin(theta);
        }

        double dd(const double theta) const
        {
            const double a = m_sx.f(theta) * m_p.x + m_sy.d(theta) * m_p.y;
            const double b = m_sx.d(theta) * m_p.x - m_sy.f(theta) * m_p.y;
            const double ap = m_sx.d(theta) * m_p.x;
            const double bp = -m_sy.d(theta) * m_p.y;
            return (ap + b) * std::cos(theta) + (bp - a) * std::sin(theta);
        }
    };

    template <typename Class>
    struct Bind
    {
        typedef double (Class::*Method)(double) const;

        const Class&    m_class;
        const Method&   m_method;

        Bind(const Class& class_, const Method& method)
          : m_class(class_)
          , m_method(method)
        {
        }

        double operator()(const double t) const
        {
            return (m_class.*m_method)(t);
        }
    };

    struct RootHandler
    {
        const TrajectoryX&          m_tx;
        const TrajectoryY&          m_ty;
        const LinearFunction&       m_sz;
        const Transformd&           m_axis_to_z;
        const Vector3d&             m_corner;
        AABB3d&                     m_motion_bbox;

        RootHandler(
            const TrajectoryX&      tx,
            const TrajectoryY&      ty,
            const LinearFunction&   sz,
            const Transformd&       axis_to_z,
            const Vector3d&         corner,
            AABB3d&                 motion_bbox)
          : m_tx(tx)
          , m_ty(ty)
          , m_sz(sz)
          , m_axis_to_z(axis_to_z)
          , m_corner(corner)
          , m_motion_bbox(motion_bbox)
        {
        }

        void operator()(const double theta) const
        {
            const Vector3d extremum(m_tx.f(theta), m_ty.f(theta), m_sz.f(theta) * m_corner.z);
            m_motion_bbox.insert(m_axis_to_z.point_to_parent(extremum));
        };
    };
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

    // Parameters.
    const double MinLength = HalfPi<double>();
    const double RootEps = 1.0e-6;
    const double GrowEps = 1.0e-4;
    const size_t MaxIterations = 100;

    // Start with the bounding box at 'from'.
    const AABB3d from_bbox = from.to_parent(bbox);
    AABB3d motion_bbox = from_bbox;

    // Setup an interpolator between 'from' and 'to'.
    TransformInterpolatord interpolator;
    if (!interpolator.set_transforms(from, to))
        return motion_bbox;

    // Compute the scalings at 'from' and 'to'.
    const Vector3d s0 = interpolator.get_s0();
    const Vector3d s1 = interpolator.get_s1();

    // Compute the relative rotation between 'from' and 'to'.
    const Quaterniond q =
        interpolator.get_q1() * conjugate(interpolator.get_q0());

    // Transform the relative rotation to the axis-angle representation.
    Vector3d axis;
    double angle;
    q.extract_axis_angle(axis, angle);
    if (axis.z < 0.0)
        angle = -angle;

    // The following code only makes sense if there is a rotation component.
    if (angle == 0.0)
        return motion_bbox;

    // Compute the rotation required to align the rotation axis with the Z axis.
    const Vector3d Z(0.0, 0.0, 1.0);
    const Vector3d perp = cross(Z, axis);
    const double perp_norm = norm(perp);
    Transformd axis_to_z;
    if (perp_norm == 0.0)
        axis_to_z = Transformd::identity();
    else
    {
        const Vector3d v = perp / perp_norm;
        const double sin_a = clamp(perp_norm, -1.0, 1.0);
        const double cos_a = std::sqrt(1.0 - sin_a * sin_a);
        axis_to_z.set_local_to_parent(Matrix4d::make_rotation(v, cos_a, +sin_a));
        axis_to_z.set_parent_to_local(Matrix4d::make_rotation(v, cos_a, -sin_a));
    }

    // Build the linear scaling functions Sx(theta), Sy(theta) and Sz(theta).
    const LinearFunction sx(1.0, s1.x / s0.x, angle);
    const LinearFunction sy(1.0, s1.y / s0.y, angle);
    const LinearFunction sz(1.0, s1.z / s0.z, angle);

    // Consider each corner of the bounding box. Notice an important trick here:
    // we take advantage of the way AABB::compute_corner() works to only iterate
    // over the four corners at Z=min instead of over all eight corners since we
    // anyway transform the rotation to be aligned with the Z axis.
    for (size_t c = 0; c < 4; ++c)
    {
        // Compute the position of this corner at 'from'.
        const Vector3d corner = axis_to_z.point_to_local(from_bbox.compute_corner(c));
        const Vector2d corner2d(corner.x, corner.y);

        // Build the trajectory functions x(theta) and y(theta).
        const TrajectoryX tx(sx, sy, corner2d);
        const TrajectoryY ty(sx, sy, corner2d);

        const double a = std::min(angle, 0.0);
        const double b = std::max(angle, 0.0);

        // Find all the rotation angles at which this corner is an extremum and update the motion bounding box.
        RootHandler root_handler(tx, ty, sz, axis_to_z, corner, motion_bbox);
        find_multiple_roots_newton(
            Bind<TrajectoryX>(tx, &TrajectoryX::d),
            Bind<TrajectoryX>(tx, &TrajectoryX::dd),
            a, b,
            MinLength,
            RootEps,
            MaxIterations,
            root_handler);
        find_multiple_roots_newton(
            Bind<TrajectoryY>(ty, &TrajectoryY::d),
            Bind<TrajectoryY>(ty, &TrajectoryY::dd),
            a, b,
            MinLength,
            RootEps,
            MaxIterations,
            root_handler);
    }

    motion_bbox.robust_grow(GrowEps);

    return motion_bbox;
}

}   // namespace renderer
