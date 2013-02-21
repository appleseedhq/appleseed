
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include <functional>
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

namespace
{
    const Transformd Identity = Transformd::identity();
}

const Transformd& TransformSequence::get_earliest_transform() const
{
    if (m_size == 0)
        return Identity;

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

namespace
{
    struct LinearScaling
    {
        const double    m_s0;
        const double    m_s1;
        const double    m_rcp_max_theta;
        const double    m_d;

        LinearScaling(const double s0, const double s1, const double max_theta)
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

    struct PositionX
    {
        const LinearScaling&    m_sx;
        const LinearScaling&    m_sy;
        const Vector2d&         m_p;

        PositionX(const LinearScaling& sx, const LinearScaling& sy, const Vector2d& p)
          : m_sx(sx)
          , m_sy(sy)
          , m_p(p)
        {
        }

        double f(const double theta) const
        {
            return m_sx.f(theta) * cos(theta) * m_p.x - m_sy.f(theta) * sin(theta) * m_p.y;
        }

        double d(const double theta) const
        {
            return (m_sx.d(theta) * m_p.x - m_sy.f(theta) * m_p.y) * cos(theta) -
                   (m_sx.f(theta) * m_p.x + m_sy.d(theta) * m_p.y) * sin(theta);
        }
    };

    struct PositionY
    {
        const LinearScaling&    m_sx;
        const LinearScaling&    m_sy;
        const Vector2d&         m_p;

        PositionY(const LinearScaling& sx, const LinearScaling& sy, const Vector2d& p)
          : m_sx(sx)
          , m_sy(sy)
          , m_p(p)
        {
        }

        double f(const double theta) const
        {
            return m_sx.f(theta) * sin(theta) * m_p.x + m_sy.f(theta) * cos(theta) * m_p.y;
        }

        double d(const double theta) const
        {
            return (m_sx.f(theta) * m_p.x + m_sy.d(theta) * m_p.y) * cos(theta) +
                   (m_sx.d(theta) * m_p.x - m_sy.f(theta) * m_p.y) * sin(theta);
        }
    };

    template <typename T, typename F>
    bool find_root_bisection(const F& f, T a, T b, const T eps, T& root)
    {
        T fa = f(a);
        T fb = f(b);

        if (fa * fb > T(0.0))
            return false;

        while (abs(b - a) > eps)
        {
            const T m = (a + b) / T(2.0);
            const T fm = f(m);

            if (fa * fm <= T(0.0))
            {
                b = m;
                fb = fm;
            }
            else
            {
                assert(fm * fb <= T(0.0));
                a = m;
                fa = fm;
            }
        }

        root = (a + b) / T(2.0);
        return true;
    }

    template <typename T, typename F>
    void find_all_roots_bisection(
        const F&                        f,
        const T                         a,
        const T                         b,
        const T                         max_length,
        const T                         eps,
        const function<void(double)>&   root_handler)
    {
        if (abs(b - a) > max_length)
        {
            const T m = (a + b) / T(2.0);
            find_all_roots_bisection(f, a, m, max_length, eps, root_handler);
            find_all_roots_bisection(f, m, b, max_length, eps, root_handler);
        }
        else
        {
            T root;
            if (find_root_bisection(f, a, b, eps, root))
                root_handler(root);
        }
    }

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

    const double MinLength = Pi / 8.0;
    const double RootEps = 1.0e-6;
    const double GrowEps = 1.0e-4;

    // Setup an interpolator between 'from' and 'to'.
    TransformInterpolatord interpolator;
    if (!interpolator.set_transforms(from, to))
        return from.to_parent(bbox);

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
        return from.to_parent(bbox);

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

    // Build the linear scaling functions Sx(theta), Sy(theta) and Sz(theta).
    const LinearScaling sx(1.0, s1.x / s0.x, angle);
    const LinearScaling sy(1.0, s1.y / s0.y, angle);
    const LinearScaling sz(1.0, s1.z / s0.z, angle);

    // Start with the bounding box at 'from'.
    const AABB3d from_bbox = from.to_parent(bbox);
    AABB3d motion_bbox = from_bbox;

    // Consider each corner point of the bounding box.
    for (size_t c = 0; c < 8; ++c)
    {
        // Compute the position of this corner at 'from'.
        const Vector3d corner = axis_to_z.point_to_local(from_bbox.compute_corner(c));
        const Vector2d corner2d(corner.x, corner.y);

        // Build the trajectory functions x(theta) and y(theta).
        const PositionX fx(sx, sy, corner2d);
        const PositionY fy(sx, sy, corner2d);

        // Find all the rotation angles at which this corner is an extremum and update the motion bounding box.
        auto root_handler = [fx, fy, sz, corner, axis_to_z, &motion_bbox](const double theta) mutable
        {
            const double fz = sz.f(theta) * corner.z;
            const Vector3d extremum(fx.f(theta), fy.f(theta), fz);
            motion_bbox.insert(axis_to_z.point_to_parent(extremum));
        };
        find_all_roots_bisection(Bind<PositionX>(fx, &PositionX::d), 0.0, angle, MinLength, RootEps, root_handler);
        find_all_roots_bisection(Bind<PositionY>(fy, &PositionY::d), 0.0, angle, MinLength, RootEps, root_handler);
    }

    motion_bbox.robust_grow(GrowEps);

    return motion_bbox;
}

}   // namespace renderer
