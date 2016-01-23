
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRAY_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRAY_H

// appleseed.renderer headers.
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>

namespace renderer
{

//
// A ray as it is used throughout the renderer.
//
// todo: add importance/contribution?
//

class ShadingRay
  : public foundation::Ray3d
{
  public:
    // Types.
    typedef double                  ValueType;
    typedef foundation::Vector3d    VectorType;
    typedef foundation::Ray3d       RayType;
    typedef foundation::RayInfo3d   RayInfoType;
    typedef foundation::uint16      DepthType;

    class Time
    {
      public:
        double                      m_absolute;                     // absolute time of the ray
        double                      m_normalized;                   // time of the ray, relative to shutter open / close times

        static Time create_with_normalized_time(
            const double            time,
            const double            shutter_open,
            const double            shutter_close);

        Time();                     // leave all fields uninitialized

      private:
        Time(
            const double            absolute,
            const double            normalized);
    };

    enum { MaxVolumeCount = 8 };

    // Public members, in an order that optimizes packing.
    RayType                         m_rx;
    RayType                         m_ry;
    Time                            m_time;
    const ObjectInstance*           m_volumes[MaxVolumeCount];      // always sorted from highest to lowest priority
    VisibilityFlags::Type           m_flags;
    DepthType                       m_depth;
    foundation::uint8               m_volume_count;
    bool                            m_has_differentials;

    // Constructors.
    ShadingRay();                   // leave all fields uninitialized
    ShadingRay(
        const RayType&              ray,
        const Time&                 time,
        const VisibilityFlags::Type flags,
        const DepthType             depth);
    ShadingRay(
        const RayType&              ray,
        const Time&                 time,
        const VisibilityFlags::Type flags,
        const DepthType             depth,
        const RayType&              rx,
        const RayType&              ry);
    ShadingRay(
        const VectorType&           org,
        const VectorType&           dir,
        const Time&                 time,
        const VisibilityFlags::Type flags,
        const DepthType             depth);
    ShadingRay(
        const VectorType&           org,
        const VectorType&           dir,
        const ValueType             tmin,
        const ValueType             tmax,
        const Time&                 time,
        const VisibilityFlags::Type flags,
        const DepthType             depth);

    // Copy all volumes from the source ray.
    void copy_volumes_from(const ShadingRay& source);

    // Copy all volumes from the source ray and add an additional volume.
    void add_volume(const ShadingRay& source, const ObjectInstance* volume);

    // Copy all volumes from the source ray except a given volume.
    void remove_volume(const ShadingRay& source, const ObjectInstance* volume);

    // Return the highest volume priority from the ray.
    foundation::uint8 get_highest_volume_priority() const;
};

// Transform a ShadingRay.
template <typename U>
ShadingRay transform_to_local(
    const foundation::Transform<U>& transform,
    const ShadingRay&               ray);
template <typename U>
ShadingRay transform_to_parent(
    const foundation::Transform<U>& transform,
    const ShadingRay&               ray);


//
// ShadingRay class implementation.
//

inline ShadingRay::ShadingRay()
  : m_volume_count(0)
  , m_has_differentials(false)
{
}

inline ShadingRay::ShadingRay(
    const RayType&                  ray,
    const Time&                     time,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth)
  : RayType(ray)
  , m_time(time)
  , m_flags(flags)
  , m_depth(depth)
  , m_volume_count(0)
  , m_has_differentials(false)
{
}

inline ShadingRay::ShadingRay(
    const RayType&                  ray,
    const Time&                     time,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth,
    const RayType&                  rx,
    const RayType&                  ry)
  : RayType(ray)
  , m_time(time)
  , m_flags(flags)
  , m_depth(depth)
  , m_volume_count(0)
  , m_has_differentials(true)
  , m_rx(rx)
  , m_ry(ry)
{
}

inline ShadingRay::ShadingRay(
    const VectorType&               org,
    const VectorType&               dir,
    const Time&                     time,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth)
  : RayType(org, dir)
  , m_time(time)
  , m_flags(flags)
  , m_depth(depth)
  , m_volume_count(0)
  , m_has_differentials(false)
{
}

inline ShadingRay::ShadingRay(
    const VectorType&               org,
    const VectorType&               dir,
    const ValueType                 tmin,
    const ValueType                 tmax,
    const Time&                     time,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth)
  : RayType(org, dir, tmin, tmax)
  , m_time(time)
  , m_flags(flags)
  , m_depth(depth)
  , m_volume_count(0)
  , m_has_differentials(false)
{
}

template <typename U>
inline ShadingRay transform_to_local(
    const foundation::Transform<U>& transform,
    const ShadingRay&               ray)
{
    if (ray.m_has_differentials)
    {
        return
            ShadingRay(
                transform.transform_to_local(ray),
                ray.m_time,
                ray.m_flags,
                ray.m_depth,
                transform.transform_to_local(ray.m_rx),
                transform.transform_to_local(ray.m_ry));
    }
    else
    {
        return
            ShadingRay(
                transform.transform_to_local(ray),
                ray.m_time,
                ray.m_flags,
                ray.m_depth);
    }
}

template <typename U>
inline ShadingRay transform_to_parent(
    const foundation::Transform<U>& transform,
    const ShadingRay&               ray)
{
    if (ray.m_has_differentials)
    {
        return
            ShadingRay(
                transform.transform_to_parent(ray),
                ray.m_time,
                ray.m_flags,
                ray.m_depth,
                transform.transform_to_parent(ray.m_rx),
                transform.transform_to_parent(ray.m_ry));
    }
    else
    {
        return
            ShadingRay(
                transform.transform_to_parent(ray),
                ray.m_time,
                ray.m_flags,
                ray.m_depth);
    }
}

inline foundation::uint8 ShadingRay::get_highest_volume_priority() const
{
    return m_volume_count == 0 ? 0 : m_volumes[0]->get_volume_priority();
}


//
// ShadingRay::Time class implementation.
//

inline ShadingRay::Time ShadingRay::Time::create_with_normalized_time(
    const double                    time,
    const double                    shutter_open,
    const double                    shutter_close)
{
    return
        Time(
            foundation::lerp(shutter_open, shutter_close, time),
            time);
}

inline ShadingRay::Time::Time()
{
}

inline ShadingRay::Time::Time(
    const double                    absolute,
    const double                    normalized)
  : m_absolute(absolute)
  , m_normalized(normalized)
{
    assert(m_normalized >= 0.0);
    assert(m_normalized < 1.0);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRAY_H
