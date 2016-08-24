
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

// Forward declarations.
namespace renderer  { class BSDF; }

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

    enum { MaxMediumCount = 8 };

    struct Medium
    {
        const ObjectInstance*       m_object_instance;
        const BSDF*                 m_bsdf;
        double                      m_ior;
    };

    // Public members, in an order that optimizes packing.
    RayType                         m_rx;
    RayType                         m_ry;
    Time                            m_time;
    Medium                          m_media[MaxMediumCount];        // always sorted from highest to lowest priority
    VisibilityFlags::Type           m_flags;
    DepthType                       m_depth;
    foundation::uint8               m_medium_count;
    bool                            m_has_differentials;

    // Constructors.
    ShadingRay();                   // only partially initialize the object
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

    // Copy all media from the source ray.
    void copy_media_from(const ShadingRay& source);

    // Copy all media from the source ray and add an additional medium.
    void add_medium(
        const ShadingRay&           source,
        const ObjectInstance*       object_instance,
        const BSDF*                 bsdf,
        const double                ior);

    // Copy all media from the source ray except a given medium.
    void remove_medium(
        const ShadingRay&           source,
        const ObjectInstance*       object_instance);

    // Return the currently active medium.
    const ShadingRay::Medium* get_current_medium() const;

    // Return the medium that would be active if we removed the currently active one.
    const ShadingRay::Medium* get_previous_medium() const;

    // Return the IOR of the medium the ray is currently in.
    double get_current_ior() const;

    // Return the IOR of the medium the ray would be in if it would leave the currently active medium.
    double get_previous_ior() const;
};


//
// ShadingRay class implementation.
//

inline ShadingRay::ShadingRay()
  : m_medium_count(0)
  , m_has_differentials(false)
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
  , m_medium_count(0)
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
  , m_medium_count(0)
  , m_has_differentials(false)
{
}

inline const ShadingRay::Medium* ShadingRay::get_current_medium() const
{
    return m_medium_count > 0 ? &m_media[0] : 0;
}

inline const ShadingRay::Medium* ShadingRay::get_previous_medium() const
{
    return m_medium_count > 1 ? &m_media[1] : 0;
}

inline double ShadingRay::get_current_ior() const
{
    return m_medium_count > 0 ? m_media[0].m_ior : 1.0;
}

inline double ShadingRay::get_previous_ior() const
{
    return m_medium_count > 1 ? m_media[1].m_ior : 1.0;
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
