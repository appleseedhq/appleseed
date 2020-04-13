
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

// appleseed.renderer headers.
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/poison.h"

// Standard headers.
#include <array>
#include <cassert>
#include <cstdint>

// Forward declarations.
namespace renderer  { class Material; }

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
    typedef foundation::RayInfo3d   RayInfoType;
    typedef std::uint16_t           DepthType;

    class Time
    {
      public:
        float                       m_absolute;                     // absolute time of the ray
        float                       m_normalized;                   // time of the ray, relative to shutter open / close times

        static Time create_with_normalized_time(
            const float             time,
            const float             shutter_open,
            const float             shutter_close);

        Time();                     // leave all fields uninitialized

      private:
        Time(
            const float             absolute,
            const float             normalized);
    };

    enum { MaxMediumCount = 8 };

    struct Medium
    {
        const ObjectInstance*       m_object_instance;
        const Material*             m_material;
        float                       m_ior;

        const Volume* get_volume() const;
    };

    // Public members, in an order that optimizes packing.
    VectorType                      m_rx_org;
    VectorType                      m_rx_dir;
    VectorType                      m_ry_org;
    VectorType                      m_ry_dir;
    Time                            m_time;
    Medium                          m_media[MaxMediumCount];        // always sorted from highest to lowest priority
    VisibilityFlags::Type           m_flags;
    DepthType                       m_depth;
    std::uint8_t                    m_medium_count;
    bool                            m_has_differentials;
    float                           m_min_roughness;

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
        const double                tmin,
        const double                tmax,
        const Time&                 time,
        const VisibilityFlags::Type flags,
        const DepthType             depth);

    // Scale ray differentials by a factor s.
    void scale_differentials(const ValueType s);

    // Copy all media from the source ray.
    void copy_media_from(const ShadingRay& source);

    // Copy all media from the source ray and add an additional medium.
    void add_medium(
        const ShadingRay&           source,
        const ObjectInstance*       object_instance,
        const Material*             material,
        const float                 ior);

    // Copy all media from the source ray except a given medium.
    void remove_medium(
        const ShadingRay&           source,
        const ObjectInstance*       object_instance);

    // Return the currently active medium.
    const ShadingRay::Medium* get_current_medium() const;

    // Return the medium that would be active if we removed the currently active one.
    const ShadingRay::Medium* get_previous_medium() const;

    // Return the IOR of the medium the ray is currently in.
    float get_current_ior() const;

    // Return the IOR of the medium the ray would be in if it would leave the currently active medium.
    float get_previous_ior() const;
};


//
// ShadingRay class implementation.
//

inline ShadingRay::ShadingRay()
  : m_medium_count(0)
  , m_has_differentials(false)
  , m_min_roughness(0.0f)
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
  , m_min_roughness(0.0f)
{
}

inline ShadingRay::ShadingRay(
    const VectorType&               org,
    const VectorType&               dir,
    const double                    tmin,
    const double                    tmax,
    const Time&                     time,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth)
  : RayType(org, dir, tmin, tmax)
  , m_time(time)
  , m_flags(flags)
  , m_depth(depth)
  , m_medium_count(0)
  , m_has_differentials(false)
  , m_min_roughness(0.0f)
{
}

inline const ShadingRay::Medium* ShadingRay::get_current_medium() const
{
    return m_medium_count > 0 ? &m_media[0] : nullptr;
}

inline const ShadingRay::Medium* ShadingRay::get_previous_medium() const
{
    return m_medium_count > 1 ? &m_media[1] : nullptr;
}

inline float ShadingRay::get_current_ior() const
{
    return m_medium_count > 0 ? m_media[0].m_ior : 1.0f;
}

inline float ShadingRay::get_previous_ior() const
{
    return m_medium_count > 1 ? m_media[1].m_ior : 1.0f;
}


//
// ShadingRay::Time class implementation.
//

inline ShadingRay::Time ShadingRay::Time::create_with_normalized_time(
    const float                     time,
    const float                     shutter_open,
    const float                     shutter_close)
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
    const float                     absolute,
    const float                     normalized)
  : m_absolute(absolute)
  , m_normalized(normalized)
{
    assert(m_normalized >= 0.0f);
    assert(m_normalized < 1.0f);
}

}       // namespace renderer

namespace foundation
{
    template <>
    class PoisonImpl<renderer::ShadingRay>
    {
      public:
        static void do_poison(renderer::ShadingRay& ray)
        {
            always_poison(ray.m_rx_org);
            always_poison(ray.m_rx_dir);
            always_poison(ray.m_ry_org);
            always_poison(ray.m_ry_dir);
            always_poison(ray.m_time.m_absolute);
            always_poison(ray.m_time.m_normalized);

            for (size_t i = 0; i < renderer::ShadingRay::MaxMediumCount; ++i)
            {
                always_poison(ray.m_media[i].m_object_instance);
                always_poison(ray.m_media[i].m_material);
                always_poison(ray.m_media[i].m_ior);
            }

            always_poison(ray.m_flags);
            always_poison(ray.m_depth);

            // Don't poison m_medium_count or m_has_differentials since
            // they are properly initialized by the default constructor.
        }
    };
}   // namespace foundation
