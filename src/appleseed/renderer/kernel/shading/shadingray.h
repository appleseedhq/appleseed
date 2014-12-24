
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/ray.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

namespace renderer
{

//
// A ray as it is used throughout the renderer.
//
// todo: add importance/contribution?
// todo: replace ray by ray differential.
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

    // Public members.
    double                          m_time;
    double                          m_dtime;
    VisibilityFlags::Type           m_flags;
    DepthType                       m_depth;

    // Constructors.
    ShadingRay();                   // leave all fields uninitialized
    ShadingRay(
        const RayType&              ray,
        const double                time,
        const double                dtime,
        const VisibilityFlags::Type flags,
        const DepthType             depth);
    ShadingRay(
        const VectorType&           org,
        const VectorType&           dir,
        const double                time,
        const double                dtime,
        const VisibilityFlags::Type flags,
        const DepthType             depth);
    ShadingRay(
        const VectorType&           org,
        const VectorType&           dir,
        const ValueType             tmin,
        const ValueType             tmax,
        const double                time,
        const double                dtime,
        const VisibilityFlags::Type flags,
        const DepthType             depth);
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
{
}

inline ShadingRay::ShadingRay(
    const RayType&                  ray,
    const double                    time,
    const double                    dtime,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth)
  : RayType(ray)
  , m_time(time)
  , m_dtime(dtime)
  , m_flags(flags)
  , m_depth(depth)
{
}

inline ShadingRay::ShadingRay(
    const VectorType&               org,
    const VectorType&               dir,
    const double                    time,
    const double                    dtime,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth)
  : RayType(org, dir)
  , m_time(time)
  , m_dtime(dtime)
  , m_flags(flags)
  , m_depth(depth)
{
}

inline ShadingRay::ShadingRay(
    const VectorType&               org,
    const VectorType&               dir,
    const ValueType                 tmin,
    const ValueType                 tmax,
    const double                    time,
    const double                    dtime,
    const VisibilityFlags::Type     flags,
    const DepthType                 depth)
  : RayType(org, dir, tmin, tmax)
  , m_time(time)
  , m_dtime(dtime)
  , m_flags(flags)
  , m_depth(depth)
{
}

template <typename U>
inline ShadingRay transform_to_local(
    const foundation::Transform<U>& transform,
    const ShadingRay&               ray)
{
    return
        ShadingRay(
            transform.transform_to_local(ray),
            ray.m_time,
            ray.m_dtime,
            ray.m_flags,
            ray.m_depth);
}

template <typename U>
inline ShadingRay transform_to_parent(
    const foundation::Transform<U>& transform,
    const ShadingRay&               ray)
{
    return
        ShadingRay(
            transform.transform_to_parent(ray),
            ray.m_time,
            ray.m_dtime,
            ray.m_flags,
            ray.m_depth);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRAY_H
