
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/apiarray.h"

namespace renderer
{

//
// A volume of interest as a hint to Light::sample().
//

class LightTarget
{
  public:
    // Constructors.
    LightTarget();
    explicit LightTarget(const foundation::AABB3d& bbox);

    // Comparison operators.
    bool operator==(const LightTarget& rhs) const;
    bool operator!=(const LightTarget& rhs) const;

    // Getters.
    const foundation::AABB3d& get_bbox() const;
    const foundation::Vector3d& get_center() const;
    double get_radius() const;

  private:
    foundation::AABB3d      m_bbox;     // world space
    foundation::Vector3d    m_center;   // world space
    double                  m_radius;   // world space
};

APPLESEED_DECLARE_APIARRAY(LightTargetArray, LightTarget);


//
// LightTarget class implementation.
//

inline LightTarget::LightTarget()
{
}

inline LightTarget::LightTarget(const foundation::AABB3d& bbox)
  : m_bbox(bbox)
  , m_center(bbox.center())
  , m_radius(bbox.radius())
{
}

inline bool LightTarget::operator==(const LightTarget& rhs) const
{
    return m_bbox == rhs.m_bbox;
}

inline bool LightTarget::operator!=(const LightTarget& rhs) const
{
    return m_bbox != rhs.m_bbox;
}

inline const foundation::AABB3d& LightTarget::get_bbox() const
{
    return m_bbox;
}

inline const foundation::Vector3d& LightTarget::get_center() const
{
    return m_center;
}

inline double LightTarget::get_radius() const
{
    return m_radius;
}

}   // namespace renderer
