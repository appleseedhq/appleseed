
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Srinath Ravichandran, The appleseedhq Organization
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
#include "curveobject.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// CurveObject class implementation.
//

struct CurveObject::Impl
{
    RegionKit               m_region_kit;
    Lazy<RegionKit>         m_lazy_region_kit;
    vector<BezierCurve3d>   m_curves;
    vector<string>          m_material_slots;

    Impl()
      : m_lazy_region_kit(&m_region_kit)
    {
    }

    GAABB3 compute_bounds() const
    {
        AABB3d bbox;
        bbox.invalidate();

        const size_t curve_count = m_curves.size();

        for (size_t i = 0; i < curve_count; ++i)
            bbox.insert(m_curves[i].compute_bbox());

        return bbox;
    }
};

CurveObject::CurveObject(
    const char*             name,
    const ParamArray&       params)
  : Object(name, params)
  , impl(new Impl())
{
}

CurveObject::~CurveObject()
{
    delete impl;
}

void CurveObject::release()
{
    delete this;
}

const char* CurveObject::get_model() const
{
    return CurveObjectFactory::get_model();
}

GAABB3 CurveObject::compute_local_bbox() const
{
    return impl->compute_bounds();
}

Lazy<RegionKit>& CurveObject::get_region_kit()
{
    return impl->m_lazy_region_kit;
}

void CurveObject::reserve_curves(const size_t count)
{
    impl->m_curves.reserve(count);
}

size_t CurveObject::push_curve(const foundation::BezierCurve3d& curve)
{
    const size_t index = impl->m_curves.size();
    impl->m_curves.push_back(curve);
    return index;
}

size_t CurveObject::get_curve_count() const
{
    return impl->m_curves.size();
}

const BezierCurve3d& CurveObject::get_curve(const size_t index) const
{
    assert(index < impl->m_curves.size());
    return impl->m_curves[index];
}

size_t CurveObject::get_material_slot_count() const
{
    return impl->m_material_slots.size();
}

const char* CurveObject::get_material_slot(const size_t index) const
{
    return impl->m_material_slots[index].c_str();
}


//
// CurveObjectFactory class implementation.
//

const char* CurveObjectFactory::get_model()
{
    return "curve_object";
}

auto_release_ptr<CurveObject> CurveObjectFactory::create(
    const char*             name,
    const ParamArray&       params)
{
    return
        auto_release_ptr<CurveObject>(
            new CurveObject(name, params));
}

}   // namespace renderer
