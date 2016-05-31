
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Srinath Ravichandran, The appleseedhq Organization
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
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/string.h"

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
    RegionKit           m_region_kit;
    Lazy<RegionKit>     m_lazy_region_kit;
    vector<CurveType1>  m_curves1;
    vector<CurveType3>  m_curves3;
    vector<string>      m_material_slots;

    Impl()
      : m_lazy_region_kit(&m_region_kit)
    {
    }

    GAABB3 compute_bounds() const
    {
        GAABB3 bbox;
        bbox.invalidate();

        const size_t curve1_count = m_curves1.size();
        const size_t curve3_count = m_curves3.size();

        for (size_t i = 0; i < curve1_count; ++i)
            bbox.insert(m_curves1[i].compute_bbox());

        for (size_t i = 0; i < curve3_count; ++i)
            bbox.insert(m_curves3[i].compute_bbox());

        return bbox;
    }
};

CurveObject::CurveObject(
    const char*         name,
    const ParamArray&   params)
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

void CurveObject::reserve_curves1(const size_t count)
{
    impl->m_curves1.reserve(count);
}

void CurveObject::reserve_curves3(const size_t count)
{
    impl->m_curves3.reserve(count);
}

size_t CurveObject::push_curve1(const CurveType1& curve)
{
    const size_t index = impl->m_curves1.size();
    impl->m_curves1.push_back(curve);
    return index;
}

size_t CurveObject::push_curve3(const CurveType3& curve)
{
    const size_t index = impl->m_curves3.size();
    impl->m_curves3.push_back(curve);
    return index;
}

size_t CurveObject::get_curve1_count() const
{
    return impl->m_curves1.size();
}

size_t CurveObject::get_curve3_count() const
{
    return impl->m_curves3.size();
}

const CurveType1& CurveObject::get_curve1(const size_t index) const
{
    assert(index < impl->m_curves1.size());
    return impl->m_curves1[index];
}

const CurveType3& CurveObject::get_curve3(const size_t index) const
{
    assert(index < impl->m_curves3.size());
    return impl->m_curves3[index];
}

size_t CurveObject::get_material_slot_count() const
{
    return impl->m_material_slots.size();
}

const char* CurveObject::get_material_slot(const size_t index) const
{
    return impl->m_material_slots[index].c_str();
}

void CurveObject::collect_asset_paths(StringArray& paths) const
{
    if (m_params.strings().exist("filepath"))
    {
        const string filepath = m_params.get<string>("filepath");
        if (!starts_with(filepath, "builtin:"))
            paths.push_back(filepath.c_str());
    }
}

void CurveObject::update_asset_paths(const StringDictionary& mappings)
{
    m_params.set("filepath", mappings.get(m_params.get("filepath")));
}


//
// CurveObjectFactory class implementation.
//

const char* CurveObjectFactory::get_model()
{
    return "curve_object";
}

auto_release_ptr<CurveObject> CurveObjectFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<CurveObject>(new CurveObject(name, params));
}

}   // namespace renderer
