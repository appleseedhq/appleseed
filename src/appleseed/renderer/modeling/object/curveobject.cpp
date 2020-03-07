
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Srinath Ravichandran, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/object/curveobjectreader.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// CurveObject class implementation.
//

namespace
{
    const char* Model = "curve_object";
}

struct CurveObject::Impl
{
    CurveBasis               m_basis;
    size_t                   m_curve_count;
    std::vector<Curve1Type>  m_curves1;
    std::vector<Curve3Type>  m_curves3;
    std::vector<std::string> m_material_slots;

    Impl()
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
    return Model;
}

GAABB3 CurveObject::compute_local_bbox() const
{
    return impl->compute_bounds();
}

void CurveObject::push_basis(const CurveBasis basis)
{
    impl->m_basis = basis;
}

CurveBasis CurveObject::get_basis() const
{
    return impl->m_basis;
}

void CurveObject::push_curve_count(const size_t count)
{
    impl->m_curve_count = count;
}

size_t CurveObject::get_curve_count() const
{
    switch (impl->m_basis)
    {
      case CurveBasis::Linear:
        return get_curve1_count();

      case CurveBasis::Bezier:
      case CurveBasis::BSpline:
      case CurveBasis::CatmullRom:
        return get_curve3_count();

      default:
        assert(!"Invalid curve basis.");
        return 0;
    }
}

void CurveObject::reserve_curves1(const size_t count)
{
    impl->m_curves1.reserve(count);
}

void CurveObject::reserve_curves3(const size_t count)
{
    impl->m_curves3.reserve(count);
}

size_t CurveObject::push_curve1(const Curve1Type& curve)
{
    const size_t index = impl->m_curves1.size();
    impl->m_curves1.push_back(curve);
    return index;
}

size_t CurveObject::push_curve3(const Curve3Type& curve)
{
    const size_t index = impl->m_curves3.size();
    Curve3Type t_curve = curve;

    switch (get_basis())
    {
      case CurveBasis::Bezier:
        // Do nothing.
        break;

      case CurveBasis::BSpline:
        t_curve.transform_basis(
            CurveMatrixType::from_array(BezierInverseBasisArray) * CurveMatrixType::from_array(BSplineBasisArray));
        break;

      case CurveBasis::CatmullRom:
        t_curve.transform_basis(
            CurveMatrixType::from_array(BezierInverseBasisArray) * CurveMatrixType::from_array(CatmullRomBasisArray));
        break;

      assert_otherwise;
    }

    impl->m_curves3.push_back(t_curve);

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

const Curve1Type& CurveObject::get_curve1(const size_t index) const
{
    assert(index < impl->m_curves1.size());
    return impl->m_curves1[index];
}

const Curve3Type& CurveObject::get_curve3(const size_t index) const
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
        const std::string filepath = m_params.get<std::string>("filepath");
        if (!starts_with(filepath, "builtin:"))
            paths.push_back(filepath.c_str());
    }
}

void CurveObject::update_asset_paths(const StringDictionary& mappings)
{
    if (m_params.strings().exist("filepath"))
    {
        const std::string filepath = m_params.get<std::string>("filepath");
        if (!starts_with(filepath, "builtin:"))
            m_params.set("filepath", mappings.get(filepath.c_str()));
    }
}


//
// CurveObjectFactory class implementation.
//

void CurveObjectFactory::release()
{
    delete this;
}

const char* CurveObjectFactory::get_model() const
{
    return Model;
}

Dictionary CurveObjectFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Curve Object");
}

DictionaryArray CurveObjectFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<Object> CurveObjectFactory::create(
    const char*             name,
    const ParamArray&       params) const
{
    return auto_release_ptr<Object>(new CurveObject(name, params));
}

bool CurveObjectFactory::create(
    const char*             name,
    const ParamArray&       params,
    const SearchPaths&      search_paths,
    const bool              omit_loading_assets,
    ObjectArray&            objects) const
{
    objects.push_back(
        omit_loading_assets
            ? create(name, params).release()
            : CurveObjectReader::read(search_paths, name, params).release());

    return true;
}

}   // namespace renderer
