
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/modeling/object/iobjectfactory.h"
#include "renderer/modeling/object/object.h"

// appleseed.foundation headers.
#include "foundation/curve/curvebasis.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace foundation    { class SearchPaths; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Curve object (source geometry).
//

class APPLESEED_DLLSYMBOL CurveObject
  : public Object
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying the model of this object.
    const char* get_model() const override;

    // Compute the local space bounding box of the object over the shutter interval.
    GAABB3 compute_local_bbox() const override;

    // Insert and access curve basis.
    void push_basis(const foundation::CurveBasis basis);
    foundation::CurveBasis get_basis() const;

    // Insert and access total number of curves.
    void push_curve_count(const size_t count);
    size_t get_curve_count() const;

    // Insert and access curves.
    void reserve_curves1(const size_t count);
    void reserve_curves3(const size_t count);
    size_t push_curve1(const Curve1Type& curve);
    size_t push_curve3(const Curve3Type& curve);
    size_t get_curve1_count() const;
    size_t get_curve3_count() const;
    const Curve1Type& get_curve1(const size_t index) const;
    const Curve3Type& get_curve3(const size_t index) const;

    // Insert and access material slots.
    size_t get_material_slot_count() const override;
    const char* get_material_slot(const size_t index) const override;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const override;
    void update_asset_paths(const foundation::StringDictionary& mappings) override;

  private:
    friend class CurveObjectFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    CurveObject(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~CurveObject() override;
};


//
// Curve object factory.
//

class APPLESEED_DLLSYMBOL CurveObjectFactory
  : public IObjectFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying this object model.
    const char* get_model() const override;

    // Return metadata for this object model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this object model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new single empty object.
    foundation::auto_release_ptr<Object> create(
        const char*                     name,
        const ParamArray&               params) const override;

    // Create objects, potentially from external assets.
    bool create(
        const char*                     name,
        const ParamArray&               params,
        const foundation::SearchPaths&  search_paths,
        const bool                      omit_loading_assets,
        ObjectArray&                    objects) const override;
};

}   // namespace renderer
