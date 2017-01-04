
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Srinath Ravichandran, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_OBJECT_CURVEOBJECT_H
#define APPLESEED_RENDERER_MODELING_OBJECT_CURVEOBJECT_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/lazy.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
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
    virtual void release() APPLESEED_OVERRIDE;

    // Return a string identifying the model of this object.
    virtual const char* get_model() const APPLESEED_OVERRIDE;

    // Compute the local space bounding box of the object over the shutter interval.
    virtual GAABB3 compute_local_bbox() const APPLESEED_OVERRIDE;

    // Return the region kit of the object.
    virtual foundation::Lazy<RegionKit>& get_region_kit() APPLESEED_OVERRIDE;

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
    virtual size_t get_material_slot_count() const APPLESEED_OVERRIDE;
    virtual const char* get_material_slot(const size_t index) const APPLESEED_OVERRIDE;

    // Expose asset file paths referenced by this entity to the outside.
    virtual void collect_asset_paths(foundation::StringArray& paths) const APPLESEED_OVERRIDE;
    virtual void update_asset_paths(const foundation::StringDictionary& mappings) APPLESEED_OVERRIDE;

  private:
    friend class CurveObjectFactory;

    struct Impl;
    Impl*  impl;

    // Constructor.
    CurveObject(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~CurveObject();
};


//
// Curve object factory.
//

class APPLESEED_DLLSYMBOL CurveObjectFactory
{
  public:
    // Return a string identifying this object model.
    static const char* get_model();

    // Create a new curve object.
    static foundation::auto_release_ptr<CurveObject> create(
        const char*         name,
        const ParamArray&   params);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_OBJECT_CURVEOBJECT_H
