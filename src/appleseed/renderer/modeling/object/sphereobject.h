
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/object/iobjectfactory.h"
#include "renderer/modeling/object/proceduralobject.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShadingRay; }

namespace renderer
{

//
// A sphere object.
//
// The sphere is assumed to be centered at the origin.
//

class APPLESEED_DLLSYMBOL SphereObject
  : public ProceduralObject
{
  public:

    void release() override;

    const char* get_model() const override;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch) override;

    GAABB3 compute_local_bbox() const override;

    size_t get_material_slot_count() const override;

    const char* get_material_slot(const size_t index) const override;

    foundation::Vector3d get_uncached_center() const;
    double get_uncached_radius() const;

    void intersect(
        const ShadingRay&           ray,
        IntersectionResult&         result) const override;

    bool intersect(const ShadingRay& ray) const override;

  private:
    friend class SphereObjectFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    SphereObject(
        const char*                 name,
        const ParamArray&           params);
};


//
// Sphere object factory.
//

class APPLESEED_DLLSYMBOL SphereObjectFactory
  : public IObjectFactory
{
  public:
    void release() override;

    const char* get_model() const override;

    foundation::Dictionary get_model_metadata() const override;

    foundation::DictionaryArray get_input_metadata() const override;

    foundation::auto_release_ptr<Object> create(
        const char*                     name,
        const ParamArray&               params) const override;

    bool create(
        const char*                     name,
        const ParamArray&               params,
        const foundation::SearchPaths&  search_paths,
        const bool                      omit_loading_assets,
        ObjectArray&                    objects) const override;
};

}       // namespace renderer
