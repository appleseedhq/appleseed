
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class ObjectRasterizer; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class Source; }

namespace renderer
{

//
// Object.
//

class APPLESEED_DLLSYMBOL Object
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    Object(
        const char*         name,
        const ParamArray&   params);

    // Return a string identifying the model of this entity.
    // Model here is synonymous with which "kind" of Object this entity is,
    // not an identifier for its actual mesh or curve representation.
    virtual const char* get_model() const = 0;

    // Compute the local space bounding box of the object over the shutter interval.
    virtual GAABB3 compute_local_bbox() const = 0;

    // Access materials slots.
    virtual size_t get_material_slot_count() const = 0;
    virtual const char* get_material_slot(const size_t index) const = 0;

    // Return the source bound to the alpha map input, or nullptr if the object doesn't have an alpha map.
    virtual const Source* get_uncached_alpha_map() const;

    // Return true if this object has an alpha map.
    bool has_alpha_map() const;

    // Return true if this object has an uniform alpha value equals to 1.0f.
    bool has_opaque_uniform_alpha_map() const;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) override;

    struct APPLESEED_DLLSYMBOL RenderData
    {
        const Source* m_alpha_map;

        RenderData();

        void clear();
    };

    // Return render-time data of this entity.
    // Render-time data are available between on_frame_begin() and on_frame_end() calls.
    const RenderData& get_render_data() const;

    // Send this object to an object rasterizer.
    virtual void rasterize(ObjectRasterizer& rasterizer) const;

  private:
    RenderData m_render_data;
};


//
// Object class implementation.
//

inline const Object::RenderData& Object::get_render_data() const
{
    return m_render_data;
}

}   // namespace renderer
