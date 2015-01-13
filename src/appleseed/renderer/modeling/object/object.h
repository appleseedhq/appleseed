
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_OBJECT_OBJECT_H
#define APPLESEED_RENDERER_MODELING_OBJECT_OBJECT_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/modeling/object/regionkit.h"

// appleseed.foundation headers.
#include "foundation/utility/lazy.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
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
    virtual const char* get_model() const = 0;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        foundation::IAbortSwitch*   abort_switch = 0);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(const Project& project);

    // Compute the local space bounding box of the object over the shutter interval.
    virtual GAABB3 compute_local_bbox() const = 0;

    // Return the region kit of the object.
    virtual foundation::Lazy<RegionKit>& get_region_kit() = 0;

    // Access materials slots.
    virtual size_t get_material_slot_count() const = 0;
    virtual const char* get_material_slot(const size_t index) const = 0;

    // Return true if this object has an alpha map.
    virtual bool has_alpha_map() const;

    // Return the source bound to the alpha map input, or 0 if the object doesn't have an alpha map.
    const Source* get_alpha_map() const;
    virtual const Source* get_uncached_alpha_map() const;

    // Return whether surface shaders should be invoked for fully transparent shading points.
    bool shade_alpha_cutouts() const;

  protected:
    const Source* m_alpha_map;
    bool          m_shade_alpha_cutouts;
};


//
// Object class implementation.
//

inline const Source* Object::get_alpha_map() const
{
    return m_alpha_map;
}

inline bool Object::shade_alpha_cutouts() const
{
    return m_shade_alpha_cutouts;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_OBJECT_OBJECT_H
