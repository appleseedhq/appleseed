
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_ENTITY_ENTITY_H
#define APPLESEED_RENDERER_MODELING_ENTITY_ENTITY_H

// appleseed.renderer headers.
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"
#include "foundation/platform/types.h"
#include "foundation/utility/siphash.h"
#include "foundation/utility/uid.h"
#include "foundation/utility/version.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class APIString; }
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class Project; }

namespace renderer
{

//
// Base class for all entities in the scene.
//

class APPLESEED_DLLSYMBOL Entity
  : public foundation::Identifiable
  , public foundation::Versionable
  , public foundation::IUnknown
{
  public:
    // Constructors.
    explicit Entity(
        const foundation::UniqueID      class_uid);
    Entity(
        const foundation::UniqueID      class_uid,
        Entity*                         parent);
    Entity(
        const foundation::UniqueID      class_uid,
        const ParamArray&               params);
    Entity(
        const foundation::UniqueID      class_uid,
        Entity*                         parent,
        const ParamArray&               params);

    // Return the unique ID of this class of entities.
    foundation::UniqueID get_class_uid() const;

    // Compute and return the unique signature of this entity instance.
    virtual foundation::uint64 compute_signature() const;

    // Combine two entity signatures.
    static foundation::uint64 combine_signatures(
        const foundation::uint64        s1,
        const foundation::uint64        s2);

    // Set/get the parent of this entity.
    void set_parent(Entity* parent);
    Entity* get_parent() const;

    // Set/get the name of this entity.
    void set_name(const char* name);
    const char* get_name() const;

    // Get the full path from the scene entity to this entity in a human-readable format.
    foundation::APIString get_path() const;

    // Return the parameters of this entity.
    ParamArray& get_parameters();
    const ParamArray& get_parameters() const;

    // Expose asset file paths referenced by this entity to the outside.
    virtual void collect_asset_paths(foundation::StringArray& paths) const;
    virtual void update_asset_paths(const foundation::StringDictionary& mappings);

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = 0);

    // This method is called once after rendering each frame (only if on_frame_begin() was called).
    virtual void on_frame_end(
        const Project&                  project,
        const BaseGroup*                parent);

  protected:
    struct Impl;
    Impl* impl;

    const foundation::UniqueID          m_class_uid;
    Entity*                             m_parent;
    ParamArray                          m_params;

    // Destructor.
    ~Entity();
};


//
// Entity class implementation.
//

inline foundation::UniqueID Entity::get_class_uid() const
{
    return m_class_uid;
}

inline foundation::uint64 Entity::compute_signature() const
{
    return foundation::siphash24(get_uid(), get_version_id());
}

inline foundation::uint64 Entity::combine_signatures(
    const foundation::uint64            s1,
    const foundation::uint64            s2)
{
    return foundation::siphash24(s1, s2);
}

inline void Entity::set_parent(Entity* parent)
{
    m_parent = parent;
}

inline Entity* Entity::get_parent() const
{
    return m_parent;
}

inline ParamArray& Entity::get_parameters()
{
    return m_params;
}

inline const ParamArray& Entity::get_parameters() const
{
    return m_params;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENTITY_ENTITY_H
