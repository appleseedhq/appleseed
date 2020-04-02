
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
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"
#include "foundation/hash/siphash.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/uid.h"
#include "foundation/utility/version.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <string>

// Forward declarations.
namespace foundation    { class APIString; }
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class OnRenderBeginRecorder; }
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
    virtual std::uint64_t compute_signature() const;

    // Combine two entity signatures.
    static std::uint64_t combine_signatures(
        const std::uint64_t             s1,
        const std::uint64_t             s2);

    // Set/get the parent of this entity.
    void set_parent(Entity* parent);
    Entity* get_parent() const;

    // Set/get the name of this entity.
    void set_name(const char* name);
    const char* get_name() const;

    // Return the name of the entity as a pointer to a OIIO::ustring for OSL.
    const void* get_name_as_ustring() const;

    // Get the full path from the scene entity to this entity in a human-readable format.
    foundation::APIString get_path() const;

    // Return the parameters of this entity.
    ParamArray& get_parameters();
    const ParamArray& get_parameters() const;

    // Expose asset file paths referenced by this entity to the outside.
    virtual void collect_asset_paths(foundation::StringArray& paths) const;
    virtual void update_asset_paths(const foundation::StringDictionary& mappings);

    //
    // Rendering proceeds roughly as follow, both in final and interactive modes:
    //
    //     Loop:
    //         [Bind scene entities inputs]
    //         on_render_begin()
    //
    //         Loop:
    //             [Retrieve camera position]
    //             on_frame_begin()
    //             [Render frame]
    //             on_frame_end()
    //         Until an entity is edited
    //
    //         on_render_end()
    //     Until rendering ends naturally or is aborted
    //
    // See the `MasterRenderer` class in masterrenderer.cpp for details.
    //

    // This method is called before rendering begins, and whenever rendering is reinitialized
    // (i.e. because an entity has been edited). At this point, all entities inputs are bound.
    // Returns true on success, or false if an error occurred or if the abort switch was triggered.
    virtual bool on_render_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnRenderBeginRecorder&          recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr);

    // This method is called after rendering has ended. It is guaranteed to be called if
    // `on_render_begin()` was called and was successful (i.e. returned true).
    virtual void on_render_end(
        const Project&                  project,
        const BaseGroup*                parent);

    // This method is called before rendering a frame begins, and whenever rendering is restarted
    // (i.e. because the camera has been moved). At this point, all entities inputs are bound.
    // Returns true on success, or false if an error occurred or if the abort switch was triggered.
    virtual bool on_frame_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr);

    // This method is called after rendering a frame has ended. It is guaranteed to be called if
    // `on_frame_begin()` was called and was successful (i.e. returned true).
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
    ~Entity() override;
};

// Utility function to invoke collect_asset_paths() on a collection of entities.
template <typename EntityCollection>
void invoke_collect_asset_paths(
    const EntityCollection&             entities,
    foundation::StringArray&            paths);

// Utility function to invoke update_asset_paths() on a collection of entities.
template <typename EntityCollection>
void invoke_update_asset_paths(
    EntityCollection&                   entities,
    const foundation::StringDictionary& mappings);

// Utility function to invoke on_render_begin() on a collection of entities.
// Returns true on success, or false if an error occurred or if the abort switch was triggered.
template <typename EntityCollection>
bool invoke_on_render_begin(
    EntityCollection&                   entities,
    const Project&                      project,
    const BaseGroup*                    parent,
    OnRenderBeginRecorder&              recorder,
    foundation::IAbortSwitch*           abort_switch);

// Utility function to invoke on_frame_begin() on a collection of entities.
// Returns true on success, or false if an error occurred or if the abort switch was triggered.
template <typename EntityCollection>
bool invoke_on_frame_begin(
    EntityCollection&                   entities,
    const Project&                      project,
    const BaseGroup*                    parent,
    OnFrameBeginRecorder&               recorder,
    foundation::IAbortSwitch*           abort_switch);


//
// Entity class implementation.
//

inline foundation::UniqueID Entity::get_class_uid() const
{
    return m_class_uid;
}

inline std::uint64_t Entity::compute_signature() const
{
    return foundation::siphash24(get_uid(), get_version_id());
}

inline std::uint64_t Entity::combine_signatures(
    const std::uint64_t                 s1,
    const std::uint64_t                 s2)
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

template <typename EntityCollection>
void invoke_collect_asset_paths(
    const EntityCollection&             entities,
    foundation::StringArray&            paths)
{
    for (const auto& entity : entities)
        entity.collect_asset_paths(paths);
}

template <typename EntityCollection>
void invoke_update_asset_paths(
    EntityCollection&                   entities,
    const foundation::StringDictionary& mappings)
{
    for (auto& entity : entities)
        entity.update_asset_paths(mappings);
}

template <typename EntityCollection>
bool invoke_on_render_begin(
    EntityCollection&                   entities,
    const Project&                      project,
    const BaseGroup*                    parent,
    OnRenderBeginRecorder&              recorder,
    foundation::IAbortSwitch*           abort_switch)
{
    for (auto& entity : entities)
    {
        if (foundation::is_aborted(abort_switch))
            return false;

        if (!entity.on_render_begin(project, parent, recorder, abort_switch))
            return false;
    }

    return true;
}

template <typename EntityCollection>
bool invoke_on_frame_begin(
    EntityCollection&                   entities,
    const Project&                      project,
    const BaseGroup*                    parent,
    OnFrameBeginRecorder&               recorder,
    foundation::IAbortSwitch*           abort_switch)
{
    for (auto& entity : entities)
    {
        if (foundation::is_aborted(abort_switch))
            return false;

        if (!entity.on_frame_begin(project, parent, recorder, abort_switch))
            return false;
    }

    return true;
}

}   // namespace renderer
