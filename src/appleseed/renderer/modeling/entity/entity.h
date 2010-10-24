
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/utility/version.h"

namespace renderer
{

//
// Base class for all entities in the scene.
//

class RENDERERDLL Entity
  : public foundation::IUnknown
  , public foundation::IIdentifiable
  , public foundation::IVersionable
{
  public:
    // Constructors.
    explicit Entity(
        const foundation::UniqueID  class_uid);
    Entity(
        const foundation::UniqueID  class_uid,
        const ParamArray&           params);

    // Return the unique ID of this class of entities.
    foundation::UniqueID get_class_uid() const;

    // Return the unique ID of this instance.
    virtual foundation::UniqueID get_uid() const;

    // Return the version ID of this instance.
    virtual foundation::VersionID get_version_id() const;

    // Return the name of this instance.
    virtual const char* get_name() const = 0;

    // Return the parameters of this instance.
    ParamArray& get_parameters();
    const ParamArray& get_parameters() const;

  protected:
    const foundation::UniqueID      m_class_uid;
    const foundation::UniqueID      m_uid;
    foundation::VersionID           m_version_id;
    ParamArray                      m_params;
};


//
// Entity class implementation.
//

inline Entity::Entity(
    const foundation::UniqueID      class_uid)
  : m_class_uid(class_uid)
  , m_uid(foundation::new_guid())
  , m_version_id(0)
{
}

inline Entity::Entity(
    const foundation::UniqueID      class_uid,
    const ParamArray&               params)
  : m_class_uid(class_uid)
  , m_uid(foundation::new_guid())
  , m_version_id(0)
  , m_params(params)
{
}

inline foundation::UniqueID Entity::get_class_uid() const
{
    return m_class_uid;
}

inline foundation::UniqueID Entity::get_uid() const
{
    return m_uid;
}

inline foundation::VersionID Entity::get_version_id() const
{
    return m_version_id;
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
