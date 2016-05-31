
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_UTILITY_TESTUTILS_H
#define APPLESEED_RENDERER_UTILITY_TESTUTILS_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/lazy.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Scene; }

namespace renderer
{

class TestFixtureBaseProjectHolder
  : public foundation::NonCopyable
{
  public:
    TestFixtureBaseProjectHolder();

    Project& get_project();

  private:
    foundation::auto_release_ptr<Project> m_project;
};

class TestFixtureBase
  : public TestFixtureBaseProjectHolder
{
  public:
    Project&    m_project;
    Scene&      m_scene;
    Assembly&   m_assembly;

    TestFixtureBase();

    void create_color_entity(
        const char*                 name,
        const foundation::Color3f&  linear_rgb);

    void create_color_entity(
        const char*                 name,
        const Spectrum&             spectrum);

    void create_texture_instance(
        const char*                 name,
        const char*                 texture_name);

    void bind_inputs();
};

template <typename Scene>
class BindInputs
  : public Scene
{
  public:
    BindInputs()
    {
        InputBinder input_binder;
        input_binder.bind(*Scene::m_scene);
        assert(input_binder.get_error_count() == 0);
    }
};

class DummyEntity
  : public Entity
{
  public:
    explicit DummyEntity(const char* name);
    virtual void release() APPLESEED_OVERRIDE;
};

class DummyEntityReleaseCheck
  : public Entity
{
  public:
    bool& m_release_was_called;

    DummyEntityReleaseCheck(const char* name, bool& release_was_called);
    virtual void release() APPLESEED_OVERRIDE;
};

class BoundingBoxObject
  : public Object
{
  public:
    BoundingBoxObject(
        const char*                 name,
        const GAABB3&               bbox);

    virtual void release() APPLESEED_OVERRIDE;

    virtual const char* get_model() const APPLESEED_OVERRIDE;

    virtual GAABB3 compute_local_bbox() const APPLESEED_OVERRIDE;

    virtual foundation::Lazy<RegionKit>& get_region_kit() APPLESEED_OVERRIDE;

    virtual size_t get_material_slot_count() const APPLESEED_OVERRIDE;
    virtual const char* get_material_slot(const size_t index) const APPLESEED_OVERRIDE;

  private:
    GAABB3                          m_bbox;
    RegionKit                       m_region_kit;
    foundation::Lazy<RegionKit>     m_lazy_region_kit;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_UTILITY_TESTUTILS_H
