
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
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/entity/onrenderbeginrecorder.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Scene; }

namespace renderer
{

//
// Convenient base class for test scenes.
//

class TestSceneBaseProjectHolder
  : public foundation::NonCopyable
{
  public:
    TestSceneBaseProjectHolder();

  protected:
    Project& get_project();

  private:
    foundation::auto_release_ptr<Project> m_project;
};

class TestSceneBase
  : public TestSceneBaseProjectHolder
{
  public:
    Project&    m_project;
    Scene&      m_scene;

    TestSceneBase();

    void create_color_entity(
        const char*                 name,
        const foundation::Color3f&  linear_rgb);

    void create_color_entity(
        const char*                 name,
        const Spectrum&             spectrum);

    void create_texture_instance(
        const char*                 name,
        const char*                 texture_name);
};


//
// Invoke on_render_begin/end() and on_frame_begin/end() hooks on all entities of a scene.
//

class TestSceneContext
{
  public:
    explicit TestSceneContext(TestSceneBase& base);
    ~TestSceneContext();

  private:
    TestSceneBase&          m_base;
    OnRenderBeginRecorder   m_render_begin_recorder;
    OnFrameBeginRecorder    m_frame_begin_recorder;
};

template <typename Base>
class StaticTestSceneContext
  : public Base
{
  public:
    StaticTestSceneContext()
      : m_context(*this)
    {
    }

  private:
    TestSceneContext m_context;
};


//
// A dummy entity that does not do anything.
//

class DummyEntity
  : public Entity
{
  public:
    explicit DummyEntity(const char* name);
    void release() override;
};


//
// An entity holding a flag indicating whether release() was called on it.
//

class DummyEntityReleaseCheck
  : public Entity
{
  public:
    bool& m_release_was_called;

    DummyEntityReleaseCheck(const char* name, bool& release_was_called);
    void release() override;
};


//
// A dummy object entity with just a bounding box.
//

class BoundingBoxObject
  : public Object
{
  public:
    BoundingBoxObject(
        const char*                 name,
        const GAABB3&               bbox);

    void release() override;

    const char* get_model() const override;

    GAABB3 compute_local_bbox() const override;

    size_t get_material_slot_count() const override;
    const char* get_material_slot(const size_t index) const override;

  private:
    GAABB3                          m_bbox;
};

}   // namespace renderer
