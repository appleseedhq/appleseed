
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"

// Forward declarations.
namespace foundation    { class Image; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }
namespace renderer      { class Scene; }

namespace renderer
{

class TestFixtureBaseProjectHolder
  : public foundation::NonCopyable
{
  public:
    TestFixtureBaseProjectHolder();

    Project& get_project();

  private:
    foundation::auto_release_ptr<Project>   m_project;
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
        const size_t                texture_index);

    void bind_inputs();
};

class RENDERERDLL DummyEntity
  : public Entity
{
  public:
    virtual void release();

  private:
    friend class DummyEntityFactory;

    explicit DummyEntity(const char* name);
};

class RENDERERDLL DummyEntityFactory
{
  public:
    static foundation::auto_release_ptr<DummyEntity> create(const char* name);
};

std::auto_ptr<foundation::Image> load_raw_image(
    const std::string&  filename,
    const size_t        width,
    const size_t        height);

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_UTILITY_TESTUTILS_H
