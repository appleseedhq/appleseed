
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

// Interface header.
#include "testutils.h"

// appleseed.renderer headers.
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

//
// TestSceneBaseProjectHolder class implementation.
//

TestSceneBaseProjectHolder::TestSceneBaseProjectHolder()
  : m_project(ProjectFactory::create("project"))
{
    m_project->set_scene(SceneFactory::create());
};

Project& TestSceneBaseProjectHolder::get_project()
{
    return m_project.ref();
}


//
// TestSceneBase class implementation.
//

TestSceneBase::TestSceneBase()
  : m_project(get_project())
  , m_scene(*get_project().get_scene())
{
}

void TestSceneBase::create_color_entity(const char* name, const Color3f& linear_rgb)
{
    ParamArray params;
    params.insert("color_space", "linear_rgb");

    const ColorValueArray values(3, &linear_rgb[0]);

    m_scene.colors().insert(
        ColorEntityFactory::create(name, params, values));
}

void TestSceneBase::create_color_entity(const char* name, const Spectrum& spectrum)
{
    ParamArray params;
    params.insert("color_space", "spectral");
    params.insert("wavelength_range", "400.0 700.0");

    const ColorValueArray values(spectrum.Samples, &spectrum[0]);

    m_scene.colors().insert(
        ColorEntityFactory::create(name, params, values));
}

void TestSceneBase::create_texture_instance(const char* name, const char* texture_name)
{
    ParamArray params;
    params.insert("addressing_mode", "clamp");
    params.insert("filtering_mode", "bilinear");

    m_scene.texture_instances().insert(
        TextureInstanceFactory::create(
            name,
            params,
            texture_name,
            Transformf::identity()));
}


//
// TestSceneContext class implementation.
//

TestSceneContext::TestSceneContext(TestSceneBase& base)
  : m_base(base)
{
    InputBinder input_binder(m_base.m_scene);
    input_binder.bind();
    assert(input_binder.get_error_count() == 0);

#ifndef NDEBUG
    bool success =
#endif
        m_base.m_scene.on_render_begin(m_base.m_project, nullptr, m_render_begin_recorder);
    assert(success);

#ifndef NDEBUG
    success =
#endif
        m_base.m_scene.on_frame_begin(m_base.m_project, nullptr, m_frame_begin_recorder);
    assert(success);
}

TestSceneContext::~TestSceneContext()
{
    m_frame_begin_recorder.on_frame_end(m_base.m_project);
    m_render_begin_recorder.on_render_end(m_base.m_project);
}


//
// DummyEntity class implementation.
//

DummyEntity::DummyEntity(const char* name)
  : Entity(0)
{
    set_name(name);
}

void DummyEntity::release()
{
    delete this;
}


//
// DummyEntityReleaseCheck class implementation.
//

DummyEntityReleaseCheck::DummyEntityReleaseCheck(
    const char* name,
    bool&       release_was_called)
  : Entity(0)
  , m_release_was_called(release_was_called)
{
    set_name(name);
}

void DummyEntityReleaseCheck::release()
{
    m_release_was_called = true;
    delete this;
}


//
// BoundingBoxObject class implementation.
//

BoundingBoxObject::BoundingBoxObject(const char* name, const GAABB3& bbox)
  : Object(name, ParamArray())
  , m_bbox(bbox)
{
}

void BoundingBoxObject::release()
{
    delete this;
}

const char* BoundingBoxObject::get_model() const
{
    return "boundingbox_object";
}

GAABB3 BoundingBoxObject::compute_local_bbox() const
{
    return m_bbox;
}

size_t BoundingBoxObject::get_material_slot_count() const
{
    return 0;
}

const char* BoundingBoxObject::get_material_slot(const size_t index) const
{
    return nullptr;
}

}   // namespace renderer
