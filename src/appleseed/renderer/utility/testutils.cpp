
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

// Interface header.
#include "testutils.h"

// appleseed.renderer headers.
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"

// Standard headers.
#include <cstdio>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// TestFixtureBaseProjectHolder class implementation.
//

TestFixtureBaseProjectHolder::TestFixtureBaseProjectHolder()
  : m_project(ProjectFactory::create("project"))
{
    m_project->set_scene(SceneFactory::create());

    m_project->get_scene()->assemblies().insert(
        AssemblyFactory::create("assembly", ParamArray()));
};

Project& TestFixtureBaseProjectHolder::get_project()
{
    return m_project.ref();
}


//
// TestFixtureBase class implementation.
//

TestFixtureBase::TestFixtureBase()
  : m_project(get_project())
  , m_scene(*get_project().get_scene())
  , m_assembly(*get_project().get_scene()->assemblies().get("assembly"))
{
}

void TestFixtureBase::create_color_entity(const char* name, const Color3f& linear_rgb)
{
    ParamArray params;
    params.insert("color_space", "linear_rgb");

    const ColorValueArray values(3, &linear_rgb[0]);

    m_scene.colors().insert(
        ColorEntityFactory::create(name, params, values));
}

void TestFixtureBase::create_color_entity(const char* name, const Spectrum& spectrum)
{
    ParamArray params;
    params.insert("color_space", "spectral");

    const ColorValueArray values(spectrum.Samples, &spectrum[0]);

    m_scene.colors().insert(
        ColorEntityFactory::create(name, params, values));
}

void TestFixtureBase::create_texture_instance(const char* name, const size_t texture_index)
{
    ParamArray params;
    params.insert("addressing_mode", "clamp");
    params.insert("filtering_mode", "bilinear");

    m_scene.texture_instances().insert(
        TextureInstanceFactory::create(name, params, texture_index));
}

void TestFixtureBase::bind_inputs()
{
    InputBinder input_binder;
    input_binder.bind(m_scene);
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
// DummyEntityFactory class implementation.
//

auto_release_ptr<DummyEntity> DummyEntityFactory::create(const char* name)
{
    return auto_release_ptr<DummyEntity>(new DummyEntity(name));
}


//
// RAW image I/O function implementation.
//

auto_ptr<Image> load_raw_image(
    const string&   filename,
    const size_t    width,
    const size_t    height)
{
    auto_ptr<Image> image(
        new Image(
            width,
            height,
            width,
            height,
            3,
            PixelFormatUInt8));

    FILE* file = fopen(filename.c_str(), "rb");

    if (file == 0)
        return auto_ptr<Image>(0);

    const size_t pixel_count = width * height;
    const size_t read = fread(image->pixel(0, 0), 3, pixel_count, file);

    fclose(file);

    return read == pixel_count ? image : auto_ptr<Image>(0);
}

bool save_raw_image(
    const string&   filename,
    const Image&    image)
{
    FILE* file = fopen(filename.c_str(), "wb");

    if (file == 0)
        return false;

    const CanvasProperties& props = image.properties();
    const size_t written = fwrite(image.pixel(0, 0), 3, props.m_pixel_count, file);

    fclose(file);

    return written == props.m_pixel_count;
}

}   // namespace renderer
