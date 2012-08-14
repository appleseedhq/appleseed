
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "assembly.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Assembly class implementation.
//

struct Assembly::Impl
{
    ColorContainer              m_colors;
    TextureContainer            m_textures;
    TextureInstanceContainer    m_texture_instances;
    BSDFContainer               m_bsdfs;
    EDFContainer                m_edfs;
    SurfaceShaderContainer      m_surface_shaders;
    MaterialContainer           m_materials;
    LightContainer              m_lights;
    ObjectContainer             m_objects;
    ObjectInstanceContainer     m_object_instances;
};

namespace
{
    const UniqueID g_class_uid = new_guid();
}

Assembly::Assembly(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    m_flushable = m_params.get_optional<bool>("flushable", false);
}

Assembly::~Assembly()
{
    delete impl;
}

void Assembly::release()
{
    delete this;
}

ColorContainer& Assembly::colors() const
{
    return impl->m_colors;
}

TextureContainer& Assembly::textures() const
{
    return impl->m_textures;
}

TextureInstanceContainer& Assembly::texture_instances() const
{
    return impl->m_texture_instances;
}

BSDFContainer& Assembly::bsdfs() const
{
    return impl->m_bsdfs;
}

EDFContainer& Assembly::edfs() const
{
    return impl->m_edfs;
}

SurfaceShaderContainer& Assembly::surface_shaders() const
{
    return impl->m_surface_shaders;
}

MaterialContainer& Assembly::materials() const
{
    return impl->m_materials;
}

LightContainer& Assembly::lights() const
{
    return impl->m_lights;
}

ObjectContainer& Assembly::objects() const
{
    return impl->m_objects;
}

ObjectInstanceContainer& Assembly::object_instances() const
{
    return impl->m_object_instances;
}

namespace
{
    template <typename EntityCollection>
    bool invoke_on_frame_begin(
        const Project&          project,
        const Assembly&         assembly,
        EntityCollection&       entities)
    {
        bool success = true;

        for (each<EntityCollection> i = entities; i; ++i)
            success = success && i->on_frame_begin(project, assembly);

        return success;
    }

    template <typename EntityCollection>
    void invoke_on_frame_end(
        const Project&          project,
        const Assembly&         assembly,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->on_frame_end(project, assembly);
    }
}

bool Assembly::on_frame_begin(const Project& project)
{
    bool success = true;

    success = success && invoke_on_frame_begin(project, *this, surface_shaders());
    success = success && invoke_on_frame_begin(project, *this, bsdfs());
    success = success && invoke_on_frame_begin(project, *this, edfs());
    success = success && invoke_on_frame_begin(project, *this, materials());
    success = success && invoke_on_frame_begin(project, *this, lights());

    return success;
}

void Assembly::on_frame_end(const Project& project)
{
    invoke_on_frame_end(project, *this, lights());
    invoke_on_frame_end(project, *this, materials());
    invoke_on_frame_end(project, *this, edfs());
    invoke_on_frame_end(project, *this, bsdfs());
    invoke_on_frame_end(project, *this, surface_shaders());
}


//
// AssemblyFactory class implementation.
//

auto_release_ptr<Assembly> AssemblyFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Assembly>(new Assembly(name, params));
}

}   // namespace renderer
