
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
#include "object.h"

// appleseed.renderer headers.
#include "renderer/modeling/input/source.h"

using namespace foundation;

namespace renderer
{

//
// Object::RenderData class implementation.
//

Object::RenderData::RenderData()
{
    clear();
}

void Object::RenderData::clear()
{
    m_alpha_map = nullptr;
}


//
// Object class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Object::get_class_uid()
{
    return g_class_uid;
}

Object::Object(
    const char*         name,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
{
    set_name(name);
}

const Source* Object::get_uncached_alpha_map() const
{
    return nullptr;
}

bool Object::has_alpha_map() const
{
    return get_uncached_alpha_map() != nullptr;
}

bool Object::has_opaque_uniform_alpha_map() const
{
    const Source* source = get_uncached_alpha_map();

    if (!source || !source->is_uniform())
        return false;

    float alpha;
    source->evaluate_uniform(alpha);

    return alpha == 1.0f;
}

bool Object::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!ConnectableEntity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_render_data.clear();
    m_render_data.m_alpha_map = get_uncached_alpha_map();

    return true;
}

void Object::on_frame_end(
    const Project&          project,
    const BaseGroup*        parent)
{
    m_render_data.clear();

    ConnectableEntity::on_frame_end(project, parent);
}

void Object::rasterize(ObjectRasterizer& rasterizer) const
{
}

}   // namespace renderer
