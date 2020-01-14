
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
#include "light.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// Light class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Light::get_class_uid()
{
    return g_class_uid;
}

struct Light::Impl
{
    Transformd m_transform;

    Impl()
      : m_transform(Transformd::identity())
    {
    }
};

Light::Light(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , impl(new Impl())
  , m_flags(0)
{
    set_name(name);
}

Light::~Light()
{
    delete impl;
}

float Light::get_uncached_importance_multiplier() const
{
    return m_params.get_optional<float>("importance_multiplier", 1.0f);
}

void Light::set_transform(const Transformd& transform)
{
    impl->m_transform = transform;
    bump_version_id();
}

const Transformd& Light::get_transform() const
{
    return impl->m_transform;
}

bool Light::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!ConnectableEntity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    if (m_params.get_optional<bool>("cast_indirect_light", true))
        m_flags |= CastIndirectLight;
    else
        m_flags &= ~CastIndirectLight;

    if (m_params.get_optional<bool>("cast_shadows", true))
        m_flags |= CastShadows;
    else
        m_flags &= ~CastShadows;

    if (get_uncached_importance_multiplier() <= 0.0)
    {
        RENDERER_LOG_WARNING(
            "light \"%s\" has negative or zero importance; expect artifacts and/or slowdowns.",
            get_path().c_str());
    }

    return true;
}

void Light::sample(
    const ShadingContext&   shading_context,
    const Transformd&       light_transform,
    const Vector2d&         s,
    const LightTargetArray& targets,
    Vector3d&               position,
    Vector3d&               outgoing,
    Spectrum&               value,
    float&                  probability) const
{
    // By default we ignore the light targets.
    return
        sample(
            shading_context,
            light_transform,
            s,
            position,
            outgoing,
            value,
            probability);
}

}   // namespace renderer
