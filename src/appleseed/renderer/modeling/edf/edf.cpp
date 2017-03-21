
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "edf.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/arena.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// EDF class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID EDF::get_class_uid()
{
    return g_class_uid;
}

EDF::EDF(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , m_flags(0)
  , m_light_near_start(0.0)
{
    set_name(name);
}

float EDF::get_uncached_importance_multiplier() const
{
    return m_params.get_optional<float>("importance_multiplier", 1.0f);
}

double EDF::get_uncached_light_near_start() const
{
    return m_params.get_optional<double>("light_near_start", 0.0);
}

float EDF::get_uncached_max_radiance_value() const
{
    Spectrum radiance;

    Source *source = m_inputs.source("radiance");

    assert(source);

    source->evaluate_uniform(radiance);

    if (radiance.size() == 3)
        radiance.resize(3);

    float radiance_multiplier = m_params.get_optional<float>("radiance_multiplier", 1.0);

    // Return max component value of the final radiance.
    return radiance_multiplier * max_value(radiance);
}

bool EDF::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!ConnectableEntity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_flags = 0;

    if (m_params.get_optional<bool>("cast_indirect_light", true))
        m_flags |= CastIndirectLight;

    m_light_near_start = get_uncached_light_near_start();

    if (m_light_near_start < 0.0)
    {
        RENDERER_LOG_WARNING(
            "edf \"%s\" has a negative light near start value; expect artifacts and/or slowdowns.",
            get_path().c_str());
    }

    m_max_radiance_value = get_uncached_max_radiance_value();

    if (get_uncached_importance_multiplier() <= 0.0f)
    {
        RENDERER_LOG_WARNING(
            "edf \"%s\" has negative or zero importance; expect artifacts and/or slowdowns.",
            get_path().c_str());
    }

    return true;
}

void* EDF::evaluate_inputs(
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point) const
{
    void* data = shading_context.get_arena().allocate(get_inputs().compute_data_size());

    get_inputs().evaluate(
        shading_context.get_texture_cache(),
        shading_point.get_uv(0),
        data);

    return data;
}

}   // namespace renderer
