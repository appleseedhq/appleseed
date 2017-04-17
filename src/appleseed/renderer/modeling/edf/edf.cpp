
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
using namespace std;

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

    m_max_contribution = get_uncached_max_contribution();

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

float EDF::get_max_contribution_scalar(const Source* source) const
{
    assert(source);

    if (!source->is_uniform())
        return numeric_limits<float>::max();

    float value;
    source->evaluate_uniform(value);

    return value;
}

float EDF::get_max_contribution_spectrum(const Source* source) const
{
    assert(source);

    if (!source->is_uniform())
        return numeric_limits<float>::max();

    Spectrum spectrum;
    source->evaluate_uniform(spectrum);

    return max_value(spectrum);
}

float EDF::get_max_contribution(const Source* input, const Source* multiplier) const
{
    const float max_contribution_input = get_max_contribution_spectrum(input);

    if (max_contribution_input == numeric_limits<float>::max())
        return numeric_limits<float>::max();

    const float max_contribution_multiplier = get_max_contribution_scalar(multiplier);

    if (max_contribution_multiplier == numeric_limits<float>::max())
        return numeric_limits<float>::max();

    return max_contribution_input * max_contribution_multiplier;
}

float EDF::get_max_contribution(const char* input_name, const char* multiplier_name) const
{
    return
        get_max_contribution(
            m_inputs.source(input_name),
            m_inputs.source(multiplier_name));
}

}   // namespace renderer
