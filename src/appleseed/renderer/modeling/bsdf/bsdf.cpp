
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "bsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/inputevaluator.h"

using namespace foundation;

namespace renderer
{

//
// BSDF class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID BSDF::get_class_uid()
{
    return g_class_uid;
}

const double BSDF::DiracDelta = -1.0;

BSDF::BSDF(
    const char*             name,
    const Type              type,
    const int               modes,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , m_type(type)
  , m_modes(modes)
{
    set_name(name);
}

bool BSDF::on_frame_begin(
    const Project&          project,
    const Assembly&         assembly,
    IAbortSwitch*           abort_switch)
{
    return true;
}

void BSDF::on_frame_end(
    const Project&          project,
    const Assembly&         assembly)
{
}

size_t BSDF::compute_input_data_size(
    const Assembly&         assembly) const
{
    return get_inputs().compute_data_size();
}

void BSDF::evaluate_inputs(
    const ShadingContext&   shading_context,
    InputEvaluator&         input_evaluator,
    const ShadingPoint&     shading_point,
    const size_t            offset) const
{
    input_evaluator.evaluate(get_inputs(), shading_point.get_uv(0), offset);
    prepare_inputs(shading_point, input_evaluator.data() + offset);
}

void BSDF::prepare_inputs(
    const ShadingPoint&     shading_point,
    void*                   data) const
{
}

double BSDF::sample_ior(
    SamplingContext&        sampling_context,
    const void*             data) const
{
    return 1.0;
}

void BSDF::compute_absorption(
    const void*             data,
    const double            distance,
    Spectrum&               absorption) const
{
    absorption.set(1.0f);
}

}   // namespace renderer
