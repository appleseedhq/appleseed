
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
#include "bsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/sourceinputs.h"

// appleseed.foundation headers.
#include "foundation/memory/arena.h"

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

const float BSDF::DiracDelta = -1.0f;

BSDF::BSDF(
    const char*                 name,
    const Type                  type,
    const int                   modes,
    const ParamArray&           params)
  : ConnectableEntity(g_class_uid, params)
  , m_type(type)
  , m_modes(modes)
{
    set_name(name);
}

size_t BSDF::compute_input_data_size() const
{
    return get_inputs().compute_data_size();
}

void* BSDF::evaluate_inputs(
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point) const
{
    void* data = shading_context.get_arena().allocate(compute_input_data_size());

    get_inputs().evaluate(
        shading_context.get_texture_cache(),
        SourceInputs(shading_point.get_uv(0)),
        data);

    prepare_inputs(
        shading_context.get_arena(),
        shading_point,
        data);

    return data;
}

void BSDF::prepare_inputs(
    Arena&                      arena,
    const ShadingPoint&         shading_point,
    void*                       data) const
{
}

float BSDF::sample_ior(
    SamplingContext&            sampling_context,
    const void*                 data) const
{
    return 1.0f;
}

void BSDF::compute_absorption(
    const void*                 data,
    const float                 distance,
    Spectrum&                   absorption) const
{
    absorption.set(1.0f);
}

void BSDF::attenuate_substrate(
    const void*                 data,
    const Basis3f&              shading_basis,
    const Vector3f&             outgoing,
    const Vector3f&             incoming,
    Spectrum&                   value) const
{
    assert(false);
}

void BSDF::attenuate_substrate(
    const void*                 data,
    const Basis3f&              shading_basis,
    const Vector3f&             outgoing,
    const Vector3f&             incoming,
    DirectShadingComponents&    value) const
{
    assert(!"This BSDF does not support layering.");
}

void BSDF::attenuate_emission(
    const void*                 data,
    const Basis3f&              shading_basis,
    const Vector3f&             outgoing,
    Spectrum&                   value) const
{
    assert(!"This BSDF does not support layering.");
}

}   // namespace renderer
