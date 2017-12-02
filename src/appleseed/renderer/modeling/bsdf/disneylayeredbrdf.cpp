
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "disneylayeredbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/material/disneymaterial.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/utility/arena.h"

// Standard headers.
#include <cassert>
#include <cstring>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// DisneyLayeredBRDF class implementation.
//

namespace
{
    const char* Model = "disney_layered_brdf";
}

DisneyLayeredBRDF::DisneyLayeredBRDF(const DisneyMaterial* parent)
  : BSDF(
        "disney_layered_brdf",
        Reflective,
        ScatteringMode::Diffuse | ScatteringMode::Glossy,
        ParamArray())
  , m_parent(parent)
  , m_brdf(DisneyBRDFFactory().create("disney_brdf", ParamArray()))
{
    assert(parent);
}

void DisneyLayeredBRDF::release()
{
    delete this;
}

const char* DisneyLayeredBRDF::get_model() const
{
    return Model;
}

bool DisneyLayeredBRDF::on_frame_begin(
    const Project&              project,
    const BaseGroup*            parent,
    OnFrameBeginRecorder&       recorder,
    IAbortSwitch*               abort_switch)
{
    if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    if (!m_brdf->on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    return true;
}

size_t DisneyLayeredBRDF::compute_input_data_size() const
{
    return sizeof(DisneyBRDFInputValues);
}

void* DisneyLayeredBRDF::evaluate_inputs(
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point) const
{
    DisneyBRDFInputValues* values = shading_context.get_arena().allocate<DisneyBRDFInputValues>();
    memset(values, 0, sizeof(DisneyBRDFInputValues));

    Color3f base_color(0.0f);

    for (size_t i = 0, e = m_parent->get_layer_count(); i < e; ++i)
    {
        const DisneyMaterialLayer& layer =
            m_parent->get_layer(i, shading_context.get_thread_index());

        layer.evaluate_expressions(
            shading_point,
            shading_context.get_oiio_texture_system(),
            base_color,
            *values);
    }

    // Colors in SeExpr are always in the sRGB color space.
    // todo: convert colors earlier so that all math is done in linear space.
    values->m_base_color.set(
        srgb_to_linear_rgb(base_color),
        g_std_lighting_conditions,
        Spectrum::Reflectance);

    m_brdf->prepare_inputs(
        shading_context.get_arena(),
        shading_point,
        values);

    return values;
}

void DisneyLayeredBRDF::sample(
    SamplingContext&            sampling_context,
    const void*                 data,
    const bool                  adjoint,
    const bool                  cosine_mult,
    const int                   modes,
    BSDFSample&                 sample) const
{
    if (m_parent->get_layer_count() == 0)
        return;

    m_brdf->sample(
        sampling_context,
        data,
        adjoint,
        cosine_mult,
        modes,
        sample);
}

float DisneyLayeredBRDF::evaluate(
    const void*                 data,
    const bool                  adjoint,
    const bool                  cosine_mult,
    const Vector3f&             geometric_normal,
    const Basis3f&              shading_basis,
    const Vector3f&             outgoing,
    const Vector3f&             incoming,
    const int                   modes,
    DirectShadingComponents&    value) const
{
    if (m_parent->get_layer_count() == 0)
        return 0.0f;

    return m_brdf->evaluate(
        data,
        adjoint,
        cosine_mult,
        geometric_normal,
        shading_basis,
        outgoing,
        incoming,
        modes,
        value);
}

float DisneyLayeredBRDF::evaluate_pdf(
    const void*                 data,
    const bool                  adjoint,
    const Vector3f&             geometric_normal,
    const Basis3f&              shading_basis,
    const Vector3f&             outgoing,
    const Vector3f&             incoming,
    const int                   modes) const
{
    if (m_parent->get_layer_count() == 0)
        return 0.0f;

    return m_brdf->evaluate_pdf(
        data,
        adjoint,
        geometric_normal,
        shading_basis,
        outgoing,
        incoming,
        modes);
}

}   // namespace renderer
