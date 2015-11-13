
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
#include "pathvertex.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"

namespace renderer
{

void PathVertex::compute_emitted_radiance(
    const ShadingContext&   shading_context,
    TextureCache&           texture_cache,
    Spectrum&               radiance) const
{
    assert(m_edf);

    // No radiance if we're too close to the light.
    if (m_shading_point->get_distance() < m_edf->get_light_near_start())
    {
        radiance.set(0.0f);
        return;
    }

#ifdef APPLESEED_WITH_OSL
    if (const ShaderGroup* sg = get_material()->get_render_data().m_shader_group)
        shading_context.execute_osl_emission(*sg, *m_shading_point);
#endif

    // Evaluate the EDF inputs.
    InputEvaluator input_evaluator(texture_cache);
    m_edf->evaluate_inputs(input_evaluator, *m_shading_point);

    // Compute the emitted radiance.
    m_edf->evaluate(
        input_evaluator.data(),
        m_shading_point->get_geometric_normal(),
        m_shading_point->get_shading_basis(),
        m_outgoing.get_value(),
        radiance);
}

}   // namespace renderer
