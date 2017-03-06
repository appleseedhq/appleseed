
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Francois Beaune, The appleseedhq Organization
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
#include "bssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/arena.h"

using namespace foundation;

namespace renderer
{

//
// BSSRDF class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID BSSRDF::get_class_uid()
{
    return g_class_uid;
}

BSSRDF::BSSRDF(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
{
    set_name(name);
}

size_t BSSRDF::compute_input_data_size() const
{
    return get_inputs().compute_data_size();
}

void* BSSRDF::evaluate_inputs(
    const ShadingContext&   shading_context,
    const ShadingPoint&     shading_point) const
{
    void* data = shading_context.get_arena().allocate(compute_input_data_size());

    get_inputs().evaluate(
        shading_context.get_texture_cache(),
        shading_point.get_uv(0),
        data);

    prepare_inputs(
        shading_context.get_arena(),
        shading_point,
        data);

    return data;
}

void BSSRDF::prepare_inputs(
    Arena&                  arena,
    const ShadingPoint&     shading_point,
    void*                   data) const
{
}

void BSSRDF::make_reflectance_and_mfp_compatible(
    Spectrum&               reflectance,
    const Spectrum&         mfp)
{
    if (reflectance.size() != mfp.size())
    {
        // Since it does not really make sense to convert a mfp, a per channel distance,
        // as if it were a color, we instead always convert the reflectance to match the
        // size of the mfp.
        if (mfp.is_spectral())
            Spectrum::upgrade(reflectance, reflectance);
        else Spectrum::downgrade(g_std_lighting_conditions, reflectance, reflectance);
    }
}

float BSSRDF::compute_eta(
    const ShadingPoint&     shading_point,
    const float             ior)
{
    const float outside_ior =
        shading_point.is_entering()
            ? shading_point.get_ray().get_current_ior()
            : shading_point.get_ray().get_previous_ior();

    return outside_ior / ior;
}

void BSSRDF::build_cdf_and_pdf(
    const Spectrum&         src,
    Spectrum&               cdf,
    Spectrum&               pdf)
{
    pdf.resize(src.size());
    cdf.resize(src.size());

    float cumulated_pdf = 0.0f;
    for (size_t i = 0, e = src.size(); i < e; ++i)
    {
        pdf[i] = src[i];
        cumulated_pdf += pdf[i];
        cdf[i] = cumulated_pdf;
    }

    const float rcp_cumulated_pdf = 1.0f / cumulated_pdf;
    pdf *= rcp_cumulated_pdf;
    cdf *= rcp_cumulated_pdf;
    cdf[src.size() - 1] = 1.0f;
}

}   // namespace renderer
