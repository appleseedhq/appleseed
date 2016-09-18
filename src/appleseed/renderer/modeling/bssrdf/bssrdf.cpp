
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/lambertianbrdf.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// BSSRDF class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

struct BSSRDF::Impl
{
    auto_release_ptr<BSDF> m_brdf;
};

UniqueID BSSRDF::get_class_uid()
{
    return g_class_uid;
}

BSSRDF::BSSRDF(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , impl(new Impl())
{
    impl->m_brdf =
        LambertianBRDFFactory().create(
            (string(name) + "_brdf").c_str(),
            ParamArray().insert("reflectance", "1.0"));

    set_name(name);
}

BSSRDF::~BSSRDF()
{
    delete impl;
}

const BSDF& BSSRDF::get_brdf() const
{
    return impl->m_brdf.ref();
}

bool BSSRDF::on_frame_begin(
    const Project&          project,
    const Assembly&         assembly,
    IAbortSwitch*           abort_switch)
{
    return true;
}

void BSSRDF::on_frame_end(
    const Project&          project,
    const Assembly&         assembly)
{
}

size_t BSSRDF::compute_input_data_size(
    const Assembly&         assembly) const
{
    return get_inputs().compute_data_size();
}

void BSSRDF::evaluate_inputs(
    const ShadingContext&   shading_context,
    InputEvaluator&         input_evaluator,
    const ShadingPoint&     shading_point,
    const size_t            offset) const
{
    input_evaluator.evaluate(get_inputs(), shading_point.get_uv(0), offset);
    prepare_inputs(shading_point, input_evaluator.data() + offset);
}

void BSSRDF::prepare_inputs(
    const ShadingPoint&     shading_point,
    void*                   data) const
{
}

double BSSRDF::compute_eta(
    const ShadingPoint&     shading_point,
    const double            ior) const
{
    const double outside_ior =
        shading_point.is_entering()
            ? shading_point.get_ray().get_current_ior()
            : shading_point.get_ray().get_previous_ior();

    return outside_ior / ior;
}

void BSSRDF::make_reflectance_and_dmfp_compatible(
    Spectrum&               reflectance,
    const Spectrum&         dmfp) const
{
    if (reflectance.size() != dmfp.size())
    {
        // Since it does not really make sense to convert a dmfp,
        // a per channel distance, as if it were a color,
        // we instead always convert the reflectance to match the
        // size of the dmfp.
        if (dmfp.is_spectral())
        {
            Spectrum::upgrade(
                reflectance,
                reflectance);
        }
        else
        {
            Spectrum::downgrade(
                g_std_lighting_conditions,
                reflectance,
                reflectance);
        }
    }
}

}   // namespace renderer
