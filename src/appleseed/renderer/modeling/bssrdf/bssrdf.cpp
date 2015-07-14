
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"

// Standard headers.
#include <cmath>

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

UniqueID BSSRDF::get_class_uid()
{
    return g_class_uid;
}

BSSRDF::BSSRDF(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , m_lighting_conditions(0)
{
    set_name(name);
}

bool BSSRDF::on_frame_begin(
    const Project&          project,
    const Assembly&         assembly,
    IAbortSwitch*           abort_switch)
{
    m_lighting_conditions = &project.get_frame()->get_lighting_conditions();
    return true;
}

void BSSRDF::on_frame_end(
    const Project&          project,
    const Assembly&         assembly)
{
    m_lighting_conditions = 0;
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
}

bool BSSRDF::sample(
    const void*             data,
    BSSRDFSample&           sample) const
{
    sample.get_sampling_context().split_in_place(1, 1);
    const double s = sample.get_sampling_context().next_double2();

    const Basis3d& shading_basis = sample.get_shading_point().get_shading_basis();

    if (s <= 0.5)
    {
        sample.set_sample_basis(shading_basis);
        sample.set_use_offset_origin(true);
    }
    else if (s <= 0.75)
    {
        sample.set_sample_basis(
            Basis3d(
                shading_basis.get_tangent_u(),
                shading_basis.get_tangent_v(),
                shading_basis.get_normal()));
    }
    else
    {
        sample.set_sample_basis(
            Basis3d(
                shading_basis.get_tangent_v(),
                shading_basis.get_normal(),
                shading_basis.get_tangent_u()));
    }

    Vector2d d;
    if (do_sample(data, sample, d))
    {
        sample.set_origin(
            sample.get_shading_point().get_point() +
            sample.get_sample_basis().get_tangent_u() * d.x +
            sample.get_sample_basis().get_tangent_v() * d.y);

        return true;
    }

    return false;
}

double BSSRDF::pdf(
    const void*             data,
    const ShadingPoint&     outgoing_point,
    const ShadingPoint&     incoming_point,
    const Basis3d&          basis,
    const size_t            channel) const
{
    // From PBRT 3.

    const Vector3d d = outgoing_point.get_point() - incoming_point.get_point();
    const Vector3d dlocal(
        dot(basis.get_tangent_u(), d),
        dot(basis.get_tangent_v(), d),
        dot(basis.get_normal()   , d));

    const Vector3d& n = incoming_point.get_shading_normal();
    const Vector3d nlocal(
        dot(basis.get_tangent_u(), n),
        dot(basis.get_tangent_v(), n),
        dot(basis.get_normal()   , n));

    return
        do_pdf(data, channel, sqrt(square(dlocal.y) + square(dlocal.z))) * 0.125 * abs(nlocal[0]) +
        do_pdf(data, channel, sqrt(square(dlocal.z) + square(dlocal.x))) * 0.125 * abs(nlocal[1]) +
        do_pdf(data, channel, sqrt(square(dlocal.x) + square(dlocal.y))) * 0.250 * abs(nlocal[2]);
}

}   // namespace renderer
