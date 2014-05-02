
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "disneylayeredbdrf.h"

// appleseed.renderer headers
#include "renderer/modeling/bsdf/disneybdrf.h"

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Disney Layered BRDF implementation.
//

namespace
{

const char* Model = "disney_layered_brdf";

}

DisneyLayeredBRDF::DisneyLayeredBRDF(
    const char*         name,
    const ParamArray&   params)
  : BSDF(name, Reflective, Diffuse | Glossy, params)
{
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
    const Assembly&             assembly,
    foundation::AbortSwitch*    abort_switch)
{
    if (!BSDF::on_frame_begin(project, assembly, abort_switch))
        return false;
    
    return true;
}

void DisneyLayeredBRDF::on_frame_end(
    const Project&              project,
    const Assembly&             assembly)
{
    BSDF::on_frame_end(project, assembly);
}

size_t DisneyLayeredBRDF::compute_input_data_size(
    const Assembly&             assembly) const
{
    return sizeof(DisneyBRDFInputValues);
}

void DisneyLayeredBRDF::evaluate_inputs(
    InputEvaluator&             input_evaluator,
    const ShadingPoint&         shading_point,
    const size_t                offset) const
{
    return BSDF::evaluate_inputs(input_evaluator, shading_point, offset);
}

BSDF::Mode DisneyLayeredBRDF::sample(
    SamplingContext&    sampling_context,
    const void*         data,
    const bool          adjoint,
    const bool          cosine_mult,
    const Vector3d&     geometric_normal,
    const Basis3d&      shading_basis,
    const Vector3d&     outgoing,
    Vector3d&           incoming,
    Spectrum&           value,
    double&             probability) const
{
    value.set(0.0f);
    probability = 0.0;
    return Absorption;
}

double DisneyLayeredBRDF::evaluate(
    const void*         data,
    const bool          adjoint,
    const bool          cosine_mult,
    const Vector3d&     geometric_normal,
    const Basis3d&      shading_basis,
    const Vector3d&     outgoing,
    const Vector3d&     incoming,
    const int           modes,
    Spectrum&           value) const
{
    value.set(0.0f);
    return 0.0;    
}

double DisneyLayeredBRDF::evaluate_pdf(
    const void*         data,
    const Vector3d&     geometric_normal,
    const Basis3d&      shading_basis,
    const Vector3d&     outgoing,
    const Vector3d&     incoming,
    const int           modes) const
{
    return 0.0;
}

}   // namespace renderer
