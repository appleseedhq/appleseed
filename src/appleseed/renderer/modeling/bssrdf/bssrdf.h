
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDF_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class Intersector; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// BSSRDF input values.
//

APPLESEED_DECLARE_INPUT_VALUES(BSSRDFInputValues)
{
    Spectrum    m_sigma_a;          // Absorption coefficient.
    Spectrum    m_sigma_s_prime;    // Reduced scattering coefficient.

    // Not so sure about the next ones...
    double      m_g;                // Anisotropy.
    double      m_ior;              // Index of refraction.
};


//
// Bidirectional Surface Scattering Reflectance Distribution Function (BSSRDF).
//
// Conventions (Veach, 3.7.5, figure 3.3 on page 93):
//
//   * All direction vectors are expressed in world space.
//
//   * All direction vectors are unit-length and pointing outward.
//
//   * All probability densities are measured with respect to solid angle.
//
//   * When the adjoint flag is false, such as in path tracing, the BSSRDF
//     characterizes the light flow.
//
//   * When the adjoint flag is true, such as in photon tracing, the BSSRDF
//     characterizes the importance flow.
//
//   * Regardless of the adjoint flag, light and importance always flow
//     from the incoming direction to the outgoing direction.
//
//   * The incoming direction is always the sampled direction.
//

class APPLESEED_DLLSYMBOL BSSRDF
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    BSSRDF(
        const char*                 name,
        const ParamArray&           params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        foundation::IAbortSwitch*   abort_switch = 0);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&              project,
        const Assembly&             assembly);

    // Compute the cumulated size in bytes of the values of all inputs of
    // this BSSRDF and its child BSSRDFs, if any.
    virtual size_t compute_input_data_size(
        const Assembly&             assembly) const;

    virtual void evaluate_inputs(
        const ShadingContext&       shading_context,
        InputEvaluator&             input_evaluator,
        const ShadingPoint&         shading_point,
        const size_t                offset = 0) const;

    virtual void sample(
        const void*                 data,
        const Intersector&          intersector,
        BSSRDFSample&               sample) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDF_H
