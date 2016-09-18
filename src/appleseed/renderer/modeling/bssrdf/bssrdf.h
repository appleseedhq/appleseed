
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDF_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class BSDF; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Profile of a Bidirectional Surface Scattering Reflectance Distribution Function (BSSRDF).
//
// The 1/Pi factor and the Fresnel terms at the incoming and outgoing points are not included.
//
// Conventions (Veach, 3.7.5, figure 3.3 on page 93):
//
//   * All direction vectors are expressed in world space.
//
//   * All direction vectors are unit-length and pointing outward.
//
//   * All probability densities are measured with respect to solid angle.
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

    // Destructor.
    ~BSSRDF();

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Return the BRDF associated with this BSSRDF.
    const BSDF& get_brdf() const;

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

    // Evaluate the inputs of this BSSRDF and of its child BSSRDFs, if any.
    // Input values are stored in the input evaluator. This method is called
    // once per shading point and pair of incoming/outgoing directions.
    virtual void evaluate_inputs(
        const ShadingContext&       shading_context,
        InputEvaluator&             input_evaluator,
        const ShadingPoint&         shading_point,
        const size_t                offset = 0) const;

    // Performs any precomputation needed for this BSSRDF input values.
    virtual void prepare_inputs(
        const ShadingPoint&         shading_point,
        void*                       data) const;

    // Sample r * R(r).
    virtual bool sample(
        SamplingContext&            sampling_context,
        const void*                 data,
        BSSRDFSample&               sample) const = 0;

    // Evaluate r * R(r) for a given pair of points and directions.
    virtual void evaluate(
        const void*                 data,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3d& outgoing_dir,
        const ShadingPoint&         incoming_point,
        const foundation::Vector3d& incoming_dir,
        Spectrum&                   value) const = 0;

    // Evaluate the PDF of r * R(r) for a given radius r.
    virtual double evaluate_pdf(
        const void*                 data,
        const size_t                channel,
        const double                radius) const = 0;

  protected:
    double compute_eta(
        const ShadingPoint&     shading_point,
        const double            ior) const;

    void make_reflectance_and_dmfp_compatible(
        Spectrum&               reflectance,
        const Spectrum&         dmfp) const;

    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_BSSRDF_H
