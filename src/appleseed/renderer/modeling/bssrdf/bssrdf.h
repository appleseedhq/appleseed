
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

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
namespace foundation    { class Arena; }
namespace renderer      { class BSDFSample; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

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

    // Return the size in bytes to allocate for the input values of this BSSRDF
    // and its precomputed values, if any. By default, enough space is allocated
    // for the inputs alone, i.e. this returns get_inputs().compute_data_size().
    // If a BSSRDF stores additional data such as precomputed values in its input
    // block, it must override this method and return the correct size.
    // If evaluate_inputs() is overridden, then this method is irrelevant.
    virtual size_t compute_input_data_size() const;

    // Evaluate the inputs of this BSSRDF and of its child BSSRDFs, if any.
    virtual void* evaluate_inputs(
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point) const;

    // Precompute data based on already evaluated input values.
    virtual void prepare_inputs(
        foundation::Arena&          arena,
        const ShadingPoint&         shading_point,
        void*                       data) const;

    // Sample the BSSRDF.
    virtual bool sample(
        const ShadingContext&       shading_context,
        SamplingContext&            sampling_context,
        const void*                 data,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3f& outgoing_dir,
        const int                   modes,                      // allowed scattering modes
        BSSRDFSample&               bssrdf_sample,
        BSDFSample&                 bsdf_sample) const = 0;

    // Evaluate the BSSRDF.
    virtual void evaluate(
        const void*                 data,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3f& outgoing_dir,
        const ShadingPoint&         incoming_point,
        const foundation::Vector3f& incoming_dir,
        const int                   modes,                      // enabled scattering modes
        Spectrum&                   value) const = 0;

  protected:
    static float compute_eta(
        const ShadingPoint&         shading_point,
        const float                 ior);

    static void build_cdf_and_pdf(
        const Spectrum&             src,
        Spectrum&                   cdf,
        Spectrum&                   pdf);
};

}   // namespace renderer
