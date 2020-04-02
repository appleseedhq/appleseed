
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
#include "renderer/modeling/bsdf/lambertianbrdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class BSDF; }
namespace renderer  { class BSDFSample; }
namespace renderer  { class BSSRDFSample; }
namespace renderer  { class ParamArray; }
namespace renderer  { class ShadingContext; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// BSSRDF that can be expressed as a product of a radially-symmetric spatial
// term and a pair of directional terms.
//

class APPLESEED_DLLSYMBOL SeparableBSSRDF
  : public BSSRDF
{
  public:
    // Constructor.
    SeparableBSSRDF(
        const char*                 name,
        const ParamArray&           params);

    // Destructor.
    ~SeparableBSSRDF() override;

    // Sample the radially-symmetric profile and return a radius value.
    virtual float sample_profile(
        const void*                 data,
        const size_t                channel,
        const float                 u) const = 0;

    // Evaluate the radially-symmetric profile's PDF with respect to area measure.
    virtual float evaluate_profile_pdf(
        const void*                 data,
        const float                 disk_radius) const = 0;

    // Evaluate the radially-symmetric profile.
    virtual void evaluate_profile(
        const void*                 data,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3f& outgoing_dir,
        const ShadingPoint&         incoming_point,
        const foundation::Vector3f& incoming_dir,
        Spectrum&                   value) const = 0;

    struct InputValues
    {
        float       m_weight;
        float       m_fresnel_weight;
        float       m_eta;
        float       m_max_disk_radius;
        Spectrum    m_channel_cdf;
    };

  protected:
    // The BRDF that represents the directional component of this BSSRDF.
    const BSDF*                     m_brdf;
    LambertianBRDFInputValues       m_brdf_data;

    // Implementation of the BSSRDF::sample() method.
    bool do_sample(
        const ShadingContext&       shading_context,
        SamplingContext&            sampling_context,
        const void*                 data,
        const InputValues&          values,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3f& outgoing_dir,
        const int                   modes,
        BSSRDFSample&               bssrdf_sample,
        BSDFSample&                 bsdf_sample) const;

    // Implementation of the BSSRDF::evaluate() method.
    void do_evaluate(
        const void*                 data,
        const InputValues&          values,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3f& outgoing_dir,
        const ShadingPoint&         incoming_point,
        const foundation::Vector3f& incoming_dir,
        const int                   modes,
        Spectrum&                   value) const;
};

}   // namespace renderer
