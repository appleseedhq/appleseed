
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
#include "renderer/modeling/bssrdf/separablebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class BSDFSample; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Input values common to all dipole BSSRDF models.
//

APPLESEED_DECLARE_INPUT_VALUES(DipoleBSSRDFInputValues)
{
    float           m_weight;
    Spectrum        m_reflectance;
    float           m_reflectance_multiplier;
    Spectrum        m_mfp;
    float           m_mfp_multiplier;
    Spectrum        m_sigma_a;
    Spectrum        m_sigma_s;
    float           m_g;
    float           m_ior;
    float           m_fresnel_weight;

    struct Precomputed
    {
        Spectrum    m_alpha_prime;
        Spectrum    m_sigma_tr;
        Spectrum    m_channel_pdf;
    };

    Precomputed                     m_precomputed;
    SeparableBSSRDF::InputValues    m_base_values;
};


//
// Base class for dipole-based BSSRDF models.
//

class DipoleBSSRDF
  : public SeparableBSSRDF
{
  public:
    // Constructor.
    DipoleBSSRDF(
        const char*                 name,
        const ParamArray&           params);

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch) override;

    size_t compute_input_data_size() const override;

    float sample_profile(
        const void*                 data,
        const size_t                channel,
        const float                 u) const override;

    float evaluate_profile_pdf(
        const void*                 data,
        const float                 disk_radius) const override;

    bool sample(
        const ShadingContext&       shading_context,
        SamplingContext&            sampling_context,
        const void*                 data,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3f& outgoing_dir,
        const int                   modes,
        BSSRDFSample&               bssrdf_sample,
        BSDFSample&                 bsdf_sample) const override;

    void evaluate(
        const void*                 data,
        const ShadingPoint&         outgoing_point,
        const foundation::Vector3f& outgoing_dir,
        const ShadingPoint&         incoming_point,
        const foundation::Vector3f& incoming_dir,
        const int                   modes,
        Spectrum&                   value) const override;

  protected:
    template <typename ComputeRdFun>
    void do_prepare_inputs(
        const ShadingPoint&         shading_point,
        DipoleBSSRDFInputValues*    values) const;

  protected:
    bool m_has_sigma_sources;
};


//
// DipoleBSSRDF class implementation.
//

inline bool DipoleBSSRDF::on_frame_begin(
    const Project&                  project,
    const BaseGroup*                parent,
    OnFrameBeginRecorder&           recorder,
    foundation::IAbortSwitch*       abort_switch)
{
    if (!SeparableBSSRDF::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_has_sigma_sources =
        m_inputs.source("sigma_a") != nullptr &&
        m_inputs.source("sigma_s") != nullptr;

    return true;
}

template <typename ComputeRdFun>
void DipoleBSSRDF::do_prepare_inputs(
    const ShadingPoint&             shading_point,
    DipoleBSSRDFInputValues*        values) const
{
    new (&values->m_precomputed) DipoleBSSRDFInputValues::Precomputed();

    new (&values->m_base_values) SeparableBSSRDF::InputValues();
    values->m_base_values.m_weight = values->m_weight;
    values->m_base_values.m_fresnel_weight = values->m_fresnel_weight;

    // Precompute the relative index of refraction.
    values->m_base_values.m_eta = compute_eta(shading_point, values->m_ior);

    if (!m_has_sigma_sources)
    {
        //
        // Compute sigma_a, sigma_s and sigma_t from the diffuse surface reflectance
        // and mean free path (mfp).
        //

        // Apply multipliers to input values.
        values->m_reflectance *= values->m_reflectance_multiplier;
        values->m_mfp *= values->m_mfp_multiplier;

        // Clamp input values.
        foundation::clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
        foundation::clamp_low_in_place(values->m_mfp, 1.0e-6f);

        // Compute sigma_a and sigma_s.
        const ComputeRdFun rd_fun(values->m_base_values.m_eta);
        compute_absorption_and_scattering_mfp(
            rd_fun,
            values->m_reflectance,
            values->m_mfp,
            values->m_sigma_a,
            values->m_sigma_s);
    }

    //
    // Compute sigma_tr from sigma_a and sigma_s.
    //
    // If you want to use the sigma_a and sigma_s values provided in [1],
    // and if your scene is modeled in meters, you will need to *multiply*
    // them by 1000. If your scene is modeled in centimers (e.g. the "size"
    // of the object is 4 units) then you will need to multiply the sigmas
    // by 100.
    //

    effective_extinction_coefficient(
        values->m_sigma_a,
        values->m_sigma_s,
        values->m_g,
        values->m_precomputed.m_sigma_tr);

    // Precompute alpha'.
    for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
    {
        const float sigma_s_prime = values->m_sigma_s[i] * (1.0f - values->m_g);
        const float sigma_t_prime = sigma_s_prime + values->m_sigma_a[i];
        values->m_precomputed.m_alpha_prime[i] = sigma_s_prime / sigma_t_prime;
    }

    // Build a CDF and PDF for channel sampling.
    build_cdf_and_pdf(
        values->m_precomputed.m_alpha_prime,
        values->m_base_values.m_channel_cdf,
        values->m_precomputed.m_channel_pdf);

    // Precompute the radius of the sampling disk.
    values->m_base_values.m_max_disk_radius =
        dipole_max_radius(foundation::min_value(values->m_precomputed.m_sigma_tr));
}

}   // namespace renderer
