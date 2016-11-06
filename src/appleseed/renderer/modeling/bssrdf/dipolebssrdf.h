
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_DIPOLEBSSRDF_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_DIPOLEBSSRDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bssrdf/separablebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer      { class Assembly; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Dipole BSSRDF input values.
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

    struct Precomputed
    {
        Spectrum    m_alpha_prime;
        Spectrum    m_sigma_tr;
        Spectrum    m_channel_pdf;
        Spectrum    m_channel_cdf;
        float       m_rmax2;
        float       m_eta;
        Spectrum    m_dirpole_reparam_weight;
    };

    Precomputed     m_precomputed;
};


//
// Base class for radially-symmetric dipole BSSRDFs.
//

class DipoleBSSRDF
  : public SeparableBSSRDF
{
  public:
    // Constructor.
    DipoleBSSRDF(
        const char*                 name,
        const ParamArray&           params);

    virtual size_t compute_input_data_size(
        const Assembly&             assembly) const APPLESEED_OVERRIDE;

    virtual bool sample(
        SamplingContext&            sampling_context,
        const void*                 data,
        BSSRDFSample&               sample) const APPLESEED_OVERRIDE;

    virtual float evaluate_pdf(
        const void*                 data,
        const size_t                channel,
        const float                 radius) const APPLESEED_OVERRIDE;

  private:
    virtual float get_eta(
        const void*                 data) const APPLESEED_OVERRIDE;

  protected:
    template <typename ComputeRdFun>
    void do_prepare_inputs(
        const ShadingPoint&         shading_point,
        DipoleBSSRDFInputValues*    values) const
    {
        new (&values->m_precomputed) DipoleBSSRDFInputValues::Precomputed();

        // Precompute the relative index of refraction.
        values->m_precomputed.m_eta = compute_eta(shading_point, values->m_ior);

        if (m_inputs.source("sigma_a") == 0 || m_inputs.source("sigma_s") == 0)
        {
            //
            // Compute sigma_a, sigma_s and sigma_t from the diffuse surface reflectance
            // and mean free path (mfp).
            //

            make_reflectance_and_mfp_compatible(values->m_reflectance, values->m_mfp);

            // Apply multipliers to input values.
            values->m_reflectance *= values->m_reflectance_multiplier;
            values->m_mfp *= values->m_mfp_multiplier;

            // Clamp input values.
            foundation::clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
            foundation::clamp_low_in_place(values->m_mfp, 1.0e-6f);

            // Compute sigma_a and sigma_s.
            const ComputeRdFun rd_fun(values->m_precomputed.m_eta);
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

        // Precompute some coefficients and build a CDF for channel sampling.
        values->m_precomputed.m_alpha_prime.resize(values->m_reflectance.size());
        values->m_precomputed.m_channel_pdf.resize(values->m_reflectance.size());
        values->m_precomputed.m_channel_cdf.resize(values->m_reflectance.size());

        float cumulated_pdf = 0.0f;
        for (size_t i = 0, e = values->m_precomputed.m_channel_cdf.size(); i < e; ++i)
        {
            const float sigma_s_prime = values->m_sigma_s[i] * static_cast<float>(1.0 - values->m_g);
            const float sigma_t_prime = sigma_s_prime + values->m_sigma_a[i];
            values->m_precomputed.m_alpha_prime[i] = sigma_s_prime / sigma_t_prime;

            values->m_precomputed.m_channel_pdf[i] = values->m_precomputed.m_alpha_prime[i];
            cumulated_pdf += values->m_precomputed.m_channel_pdf[i];
            values->m_precomputed.m_channel_cdf[i] = cumulated_pdf;
        }

        const float rcp_cumulated_pdf = 1.0f / cumulated_pdf;
        values->m_precomputed.m_channel_pdf *= rcp_cumulated_pdf;
        values->m_precomputed.m_channel_cdf *= rcp_cumulated_pdf;
        values->m_precomputed.m_channel_cdf[values->m_precomputed.m_channel_cdf.size() - 1] = 1.0f;

        // Precompute the (square of the) max radius.
        values->m_precomputed.m_rmax2 = foundation::square(dipole_max_radius(foundation::min_value(values->m_precomputed.m_sigma_tr)));
    }
};


//
// DipoleBSSRDF class implementation.
//

inline size_t DipoleBSSRDF::compute_input_data_size(
    const Assembly&         assembly) const
{
    return foundation::align(sizeof(DipoleBSSRDFInputValues), 16);
}

inline float DipoleBSSRDF::get_eta(
    const void*             data) const
{
    return reinterpret_cast<const DipoleBSSRDFInputValues*>(data)->m_precomputed.m_eta;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_DIPOLEBSSRDF_H
