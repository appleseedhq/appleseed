
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
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
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
    ScalarInput m_weight;
    Spectrum    m_reflectance;
    ScalarInput m_reflectance_multiplier;
    Spectrum    m_dmfp;
    ScalarInput m_dmfp_multiplier;
    Spectrum    m_sigma_a;
    Spectrum    m_sigma_s;
    ScalarInput m_anisotropy;
    ScalarInput m_ior;

    // Precomputed values.
    Spectrum    m_sigma_t;
    Spectrum    m_sigma_s_prime;
    Spectrum    m_sigma_t_prime;
    Spectrum    m_alpha_prime;
    Spectrum    m_sigma_tr;
    Spectrum    m_channel_pdf;
    Spectrum    m_channel_cdf;
    double      m_rmax2;
    double      m_eta;
    Spectrum    m_dirpole_reparam_weight;
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
        const char*         name,
        const ParamArray&   params);

    virtual size_t compute_input_data_size(
        const Assembly&     assembly) const APPLESEED_OVERRIDE;

    virtual bool sample(
        SamplingContext&    sampling_context,
        const void*         data,
        BSSRDFSample&       sample) const APPLESEED_OVERRIDE;

    virtual double evaluate_pdf(
        const void*         data,
        const size_t        channel,
        const double        radius) const APPLESEED_OVERRIDE;

  private:
    virtual double get_eta(
        const void*         data) const APPLESEED_OVERRIDE;
};


//
// DipoleBSSRDF class implementation.
//

inline size_t DipoleBSSRDF::compute_input_data_size(
    const Assembly&         assembly) const
{
    return foundation::align(sizeof(DipoleBSSRDFInputValues), 16);
}

inline double DipoleBSSRDF::get_eta(
    const void*             data) const
{
    return reinterpret_cast<const DipoleBSSRDFInputValues*>(data)->m_eta;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_DIPOLEBSSRDF_H
