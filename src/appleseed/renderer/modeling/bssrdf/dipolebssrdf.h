
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

#ifndef APPLESEED_RENDERER_MODELING_BSSRDF_DIPOLEBSSRDF_H
#define APPLESEED_RENDERER_MODELING_BSSRDF_DIPOLEBSSRDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/ibssrdffactory.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class Assembly; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Dipole BSSRDF input values.
//

APPLESEED_DECLARE_INPUT_VALUES(DipoleBSSRDFInputValues)
{
    double      m_weight;
    Spectrum    m_reflectance;
    double      m_reflectance_multiplier;
    double      m_dmfp;
    double      m_dmfp_multiplier;
    double      m_anisotropy;
    double      m_outside_ior;
    double      m_inside_ior;

    // Precomputed values.
    Spectrum    m_sigma_a;
    Spectrum    m_sigma_s;
    double      m_max_radius2;
};


//
// Base class for dipole BSSRDFs.
//

class DipoleBSSRDF
  : public BSSRDF
{
  public:
    // Constructor.
    DipoleBSSRDF(
        const char*             name,
        const ParamArray&       params);

    virtual size_t compute_input_data_size(
        const Assembly&         assembly) const APPLESEED_OVERRIDE;

    virtual void evaluate_inputs(
        const ShadingContext&   shading_context,
        foundation::uint8*      data,
        const ShadingPoint&     shading_point,
        const size_t            offset = 0) const APPLESEED_OVERRIDE;

    virtual bool sample(
        const void*             data,
        BSSRDFSample&           sample) const APPLESEED_OVERRIDE;

    virtual double evaluate_pdf(
        const void*             data,
        const size_t            channel,
        const double            radius) const APPLESEED_OVERRIDE;
};


//
// Base class for dipole BSSRDF factories.
//

class APPLESEED_DLLSYMBOL DipoleBSSRDFFactory
  : public IBSSRDFFactory
{
  public:
    virtual foundation::DictionaryArray get_input_metadata() const APPLESEED_OVERRIDE;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSSRDF_DIPOLEBSSRDF_H
