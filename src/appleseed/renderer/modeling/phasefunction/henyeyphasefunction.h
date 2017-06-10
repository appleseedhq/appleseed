
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_PHASEFUNCTION_HENYEYPHASEFUNCTION_H
#define APPLESEED_RENDERER_MODELING_PHASEFUNCTION_HENYEYPHASEFUNCTION_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/phasefunction/iphasefunctionfactory.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class ParamArray; }
namespace renderer      { class PhaseFunction; }

namespace renderer
{

//
// Henyey-Greenstein phase function input values.
//

APPLESEED_DECLARE_INPUT_VALUES(HenyeyPhaseFunctionInputValues)
{
    Spectrum    m_absorption;               // absorption coefficient of the media
    float       m_absorption_multiplier;    // absorption coefficient multiplier
    Spectrum    m_scattering;               // scattering coefficient of the media
    float       m_scattering_multiplier;    // scattering coefficient multiplier
    float       m_average_cosine;           // asymmetry parameter, often referred as g

    struct Precomputed
    {
        Spectrum    m_normalized_extinction;    // extinction coefficient of the media (normalized)
        float       m_extinction_multiplier;    // extinction coefficient multiplier
    };

    Precomputed m_precomputed;
};

//
// Henyey-Greenstein phase function factory.
//

class APPLESEED_DLLSYMBOL HenyeyPhaseFunctionFactory
  : public IPhaseFunctionFactory
{
  public:
    // Return a string identifying this phase function model.
    virtual const char* get_model() const override;

    // Return metadata for this phase function model.
    virtual foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this phase function model.
    virtual foundation::DictionaryArray get_input_metadata() const override;

    // Create a new phase function instance.
    virtual foundation::auto_release_ptr<PhaseFunction> create(
        const char*         name,
        const ParamArray&   params) const override;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PHASEFUNCTION_HENYEYPHASEFUNCTION_H
