
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Girish Ramesh, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class BSDF; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// Hair BSDF input values.
//

APPLESEED_DECLARE_INPUT_VALUES(HairBSDFInputValues)
{
    Spectrum        m_reflectance;
    float           m_melanin;
    float           m_melanin_redness;
    float           m_eta;
    float           m_beta_M;
    float           m_beta_N;
    float           m_alpha;

    struct Precomputed
    {
        Spectrum    m_sigma_a;
        float       m_h;
        float       m_v;
        float       m_s;
        float       m_angle_R;
        float       m_angle_TT;
        float       m_angle_TRT;
    };

    Precomputed m_precomputed;

};

//
// Hair BSDF factory.
//

class APPLESEED_DLLSYMBOL HairBSDFFactory
  : public IBSDFFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying this BSDF model.
    const char* get_model() const override;

    // Return metadata for this BSDF model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this BSDF model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new BSDF instance.
    foundation::auto_release_ptr<BSDF> create(
        const char*         name,
        const ParamArray&   params) const override;
};

}       // namespace renderer

