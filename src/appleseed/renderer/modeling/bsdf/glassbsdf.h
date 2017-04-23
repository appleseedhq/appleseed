
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_GLASSBSDF_H
#define APPLESEED_RENDERER_MODELING_BSDF_GLASSBSDF_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

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
// Glass BSDF input values.
//

APPLESEED_DECLARE_INPUT_VALUES(GlassBSDFInputValues)
{
    Spectrum        m_surface_transmittance;
    float           m_surface_transmittance_multiplier;
    Spectrum        m_reflection_tint;
    Spectrum        m_refraction_tint;
    float           m_roughness;
    float           m_highlight_falloff;
    float           m_anisotropy;
    float           m_ior;
    Spectrum        m_volume_transmittance;
    float           m_volume_transmittance_distance;
    Spectrum        m_volume_absorption;
    float           m_volume_density;
    float           m_volume_scale;

    struct Precomputed
    {
        bool        m_backfacing;
        float       m_eta;
        Spectrum    m_reflection_color;
        Spectrum    m_refraction_color;
        float       m_reflection_weight;
        float       m_refraction_weight;
    };

    Precomputed     m_precomputed;
};


//
// Glass BSDF factory.
//

class APPLESEED_DLLSYMBOL GlassBSDFFactory
  : public IBSDFFactory
{
  public:
    // Return a string identifying this BSDF model.
    virtual const char* get_model() const APPLESEED_OVERRIDE;

    // Return metadata for this BSDF model.
    virtual foundation::Dictionary get_model_metadata() const APPLESEED_OVERRIDE;

    // Return metadata for the inputs of this BSDF model.
    virtual foundation::DictionaryArray get_input_metadata() const APPLESEED_OVERRIDE;

    // Create a new BSDF instance.
    virtual foundation::auto_release_ptr<BSDF> create(
        const char*         name,
        const ParamArray&   params) const APPLESEED_OVERRIDE;

    // Create a new BSDF instance for use with OSL.
    foundation::auto_release_ptr<BSDF> create_osl(
        const char*         name,
        const ParamArray&   params) const;

    // Static variant of the create() method above.
    static foundation::auto_release_ptr<BSDF> static_create(
        const char*         name,
        const ParamArray&   params);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_GLASSBSDF_H
