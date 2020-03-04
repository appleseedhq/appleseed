
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/platform/compiler.h"

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
    float           m_anisotropy;
    float           m_ior;
    Spectrum        m_volume_transmittance;
    float           m_volume_transmittance_distance;
    Spectrum        m_volume_absorption;
    float           m_volume_density;
    float           m_volume_scale;
    float           m_energy_compensation;

    struct Precomputed
    {
        bool        m_backfacing;
        float       m_outside_ior;
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

// Write the computed tables to OpenEXR images and C++ arrays.
// Used in Renderer_Modeling_BSDF_EnergyCompensation unit tests.
void write_glass_directional_albedo_tables(const char* directory);

}   // namespace renderer
