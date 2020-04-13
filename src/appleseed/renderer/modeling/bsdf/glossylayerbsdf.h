
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace renderer      { class BSDF; }
namespace renderer      { class ParamArray; }

namespace renderer
{

//
// OSL glossy layer BSDF input values.
//

APPLESEED_DECLARE_INPUT_VALUES(GlossyLayerBSDFInputValues)
{
    Spectrum    m_reflectance;
    float       m_roughness;
    float       m_anisotropy;
    float       m_ior;
    Spectrum    m_transmittance;
    float       m_thickness;

    void*       m_substrate;

    struct Precomputed
    {
        float   m_outside_ior;
        float   m_F0;
    };

    Precomputed m_precomputed;
};


//
// OSL glossy layer BSDF factory.
//

class GlossyLayerBSDFFactory
{
  public:
    // Create a new BSDF instance.
    static foundation::auto_release_ptr<BSDF> create(const char* name, const ParamArray& params);
};


//
// Albedo tables.
//

float get_dielectric_layer_directional_albedo(
    const float         eta,
    const float         roughness,
    const float         cos_theta);

float get_dielectric_layer_average_albedo(
    const float         eta,
    const float         roughness);

// Write the computed tables to OpenEXR images and C++ arrays.
// Used in Renderer_Modeling_BSDF_EnergyCompensation unit test.
void write_dielectric_layer_directional_albedo_tables(const char* directory);

}       // namespace renderer
