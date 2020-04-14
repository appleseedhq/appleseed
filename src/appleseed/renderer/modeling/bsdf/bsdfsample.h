
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"

// Standard headers.
#include <cassert>

namespace renderer
{

//
// The BSDFSample class represents the result of sampling a BSDF.
//

class BSDFSample
{
  public:
    foundation::Dual3f              m_incoming;             // world space incoming direction, unit-length, defined only if m_mode != None
    DirectShadingComponents         m_value;                // BSDF value, defined only if m_mode != None
    AOVComponents                   m_aov_components;
    float                           m_min_roughness;        // minimum BSDF roughness down the line if Roughness Clamping is enabled

    // Constructor.
    BSDFSample();

    ScatteringMode::Mode get_mode() const;
    float get_probability() const;

    void set_to_absorption();
    void set_to_scattering(
        const ScatteringMode::Mode  mode,
        const float                 probability);

    void compute_specular_reflected_differentials(
        const BSDF::LocalGeometry&  local_geometry,
        const foundation::Dual3f&   outgoing);

    void compute_specular_transmitted_differentials(
        const BSDF::LocalGeometry&  local_geometry,
        const float                 eta,
        const bool                  is_entering,
        const foundation::Dual3f&   outgoing);

    void compute_glossy_reflected_differentials(
        const BSDF::LocalGeometry&  local_geometry,
        const float                 roughness,
        const foundation::Dual3f&   outgoing);

    void compute_glossy_transmitted_differentials(
        const BSDF::LocalGeometry&  local_geometry,
        const float                 eta,
        const float                 roughness,
        const bool                  is_entering,
        const foundation::Dual3f&   outgoing);

    void compute_diffuse_differentials(
        const foundation::Dual3f&   outgoing);

  private:
    ScatteringMode::Mode            m_mode;                 // scattering mode
    float                           m_probability;          // PDF value
};


//
// BSDFSample class implementation.
//

inline BSDFSample::BSDFSample()
  : m_min_roughness(0.0f)
{
    set_to_absorption();
}

inline ScatteringMode::Mode BSDFSample::get_mode() const
{
    return m_mode;
}

inline float BSDFSample::get_probability() const
{
    assert(
        (m_mode == ScatteringMode::None     && m_probability == 0.0f) ||
        (m_mode == ScatteringMode::Specular && m_probability == BSDF::DiracDelta) ||
        (m_mode != ScatteringMode::Specular && m_probability > 0.0f));

    return m_probability;
}

inline void BSDFSample::set_to_absorption()
{
    m_mode = ScatteringMode::None;
    m_probability = 0.0f;
}

inline void BSDFSample::set_to_scattering(const ScatteringMode::Mode mode, const float probability)
{
    assert(
        mode == ScatteringMode::None ||
        mode == ScatteringMode::Diffuse ||
        mode == ScatteringMode::Glossy ||
        mode == ScatteringMode::Specular);

    assert(
        (mode == ScatteringMode::None     && probability == 0.0f) ||
        (mode == ScatteringMode::Specular && probability == BSDF::DiracDelta) ||
        (mode != ScatteringMode::Specular && probability > 0.0f));

    m_mode = mode;
    m_probability = probability;
}

}   // namespace renderer
