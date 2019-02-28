
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

namespace renderer
{

//
// The BSDFSample class represents the result of sampling a BSDF.
// It is also used to pass arguments to the BSDF::sample() method.
//

class BSDFSample
{
  public:
    // Inputs.
    const ShadingPoint*             m_shading_point;
    foundation::Vector3f            m_geometric_normal;     // world space geometric normal at the point where sampling is done, unit-length
    foundation::Basis3f             m_shading_basis;        // world space shading basis at the point where sampling is done
    foundation::Dual3f              m_outgoing;             // world space outgoing direction, unit-length

    // Outputs.
    foundation::Dual3f              m_incoming;             // world space incoming direction, unit-length, defined only if m_mode != None
    DirectShadingComponents         m_value;                // BSDF value, defined only if m_mode != None
    AOVComponents                   m_aov_components;
    float                           m_min_roughness;        // minimum BSDF roughness down the line if Roughness Clamping is enabled

    // Constructor.
    BSDFSample(
        const ShadingPoint*         shading_point,
        const foundation::Dual3f&   outgoing);

    void set_to_absorption();
    void set_to_scattering(const ScatteringMode::Mode mode, const float probability);

    ScatteringMode::Mode get_mode() const;
    float get_probability() const;

    void compute_reflected_differentials();
    void compute_transmitted_differentials(const float eta);

  private:
    ScatteringMode::Mode            m_mode;                 // scattering mode
    float                           m_probability;          // PDF value

    void compute_normal_derivatives(
        foundation::Vector3f&       dndx,
        foundation::Vector3f&       dndy,
        float&                      ddndx,
        float&                      ddndy) const;

    void apply_pdf_differentials_heuristic();
};


//
// BSDFSample class implementation.
//

inline BSDFSample::BSDFSample(
    const ShadingPoint*             shading_point,
    const foundation::Dual3f&       outgoing)
  : m_shading_point(shading_point)
  , m_geometric_normal(shading_point->get_geometric_normal())
  , m_shading_basis(shading_point->get_shading_basis())
  , m_outgoing(outgoing)
  , m_min_roughness(0.0f)
{
    set_to_absorption();
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

}   // namespace renderer
