
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_INPUT_COLORSOURCE_H
#define APPLESEED_RENDERER_MODELING_INPUT_COLORSOURCE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/input/inputformat.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Forward declarations.
namespace renderer  { class ColorEntity; }

namespace renderer
{

//
// Uniform source.
//

class ColorSource
  : public Source
{
  public:
    // Constructor.
    ColorSource(
        const ColorEntity&          color_entity,
        const InputFormat           input_format);

    // Evaluate the source.
    virtual void evaluate_uniform(
        double&                     scalar) const OVERRIDE;
    virtual void evaluate_uniform(
        foundation::Color3f&        linear_rgb) const OVERRIDE;
    virtual void evaluate_uniform(
        Spectrum&                   spectrum) const OVERRIDE;
    virtual void evaluate_uniform(
        Alpha&                      alpha) const OVERRIDE;
    virtual void evaluate_uniform(
        foundation::Color3f&        linear_rgb,
        Alpha&                      alpha) const OVERRIDE;
    virtual void evaluate_uniform(
        Spectrum&                   spectrum,
        Alpha&                      alpha) const OVERRIDE;

  private:
    double                          m_scalar;
    foundation::Color3f             m_linear_rgb;
    Spectrum                        m_spectrum;
    Alpha                           m_alpha;

    void initialize_from_spectrum(
        const ColorEntity&          color_entity);

    void initialize_from_3d_color(
        const ColorEntity&          color_entity,
        const InputFormat           input_format);
};


//
// ColorSource class implementation.
//

inline void ColorSource::evaluate_uniform(
    double&                         scalar) const
{
    scalar = m_scalar;
}

inline void ColorSource::evaluate_uniform(
    foundation::Color3f&            linear_rgb) const
{
    linear_rgb = m_linear_rgb;
}

inline void ColorSource::evaluate_uniform(
    Spectrum&                       spectrum) const
{
    spectrum = m_spectrum;
}

inline void ColorSource::evaluate_uniform(
    Alpha&                          alpha) const
{
    alpha = m_alpha;
}

inline void ColorSource::evaluate_uniform(
    foundation::Color3f&            linear_rgb,
    Alpha&                          alpha) const
{
    linear_rgb = m_linear_rgb;
    alpha = m_alpha;
}

inline void ColorSource::evaluate_uniform(
    Spectrum&                       spectrum,
    Alpha&                          alpha) const
{
    spectrum = m_spectrum;
    alpha = m_alpha;
}

// Convert a set of regularly spaced spectral values to the internal spectrum format.
DLLSYMBOL Spectrum spectral_values_to_spectrum(
    const float               wavelength_start,
    const float               wavelength_end,
    const ColorValueArray&    values);

// Converts a given spectrum to CIEXYZ using the IlluminantCIED65 illuminant and XYZCMFCIE196410Deg matching function
DLLSYMBOL foundation::Color3f spectrum_to_xyz_standard(
    const Spectrum& spectrum);

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_INPUT_COLORSOURCE_H
