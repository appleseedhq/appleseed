
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstdint>

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
    explicit ColorSource(const ColorEntity& color_entity);

    // Retrieve the color entity used by this source.
    const ColorEntity& get_color_entity() const;

    // Compute a signature unique to this source.
    std::uint64_t compute_signature() const override;

    // Return hints allowing to treat this source as one of another type.
    Hints get_hints() const override;

    // Evaluate the source.
    void evaluate_uniform(
        float&                      scalar) const override;
    void evaluate_uniform(
        foundation::Color3f&        linear_rgb) const override;
    void evaluate_uniform(
        Spectrum&                   spectrum) const override;
    void evaluate_uniform(
        Alpha&                      alpha) const override;
    void evaluate_uniform(
        foundation::Color3f&        linear_rgb,
        Alpha&                      alpha) const override;
    void evaluate_uniform(
        Spectrum&                   spectrum,
        Alpha&                      alpha) const override;

  private:
    const ColorEntity&              m_color_entity;
    float                           m_scalar;
    foundation::Color3f             m_linear_rgb;
    Spectrum                        m_spectrum;
    Alpha                           m_alpha;

    void initialize_from_spectrum(const ColorEntity& color_entity);
    void initialize_from_color3(const ColorEntity& color_entity);
};


//
// ColorSource class implementation.
//

inline const ColorEntity& ColorSource::get_color_entity() const
{
    return m_color_entity;
}

inline void ColorSource::evaluate_uniform(
    float&                          scalar) const
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

}   // namespace renderer
