
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_INPUT_SCALARSOURCE_H
#define APPLESEED_RENDERER_MODELING_INPUT_SCALARSOURCE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/siphash.h"

namespace renderer
{

//
// Scalar source.
//

class ScalarSource
  : public Source
{
  public:
    // Constructor.
    explicit ScalarSource(const ScalarInput scalar);

    // Compute a signature unique to this source.
    virtual foundation::uint64 compute_signature() const APPLESEED_OVERRIDE;

    // Evaluate the source.
    virtual void evaluate_uniform(
        ScalarInput&                scalar) const APPLESEED_OVERRIDE;
    virtual void evaluate_uniform(
        foundation::Color3f&        linear_rgb) const APPLESEED_OVERRIDE;
    virtual void evaluate_uniform(
        Spectrum&                   spectrum) const APPLESEED_OVERRIDE;
    virtual void evaluate_uniform(
        Alpha&                      alpha) const APPLESEED_OVERRIDE;
    virtual void evaluate_uniform(
        foundation::Color3f&        linear_rgb,
        Alpha&                      alpha) const APPLESEED_OVERRIDE;
    virtual void evaluate_uniform(
        Spectrum&                   spectrum,
        Alpha&                      alpha) const APPLESEED_OVERRIDE;

  private:
    const ScalarInput    m_scalar;
};


//
// ScalarSource class implementation.
//

inline ScalarSource::ScalarSource(const ScalarInput scalar)
  : Source(true)
  , m_scalar(scalar)
{
}

inline foundation::uint64 ScalarSource::compute_signature() const
{
    return foundation::siphash24(m_scalar);
}

inline void ScalarSource::evaluate_uniform(
    ScalarInput&                    scalar) const
{
    scalar = m_scalar;
}

inline void ScalarSource::evaluate_uniform(
    foundation::Color3f&            linear_rgb) const
{
    linear_rgb.set(static_cast<float>(m_scalar));
}

inline void ScalarSource::evaluate_uniform(
    Spectrum&                       spectrum) const
{
    spectrum.set(static_cast<float>(m_scalar));
}

inline void ScalarSource::evaluate_uniform(
    Alpha&                          alpha) const
{
    alpha.set(static_cast<float>(m_scalar));
}

inline void ScalarSource::evaluate_uniform(
    foundation::Color3f&            linear_rgb,
    Alpha&                          alpha) const
{
    linear_rgb.set(static_cast<float>(m_scalar));
    alpha.set(1.0f);
}

inline void ScalarSource::evaluate_uniform(
    Spectrum&                       spectrum,
    Alpha&                          alpha) const
{
    spectrum.set(static_cast<float>(m_scalar));
    alpha.set(1.0f);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_INPUT_SCALARSOURCE_H
