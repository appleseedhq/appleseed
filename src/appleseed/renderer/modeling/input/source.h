
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace renderer      { class TextureCache; }
namespace renderer      { class SourceInputs; }

namespace renderer
{

//
// Source base class.
//

class APPLESEED_DLLSYMBOL Source
{
  public:
    // Constructor.
    explicit Source(const bool uniform);

    // Destructor.
    virtual ~Source() = 0;

    // Compute a signature unique to this source.
    virtual std::uint64_t compute_signature() const = 0;

    // Return true if the source is uniform, false if it is varying.
    bool is_uniform() const;

    struct Hints
    {
        // Allow treating this source as a 2D texture map with the following dimensions in pixels.
        size_t  m_width;
        size_t  m_height;
    };

    // Return hints allowing to treat this source as one of another type.
    virtual Hints get_hints() const = 0;

    // Evaluate the source at a given shading point.
    virtual void evaluate(
        TextureCache&               texture_cache,
        const SourceInputs&         source_inputs,
        float&                      scalar) const;
    virtual void evaluate(
        TextureCache&               texture_cache,
        const SourceInputs&         source_inputs,
        foundation::Color3f&        linear_rgb) const;
    virtual void evaluate(
        TextureCache&               texture_cache,
        const SourceInputs&         source_inputs,
        Spectrum&                   spectrum) const;
    virtual void evaluate(
        TextureCache&               texture_cache,
        const SourceInputs&         source_inputs,
        Alpha&                      alpha) const;
    virtual void evaluate(
        TextureCache&               texture_cache,
        const SourceInputs&         source_inputs,
        foundation::Color3f&        linear_rgb,
        Alpha&                      alpha) const;
    virtual void evaluate(
        TextureCache&               texture_cache,
        const SourceInputs&         source_inputs,
        Spectrum&                   spectrum,
        Alpha&                      alpha) const;

    // Evaluate the source as a uniform source.
    virtual void evaluate_uniform(
        float&                      scalar) const;
    virtual void evaluate_uniform(
        foundation::Color3f&        linear_rgb) const;
    virtual void evaluate_uniform(
        Spectrum&                   spectrum) const;
    virtual void evaluate_uniform(
        Alpha&                      alpha) const;
    virtual void evaluate_uniform(
        foundation::Color3f&        linear_rgb,
        Alpha&                      alpha) const;
    virtual void evaluate_uniform(
        Spectrum&                   spectrum,
        Alpha&                      alpha) const;

  private:
    const bool m_uniform;
};


//
// Source class implementation.
//

inline Source::Source(const bool uniform)
  : m_uniform(uniform)
{
}

inline Source::~Source()
{
}

inline bool Source::is_uniform() const
{
    return m_uniform;
}

inline void Source::evaluate(
    TextureCache&                   texture_cache,
    const SourceInputs&             source_inputs,
    float&                          scalar) const
{
    evaluate_uniform(scalar);
}

inline void Source::evaluate(
    TextureCache&                   texture_cache,
    const SourceInputs&             source_inputs,
    foundation::Color3f&            linear_rgb) const
{
    evaluate_uniform(linear_rgb);
}

inline void Source::evaluate(
    TextureCache&                   texture_cache,
    const SourceInputs&             source_inputs,
    Spectrum&                       spectrum) const
{
    evaluate_uniform(spectrum);
}

inline void Source::evaluate(
    TextureCache&                   texture_cache,
    const SourceInputs&             source_inputs,
    Alpha&                          alpha) const
{
    evaluate_uniform(alpha);
}

inline void Source::evaluate(
    TextureCache&                   texture_cache,
    const SourceInputs&             source_inputs,
    foundation::Color3f&            linear_rgb,
    Alpha&                          alpha) const
{
    evaluate_uniform(linear_rgb, alpha);
}

inline void Source::evaluate(
    TextureCache&                   texture_cache,
    const SourceInputs&             source_inputs,
    Spectrum&                       spectrum,
    Alpha&                          alpha) const
{
    evaluate_uniform(spectrum, alpha);
}

inline void Source::evaluate_uniform(
    float&                          scalar) const
{
    scalar = 0.0f;
}

inline void Source::evaluate_uniform(
    foundation::Color3f&            linear_rgb) const
{
    linear_rgb.set(0.0f);
}

inline void Source::evaluate_uniform(
    Spectrum&                       spectrum) const
{
    spectrum.set(0.0f);
}

inline void Source::evaluate_uniform(
    Alpha&                          alpha) const
{
    alpha.set(0.0f);
}

inline void Source::evaluate_uniform(
    foundation::Color3f&            linear_rgb,
    Alpha&                          alpha) const
{
    evaluate_uniform(linear_rgb);
    evaluate_uniform(alpha);
}

inline void Source::evaluate_uniform(
    Spectrum&                       spectrum,
    Alpha&                          alpha) const
{
    evaluate_uniform(spectrum);
    evaluate_uniform(alpha);
}

}   // namespace renderer
