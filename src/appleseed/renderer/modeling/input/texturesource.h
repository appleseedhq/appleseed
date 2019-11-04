
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
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/modeling/scene/textureinstance.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace renderer      { class TextureCache; }

namespace renderer
{

//
// Texture source.
//

class TextureSource
  : public Source
{
  public:
    // Constructor.
    TextureSource(
        const foundation::UniqueID          assembly_uid,
        const TextureInstance&              texture_instance);

    // Retrieve the texture instance used by this source.
    const TextureInstance& get_texture_instance() const;

    // Compute a signature unique to this source.
    std::uint64_t compute_signature() const override;

    // Return hints allowing to treat this source as one of another type.
    Hints get_hints() const override;

    // Evaluate the source at a given shading point.
    void evaluate(
        TextureCache&                       texture_cache,
        const SourceInputs&                 source_inputs,
        float&                              scalar) const override;
    void evaluate(
        TextureCache&                       texture_cache,
        const SourceInputs&                 source_inputs,
        foundation::Color3f&                linear_rgb) const override;
    void evaluate(
        TextureCache&                       texture_cache,
        const SourceInputs&                 source_inputs,
        Spectrum&                           spectrum) const override;
    void evaluate(
        TextureCache&                       texture_cache,
        const SourceInputs&                 source_inputs,
        Alpha&                              alpha) const override;
    void evaluate(
        TextureCache&                       texture_cache,
        const SourceInputs&                 source_inputs,
        foundation::Color3f&                linear_rgb,
        Alpha&                              alpha) const override;
    void evaluate(
        TextureCache&                       texture_cache,
        const SourceInputs&                 source_inputs,
        Spectrum&                           spectrum,
        Alpha&                              alpha) const override;

  private:
    const foundation::UniqueID              m_assembly_uid;
    const TextureInstance&                  m_texture_instance;
    const foundation::UniqueID              m_texture_uid;
    const foundation::CanvasProperties      m_texture_props;
    const foundation::Transformf            m_texture_transform;
    const float                             m_scalar_canvas_width;
    const float                             m_scalar_canvas_height;
    const float                             m_max_x;
    const float                             m_max_y;

    // Apply the texture instance transform to UV coordinates.
    foundation::Vector2f apply_transform(
        const foundation::Vector2f&         uv) const;

    // Retrieve a given texel. Return a color in the linear RGB color space.
    foundation::Color4f get_texel(
        TextureCache&                       texture_cache,
        const size_t                        ix,
        const size_t                        iy) const;

    // Retrieve a 2x2 block of texels. Texels are expressed in the linear RGB color space.
    void get_texels_2x2(
        TextureCache&                       texture_cache,
        const int                           ix,
        const int                           iy,
        foundation::Color4f&                t00,
        foundation::Color4f&                t10,
        foundation::Color4f&                t01,
        foundation::Color4f&                t11) const;

    // Sample the texture. Return a color in the linear RGB color space.
    foundation::Color4f sample_texture(
        TextureCache&                       texture_cache,
        const foundation::Vector2f&         uv) const;

    // Compute an alpha value given a linear RGBA color and the alpha mode of the texture instance.
    void evaluate_alpha(
        const foundation::Color4f&          color,
        Alpha&                              alpha) const;
};


//
// TextureSource class implementation.
//

inline const TextureInstance& TextureSource::get_texture_instance() const
{
    return m_texture_instance;
}

inline void TextureSource::evaluate(
    TextureCache&                           texture_cache,
    const SourceInputs&                     source_inputs,
    float&                                  scalar) const
{
    const foundation::Color4f color = sample_texture(texture_cache, foundation::Vector2f(source_inputs.m_uv_x, source_inputs.m_uv_y));
    scalar = color[0];
}

inline void TextureSource::evaluate(
    TextureCache&                           texture_cache,
    const SourceInputs&                     source_inputs,
    foundation::Color3f&                    linear_rgb) const
{
    const foundation::Color4f color = sample_texture(texture_cache, foundation::Vector2f(source_inputs.m_uv_x, source_inputs.m_uv_y));
    linear_rgb = color.rgb();
}

inline void TextureSource::evaluate(
    TextureCache&                           texture_cache,
    const SourceInputs&                     source_inputs,
    Spectrum&                               spectrum) const
{
    const foundation::Color4f color = sample_texture(texture_cache, foundation::Vector2f(source_inputs.m_uv_x, source_inputs.m_uv_y));
    spectrum.set(color.rgb(), g_std_lighting_conditions, Spectrum::Reflectance);
}

inline void TextureSource::evaluate(
    TextureCache&                           texture_cache,
    const SourceInputs&                     source_inputs,
    Alpha&                                  alpha) const
{
    const foundation::Color4f color = sample_texture(texture_cache, foundation::Vector2f(source_inputs.m_uv_x, source_inputs.m_uv_y));
    evaluate_alpha(color, alpha);
}

inline void TextureSource::evaluate(
    TextureCache&                           texture_cache,
    const SourceInputs&                     source_inputs,
    foundation::Color3f&                    linear_rgb,
    Alpha&                                  alpha) const
{
    const foundation::Color4f color = sample_texture(texture_cache, foundation::Vector2f(source_inputs.m_uv_x, source_inputs.m_uv_y));
    linear_rgb = color.rgb();
    evaluate_alpha(color, alpha);
}

inline void TextureSource::evaluate(
    TextureCache&                           texture_cache,
    const SourceInputs&                     source_inputs,
    Spectrum&                               spectrum,
    Alpha&                                  alpha) const
{
    const foundation::Color4f color = sample_texture(texture_cache, foundation::Vector2f(source_inputs.m_uv_x, source_inputs.m_uv_y));
    spectrum.set(color.rgb(), g_std_lighting_conditions, Spectrum::Reflectance);
    evaluate_alpha(color, alpha);
}

inline void TextureSource::evaluate_alpha(
    const foundation::Color4f&              color,
    Alpha&                                  alpha) const
{
    switch (m_texture_instance.get_effective_alpha_mode())
    {
      case TextureAlphaModeAlphaChannel:
        alpha.set(color.a);
        break;

      case TextureAlphaModeLuminance:
        alpha.set(average_value(color.rgb()));
        break;

      assert_otherwise;
    }
}

}   // namespace renderer
