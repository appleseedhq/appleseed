
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "renderer/modeling/postprocessingstage/effect/imageeffectapplier.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/vector.h"
#include "foundation/utility/registrar.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Image; }
namespace foundation    { class DictionaryArray; }

namespace renderer
{

//
// Tone map post-processing effect appliers.
//

//
// References:
//
//   Tone Mapper, Tizian Zeltner
//   https://github.com/tizian/tonemapper
//
//   "Tone Mapping", Matt Taylor (64)
//   https://64.github.io/tonemapping/
//
//   "Filmic Tonemapping Operators", John Hable
//   http://filmicworlds.com/blog/filmic-tonemapping-operators/
//
//   "Filmic Tonemapping with Piecewise Power Curves", John Hable
//   http://filmicworlds.com/blog/filmic-tonemapping-with-piecewise-power-curves/
//

class ToneMapApplier
  : public ImageEffectApplier
{
  public:
    // Delete this instance.
    void release() override;

    // Apply a tone mapping curve to a given tile.
    virtual void apply(
        foundation::Image&      image,
        const std::size_t       tile_x,
        const std::size_t       tile_y) const override;

  protected:
    // Apply a tone mapping curve to a given pixel.
    virtual void tone_map(foundation::Color3f& color) const = 0;
};

class LinearApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit LinearApplier();

  private:
    void tone_map(foundation::Color3f& color) const final;
};

//
// Most of the following tone mapping operators were taken from
// Tizian Zeltner's Tone Mapper application, with minimal code changes.
// The original license follows:
//

// The MIT License (MIT)
//
// Copyright (c) 2016 Tizian Zeltner
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

//
// ACES (Narkowicz)
//

class AcesNarkowiczApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit AcesNarkowiczApplier(
        const float     exposure_bias);

    // Default values.
    static constexpr float DefaultExposureBias = 0.8f;

  private:
    const float         m_exposure_bias;

    void tone_map(foundation::Color3f& color) const final;    // linear white point (minimal value that is mapped to 1)
};

//
// ACES (Unreal)
//

class AcesUnrealApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit AcesUnrealApplier();

  private:
    void tone_map(foundation::Color3f& color) const final;
};

//
// Filmic (Hejl)
//

class FilmicHejlApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit FilmicHejlApplier();

  private:
    void tone_map(foundation::Color3f& color) const final;
};

//
// Filmic (Piecewise)
//

class FilmicPiecewiseApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit FilmicPiecewiseApplier(
        const float     toe_strength,       // 0..1 ratio
        const float     toe_length,         // 0..1 ratio
        const float     shoulder_strength,  // 0..1 ratio (defines the white point)
        const float     shoulder_length,    // in F stops
        const float     shoulder_angle);    // 0..1 ratio

    // Default values.
    static constexpr float DefaultToeStrength = 0.0f;
    static constexpr float DefaultToeLength = 0.5f;
    static constexpr float DefaultShoulderStrength = 0.0f;
    static constexpr float DefaultShoulderLength = 0.5f;
    static constexpr float DefaultShoulderAngle = 0.0f;

  private:
    struct PowerCurve
    {
        float offset_x;
        float offset_y;
        float scale_x;    // either 1 or -1
        float scale_y;
        float lnA;
        float B;

        PowerCurve() = default;
        PowerCurve(const PowerCurve& other) = default;

        float eval(const float x) const;
    };

    const float         m_x0;               // (x0, y0) = toe segment end
    const float         m_y0;
    const float         m_x1;               // (x1, y1) = shoulder segment start
    const float         m_y1;
    const float         m_W;                // white point (i.e. values above W are clipped)
    const float         m_rcp_W;
    const float         m_overshoot_x;      // larger overshoot values add extra space to allow for a steeper shoulder
    const float         m_overshoot_y;
    PowerCurve          m_segments[3];      // segments that piecewisely define the tone curve

    enum Segment { TOE = 0, LINEAR = 1, SHOULDER = 2 };

    float eval_at(const float x) const;
    void tone_map(foundation::Color3f& color) const final;
};

//
// Filmic (Uncharted)
//

class FilmicUnchartedApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit FilmicUnchartedApplier(
        const float     A,
        const float     B,
        const float     C,
        const float     D,
        const float     E,
        const float     F,
        const float     W,
        const float     exposure_bias);

    //
    // Note: below are the default values from the original presentation (slides 141-142) [1].
    // John Hable later changed the values of A, B and E slightly, on a follow-up blog post [2].
    // Also, MJP uses different values for A, B, C, D and W on his implementation of Hable's operator [3].
    //
    // References:
    //
    //   [1] https://www.slideshare.net/ozlael/hable-john-uncharted2-hdr-lighting
    //   [2] http://filmicworlds.com/blog/filmic-tonemapping-operators/
    //   [3] https://github.com/TheRealMJP/BakingLab/blob/master/BakingLab/ToneMapping.hlsl#L73
    //

    // Default values.
    static constexpr float DefaultA = 0.22f;
    static constexpr float DefaultB = 0.30f;
    static constexpr float DefaultC = 0.10f;
    static constexpr float DefaultD = 0.20f;
    static constexpr float DefaultE = 0.01f;
    static constexpr float DefaultF = 0.30f;
    static constexpr float DefaultW = 11.2f;
    static constexpr float DefaultExposureBias = 2.0f;

  private:
    const float         m_A;                // shoulder strength
    const float         m_B;                // linear strength
    const float         m_C;                // linear angle
    const float         m_D;                // toe strength
    const float         m_E;                // toe numerator
    const float         m_F;                // toe denominator
    const float         m_W;                // linear white point (minimal value that is mapped to 1)
    const float         m_exposure_bias;

    void tone_map(foundation::Color3f& color) const final;

    float uncharted_tone_map(const float x) const;
    foundation::Color3f uncharted_tone_map(const foundation::Color3f& x) const;
};

//
// Reinhard (Simple)
//

class ReinhardApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit ReinhardApplier(
        const bool      use_luminance);

    // Default values.
    static constexpr bool DefaultUseLuminance = true;

  private:
    const bool          m_use_luminance;    // true  = apply the curve to luminance values only
                                            // false = apply the curve to each RGB channel individually

    void tone_map(foundation::Color3f& color) const final;
};

//
// Reinhard (Extended)
//

class ReinhardExtendedApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit ReinhardExtendedApplier(
        const float     max_white,
        const bool      use_luminance);

    // Default values.
    static constexpr float DefaultMaxWhite = 1.0f;
    static constexpr bool DefaultUseLuminance = true;

  private:
    const float         m_max_white;        // maximum luminance in the scene
    const bool          m_use_luminance;    // true  = apply the curve to luminance values only
                                            // false = apply the curve to each RGB channel individually

    void tone_map(foundation::Color3f& color) const final;
};

}   // namespace renderer
