
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
// References:
//
//   Tone Mapper, Tizian Zeltner
//   https://github.com/tizian/tonemapper
//
//   "Tone Mapping", Matt Taylor
//   https://64.github.io/tonemapping/
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


// TODO
// - add references for each operator and also note that they are
//   only *barely* modified from: https://github.com/tizian/tonemapper
// - add the option to apply Reinhard operators to RGB channels


//
// ACES (Narkowicz)
//

class AcesNarkowiczApplier
  : public ToneMapApplier
{
  public:
    // Constructor.
    explicit AcesNarkowiczApplier(
        const float     gamma);

    static constexpr float DefaultGamma = 2.2f;

  private:
    const float         m_gamma;

    void tone_map(foundation::Color3f& color) const final;
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

    static constexpr float DefaultA = 0.15f; // 0.22f;
    static constexpr float DefaultB = 0.50f; // 0.30f;
    static constexpr float DefaultC = 0.10f; // 0.10f;
    static constexpr float DefaultD = 0.20f; // 0.20f;
    static constexpr float DefaultE = 0.02f; // 0.01f;
    static constexpr float DefaultF = 0.30f; // 0.30f;
    static constexpr float DefaultW = 11.2f; // 11.2f;
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
        const float     gamma);

    static constexpr float DefaultGamma = 2.2f;

  private:
    const float         m_gamma;

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
        const float     gamma,
        const float     max_white);

    static constexpr float DefaultGamma = 2.2f;
    static constexpr float DefaultMaxWhite = 1.0f;

  private:
    const float         m_gamma;
    const float         m_max_white;    // maximum luminance in the scene

    void tone_map(foundation::Color3f& color) const final;
};

}   // namespace renderer
