
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
#include <functional>
#include <string>
#include <vector>

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
//   Tone Mapping, Matt Taylor
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

    // Return a string identifying the tone mapping operator.
    // virtual const char* get_operator_id() const = 0;

    // Return a human-readable string identifying the tone mapping operator.
    // virtual const char* get_operator_name() const = 0;

  protected:
    // Apply a tone mapping curve to a given pixel.
    virtual void tone_map(foundation::Color3f& color) const = 0;
};

//
// ACES (Narkowicz)
//

class AcesNarkowiczApplier
  : public ToneMapApplier
{
  public:
    static constexpr float DefaultGamma = 2.2f;

    // Constructor.
    explicit AcesNarkowiczApplier(
        const float     gamma);

    // Operator metadata.
    static const char* get_operator_id();// const;
    static const char* get_operator_name();// const;
    static void add_operator_metadata(foundation::DictionaryArray& metadata);

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
    static constexpr float DefaultGamma = 2.2f;
    static constexpr float DefaultMaxWhite = 1.0f;

    // Constructor.
    explicit ReinhardExtendedApplier(
        const float     gamma,
        const float     max_white);

    // Operator metadata.
    static const char* get_operator_id();// const;
    static const char* get_operator_name();// const;
    static void add_operator_metadata(foundation::DictionaryArray& metadata);

  private:
    const float         m_gamma;
    const float         m_max_white;    // maximum luminance in the scene

    void tone_map(foundation::Color3f& color) const final;
};

#if 0
// TODO add references for each and note that
//      they are only slightly modified from:
//      https://github.com/tizian/tonemapper
//      (also, include the original license)

class AcesUnrealApplier
  : public ToneMapApplier
{
  public:
    explicit AcesUnrealApplier();
};

class AcesNarkowiczApplier
  : public ToneMapApplier
{
  public:
    explicit AcesNarkowiczApplier(
        const float     gamma);

  private:
    const float         m_gamma;
};

class FilmicHejlApplier
  : public ToneMapApplier
{
  public:
    explicit FilmicHejlApplier();
};

class ReinhardApplier
  : public ToneMapApplier
{
  public:
    explicit ReinhardApplier(
        const float     gamma);

  private:
    const float         m_gamma;
};

class ReinhardExtendedApplier
  : public ToneMapApplier
{
  public:
    explicit ReinhardExtendedApplier(
        const float     gamma,
        const float     max_white);

  private:
    const float         m_gamma;
    const float         m_max_white;    // maximum luminance in the scene
};
#endif

}   // namespace renderer
