
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
#include <map>
#include <string>
#include <array>

// Forward declarations.
namespace foundation    { class Image; }

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


struct ToneMapOperator
{
    struct Parameter
    {
        const char*     id;
        const char*     name;
        const float     default_value;
        const float     min_value;
        const float     max_value;

        Parameter() = default;
    };

    const char*     id;
    const char*     name;
    const Parameter parameters[];

    ToneMapOperator() = default;
};

#if 0
struct ToneMapOperatorParameter
{
    const char*     id;
    const char*     name;
    const float     default_value;
    const float     min_value;
    const float     max_value;
};
#endif


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

class AcesNarkowiczApplier
  : public ToneMapApplier
{
  public:
    static constexpr const ToneMapOperator Operator
    {
        "aces_narkowicz",
        "ACES (Narkowicz)",

        // Curve parameters.
        {
            { "gamma", "Gamma", 2.2f, 1.0f, 10.0f }
        }
    };

    // Constructor.
    explicit AcesNarkowiczApplier(
        const float     gamma);

  protected:
    void tone_map(foundation::Color3f& color) const override; // TODO use final (?)

  private:
    const float         m_gamma;
};


class ReinhardExtendedApplier
  : public ToneMapApplier
{
  public:
    static constexpr const ToneMapOperator Operator
    {
        "reinhard_extended",
        "Reinhard (Extended)",

        // Curve parameters.
        {
            { "gamma", "Gamma", 2.2f, 1.0f, 10.0f },
            { "l_max", "Lmax", 1.0f, 0.0f, 10000.0f } // FIXME these values depend on the image
        }
    };

    explicit ReinhardExtendedApplier(
        const float     gamma,
        const float     max_white);

  private:
    const float         m_gamma;
    const float         m_max_white;    // maximum luminance in the scene

    void tone_map(foundation::Color3f& color) const override;
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
