
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

// Forward declarations.
namespace foundation    { class Image; }

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

  protected:
    typedef std::function<void(foundation::Color3f&)> ToneMapFunction;

    // Constructor.
    ToneMapApplier(
        const ToneMapFunction   tone_map);

    const ToneMapFunction       tone_map;
};

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

}   // namespace renderer
