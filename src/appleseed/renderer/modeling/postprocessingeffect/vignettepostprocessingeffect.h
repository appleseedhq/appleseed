
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
#include "renderer/utility/paramarray.h"
#include "./ipostprocessingeffect.h" // FIXME

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <vector>

// Forward declarations.
namespace renderer  { class Frame; }

namespace renderer
{

//
// Vignette effect applier.
//

class VignetteEffect
  : public IEffect
{
  public:
    // Constructor.
    VignetteEffect(
        const float                     intensity,
        const float                     anisotropy,
        const foundation::Vector2u&     resolution,
        const foundation::Vector2f&     normalization_factor);

    // Delete this instance.
    void release() override;

    // Apply the effect to a given tile.
    void apply(
        const Frame&                frame,
        const size_t                tile_x,
        const size_t                tile_y,
        foundation::IAbortSwitch&   abort_switch) const override;

  private:
    // Settings.
    const float                     m_intensity;
    const float                     m_anisotropy;

    // Context.
    const foundation::Vector2u&     m_resolution;
    const foundation::Vector2f&     m_normalization_factor;
};


//
// Vignette effect applier factory.
//

class VignetteEffectFactory
  : public IEffectFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a new effect applier instance.
    virtual VignetteEffect* create(
        const ParamArray&   effect_params,
        const size_t        thread_index) override;
};

}   // namespace renderer
