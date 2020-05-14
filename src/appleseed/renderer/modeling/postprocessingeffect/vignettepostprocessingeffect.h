
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
// Vignette-specific settings and context.
//

struct VignetteParams {
    // Settings.
    const float intensity;
    const float anisotropy;

    // Context.
    const foundation::Vector2u resolution;
    const foundation::Vector2f normalization_factor;
};


//
// Vignette post-processing effect applier factory.
//

class VignetteEffectApplierFactory
  : public IEffectApplierFactory
{
  public:
    // Constructor.
    VignetteEffectApplierFactory(const VignetteParams& params);

    // Delete this instance.
    void release() override;

    // Return a new effect applier instance.
    IEffectApplier* create() override;

    // Return a new effect applier instance.
    static IEffectApplier* create(const VignetteParams& params);

  private:
    const VignetteParams m_params;
};

}   // namespace renderer
