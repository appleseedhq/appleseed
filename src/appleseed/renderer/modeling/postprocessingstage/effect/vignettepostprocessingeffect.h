
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
#include "renderer/modeling/postprocessingstage/effect/ipostprocessingeffect.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Frame; }

namespace renderer
{

//
// Vignette-specific parameters.
//

struct VignetteParams {
    // Settings.
    float       intensity;
    float       anisotropy; // 0 = no anisotropy (i.e. perfectly rounded)
                            // 1 = full anisotropy (i.e. respect the frame's aspect ratio)

    // Context.
    float       frame_width;
    float       frame_height;
};


//
// Vignette post-processing effect applier factory.
//

class VignetteApplierFactory
  : public IEffectApplierFactory
{
  public:
    // Constructor.
    VignetteApplierFactory(const VignetteParams& params);

    // Delete this instance.
    void release() override;

    // Return a new effect applier instance.
    IEffectApplier* create() override;

  private:
    const float                     m_intensity;
    const foundation::Vector2f      m_resolution;
    const foundation::Vector2f      m_vignette_resolution;
};

}   // namespace renderer
