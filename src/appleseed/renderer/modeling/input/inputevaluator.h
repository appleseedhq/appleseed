
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_INPUT_INPUTEVALUATOR_H
#define APPLESEED_RENDERER_MODELING_INPUT_INPUTEVALUATOR_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/input/inputarray.h"

// Forward declarations.
namespace renderer      { class InputParams; }
namespace renderer      { class TextureCache; }

namespace renderer
{

//
// Input evaluator.
//

class InputEvaluator
{
  public:
    // Constructor.
    explicit InputEvaluator(TextureCache& texture_cache);

    // Evaluate a set of inputs, and return the values as an opaque block of memory.
    const void* evaluate(
        const InputArray&   inputs,
        const InputParams&  params);
    template <typename T>
    const T* evaluate(
        const InputArray&   inputs,
        const InputParams&  params);

  private:
    static const size_t ScratchSize = 16 * 1024;    // bytes

    TextureCache&       m_texture_cache;

    FOUNDATION_ALIGN_SSE_VARIABLE
    foundation::uint8   m_scratch[ScratchSize];
};


//
// InputEvaluator class implementation.
//

// Constructor.
inline InputEvaluator::InputEvaluator(TextureCache& texture_cache)
  : m_texture_cache(texture_cache)
{
}

// Evaluate a set of inputs.
inline const void* InputEvaluator::evaluate(
    const InputArray&   inputs,
    const InputParams&  params)
{
    inputs.evaluate(
        m_texture_cache,
        params,
        m_scratch);

    return m_scratch;
}
template <typename T>
inline const T* InputEvaluator::evaluate(
    const InputArray&   inputs,
    const InputParams&  params)
{
    inputs.evaluate(
        m_texture_cache,
        params,
        m_scratch);

    return reinterpret_cast<const T*>(m_scratch);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_INPUT_INPUTEVALUATOR_H
