
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class TextureCache; }

namespace renderer
{

//
// An arena is the storage for inputs values and additional data such as precomputed values.
//

class Arena
{
  public:
    enum { DataSize = 32 * 1024 };  // bytes

    const foundation::uint8* data() const;
    foundation::uint8* data();

    template <typename T> const T& as() const;
    template <typename T> T& as();

  private:
    APPLESEED_SIMD4_ALIGN foundation::uint8 m_data[DataSize];
};


//
// Input evaluator.
//

class InputEvaluator
{
  public:
    // Constructor.
    explicit InputEvaluator(TextureCache& texture_cache);

    // Evaluate a set of inputs, and return the values as an opaque block of memory.
    void evaluate(
        const InputArray&           inputs,
        const foundation::Vector2f& uv,
        Arena&                      arena,
        const size_t                offset = 0);

  private:
    TextureCache& m_texture_cache;
};


//
// Arena class implementation.
//

inline const foundation::uint8* Arena::data() const
{
    return m_data;
}

inline foundation::uint8* Arena::data()
{
    return m_data;
}

template <typename T>
inline const T& Arena::as() const
{
    return *reinterpret_cast<const T*>(m_data);
}

template <typename T>
inline T& Arena::as()
{
    return *reinterpret_cast<T*>(m_data);
}


//
// InputEvaluator class implementation.
//

inline InputEvaluator::InputEvaluator(TextureCache& texture_cache)
  : m_texture_cache(texture_cache)
{
}

inline void InputEvaluator::evaluate(
    const InputArray&               inputs,
    const foundation::Vector2f&     uv,
    Arena&                          arena,
    const size_t                    offset)
{
    inputs.evaluate(m_texture_cache, uv, arena.data(), offset);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_INPUT_INPUTEVALUATOR_H
