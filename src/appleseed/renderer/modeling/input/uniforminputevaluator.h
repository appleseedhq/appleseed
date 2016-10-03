
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_INPUT_UNIFORMINPUTEVALUATOR_H
#define APPLESEED_RENDERER_MODELING_INPUT_UNIFORMINPUTEVALUATOR_H

// appleseed.renderer headers.
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

//
// Uniform input evaluator.
//

class UniformInputEvaluator
{
  public:
    // Evaluate all uniform inputs from a set of inputs, and return the values as
    // an opaque block of memory containing holes corresponding to varying inputs.
    const void* evaluate(const InputArray& inputs);

  private:
    static const size_t ScratchSize = 16 * 1024;    // bytes

    APPLESEED_SIMD4_ALIGN foundation::uint8 m_scratch[ScratchSize];
};


//
// UniformInputEvaluator class implementation.
//

inline const void* UniformInputEvaluator::evaluate(const InputArray& inputs)
{
    inputs.evaluate_uniforms(m_scratch);
    return m_scratch;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_INPUT_UNIFORMINPUTEVALUATOR_H
