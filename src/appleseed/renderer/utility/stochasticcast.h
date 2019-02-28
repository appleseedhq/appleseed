
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>

namespace renderer
{

//
// Randomly cast a non-negative scalar n to an integer such that the expected value of that integer is n.
//

template <typename RNG, typename Int, typename Float>
inline Int stochastic_cast(RNG& rng, const Float n)
{
    assert(n >= Float(0.0));

    Int i = foundation::truncate<Int>(n);

    const Float r = n - i;

    if (r > Float(0.0))
    {
        if (foundation::rand2<Float>(rng) < n - i)
            ++i;
    }

    return i;
}

template <typename Int, typename Float>
inline Int stochastic_cast(SamplingContext& sampling_context, const Float n)
{
    assert(n >= Float(0.0));

    Int i = foundation::truncate<Int>(n);

    const Float r = n - i;

    if (r > Float(0.0))
    {
        sampling_context.split_in_place(1, 1);

        if (sampling_context.next2<Float>() < n - i)
            ++i;
    }

    return i;
}

}   // namespace renderer
