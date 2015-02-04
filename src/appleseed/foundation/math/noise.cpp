
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

// Interface header.
#include "noise.h"

// appleseed.foundation headers.
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/permutation.h"

namespace foundation
{

//
// Permutation tables.
//

namespace noise_impl
{
    Permutations g_perms;

    struct InitializePermutations
    {
        InitializePermutations()
        {
            const size_t Size = noise_impl::Permutations::Size;

            MersenneTwister rng;

            random_permutation(Size, g_perms.m_p1, rng);
            for (size_t i = 0; i < Size; ++i)
                g_perms.m_p1[i + Size] = g_perms.m_p1[i];

            random_permutation(Size, g_perms.m_p2, rng);
            for (size_t i = 0; i < Size; ++i)
                g_perms.m_p2[i + Size] = g_perms.m_p2[i];

            random_permutation(Size, g_perms.m_p3, rng);
            for (size_t i = 0; i < Size; ++i)
                g_perms.m_p3[i + Size] = g_perms.m_p3[i];

            random_permutation(Size, g_perms.m_p4, rng);
            for (size_t i = 0; i < Size; ++i)
                g_perms.m_p4[i + Size] = g_perms.m_p4[i];
        }
    };

    static InitializePermutations initialize_permutations;
}

}   // namespace foundation
