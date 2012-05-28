
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.renderer headers.
#include "renderer/kernel/atomkraft/ewa.h"
#include "renderer/kernel/atomkraft/textureobject.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace renderer;

BENCHMARK_SUITE(AtomKraft_EWA)
{
    struct Fixture
    {
        static const size_t TextureWidth = 2048;
        static const size_t TextureHeight = 2048;

        Image                               m_texture;
        TextureObject                       m_texture_object;
        ak::EWAFilter<4, TextureObject>     m_filter;
        Color4f                             m_result;

        Fixture()
          : m_texture(TextureWidth, TextureHeight, TextureWidth, TextureHeight, 4, PixelFormatFloat)
          , m_texture_object(m_texture)
        {
        }
    };

    BENCHMARK_CASE_F(Filter_NoClamping, Fixture)
    {
        m_filter.filter(
            m_texture_object,
            1020.0f,                        // center x
            1020.0f,                        // center y
            10.0f,                          // du/dx
            0.0f,                           // du/dy
            0.0f,                           // dv/dx
            10.0f,                          // dv/dy
            100.0f,                         // max radius
            &m_result[0]);
    }

    BENCHMARK_CASE_F(Filter_Clamping, Fixture)
    {
        m_filter.filter(
            m_texture_object,
            1020.0f,                        // center x
            1020.0f,                        // center y
            10.0f,                          // du/dx
            0.0f,                           // du/dy
            0.0f,                           // dv/dx
            10.0f,                          // dv/dy
            5.0f,                           // max radius
            &m_result[0]);
    }
}
