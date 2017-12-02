
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

#ifndef APPLESEED_FOUNDATION_IMAGE_DRAWING_H
#define APPLESEED_FOUNDATION_IMAGE_DRAWING_H

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// A collection of drawing functions.
//

class Drawing
{
  public:
    // Draw an antialiased 4x4 pixel dot.
    template <typename T, size_t N>
    static void draw_dot(
        Image&              image,
        const Vector2d&     position,
        const Color<T, N>&  color);
};


//
// Drawing class implementation.
//

template <typename T, size_t N>
void Drawing::draw_dot(
    Image&                  image,
    const Vector2d&         position,
    const Color<T, N>&      color)
{
    static size_t DotIntensity[16] =
    {
         24, 171, 178,  48,
        159, 255, 255, 207,
        183, 255, 255, 231,
         64, 227, 241, 112
    };

    const CanvasProperties& props = image.properties();

    const int w = static_cast<int>(props.m_canvas_width);
    const int h = static_cast<int>(props.m_canvas_height);

    const int cx = static_cast<int>(position.x * props.m_canvas_width);
    const int cy = static_cast<int>(position.y * props.m_canvas_height);

    for (int y = 0; y < 4; ++y)
    {
        for (int x = 0; x < 4; ++x)
        {
            const int ix = cx + x - 2;
            const int iy = cy + y - 2;

            if (ix >= 0 && iy >= 0 && ix < w && iy < h)
            {
                const T intensity = DotIntensity[y * 4 + x] * T(1.0 / 255.0);

                Color<T, N> background;
                image.get_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    background);

                image.set_pixel(
                    static_cast<size_t>(ix),
                    static_cast<size_t>(iy),
                    background * (T(1.0) - intensity) + color * intensity);
            }
        }
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_DRAWING_H
