
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <memory>

using namespace foundation;
using namespace std;

TEST_SUITE(EWAFilteringExploration)
{
    auto_ptr<Image> make_checkerboard_texture(
        const size_t        width,
        const size_t        height,
        const size_t        scale = 16)
    {
        auto_ptr<Image> texture(new Image(width, height, width, height, 3, PixelFormatFloat));

        for (size_t y = 0; y < height; ++y)
        {
            for (size_t x = 0; x < width; ++x)
            {
                const size_t on = ((x / scale) ^ (y / scale)) & 1;
                const float color = static_cast<float>(on);
                texture->set_pixel(x, y, Color3f(color));
            }
        }

        return texture;
    }

    TEST_CASE(MakeCheckerboardTexture)
    {
        auto_ptr<Image> texture = make_checkerboard_texture(512, 512);

        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa_make_checkerboard_texture.png", *texture.get());
    }

    TEST_CASE(EllipseInclusionTesting)
    {
        const int Width = 512;
        const int Height = 512;

        auto_ptr<Image> texture = make_checkerboard_texture(size_t(Width), size_t(Height));

        // Parameters of the ellipse.
        const Vector2i Center(Width / 2, Height / 2);
        const Vector2i Du(Width / 4, 0);
        const Vector2i Dv(0, Height / 3);

        // Compute the ellipse coefficients.
        const double A = Du.y * Du.y + Dv.y * Dv.y;
        const double B = -2.0 * (Du.x * Du.y + Dv.x * Dv.y);
        const double C = Du.x * Du.x + Dv.x * Dv.x;

        // Compute the inclusion threshold.
        const double F = square(Du.x * Dv.y - Dv.x * Du.y);

        // Make sure we have an elliptical paraboloid, concave upward.
        assert(A > 0.0);
        assert(A * C - B * B / 4.0 > 0.0);

        const double Ddq = 2.0 * A;
        const double u = 0 - Center.x;

        for (int y = 0; y < Height; ++y)
        {
            const double v = y - Center.y;

            double dq = A * (2.0 * u + 1.0) + B * v;
            double q = (C * v + B * u) * v + A * u * u;

            for (int x = 0; x < Width; ++x)
            {
                if (q < F)
                {
                    Color3f base;
                    texture->get_pixel(size_t(x), size_t(y), base);

                    const Color3f Overlay(0.0f, 0.0f, 1.0f);

                    texture->set_pixel(size_t(x), size_t(y), lerp(base, Overlay, 0.3f));
                }

                q += dq;
                dq += Ddq;
            }
        }

        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa_ellipse_inclusion_testing.png", *texture.get());
    }

    Color3f ewa(
        const Image&        texture,
        const Vector2d&     center,
        const Vector2d&     du,
        const Vector2d&     dv)
    {
        return Color3f(0.0f);
    }

    TEST_CASE(EWA)
    {
        auto_ptr<Image> texture = make_checkerboard_texture(512, 512);

        const Vector2d Center(0.5, 0.5);
        const Vector2d Du(0.1, 0.0);
        const Vector2d Dv(0.0, 0.2);

        const Color3f result = ewa(*texture.get(), Center, Du, Dv);
    }
}
