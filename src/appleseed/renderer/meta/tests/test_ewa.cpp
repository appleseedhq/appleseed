
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

using namespace foundation;
using namespace std;

TEST_SUITE(EWAFilteringExploration)
{
    void draw_checkerboard(
        Image&          image,
        const size_t    scale = 16,
        const Color3f&  color1 = Color3f(0.0f),
        const Color3f&  color2 = Color3f(1.0f))
    {
        const size_t width = image.properties().m_canvas_width;
        const size_t height = image.properties().m_canvas_height;

        for (size_t y = 0; y < height; ++y)
        {
            for (size_t x = 0; x < width; ++x)
            {
                const int b = ((x / scale) ^ (y / scale)) & 1;
                image.set_pixel(x, y, b ? color1 : color2);
            }
        }
    }

    void draw_line(
        Image&          image,
        const Vector2i& v0,
        const Vector2i& v1,
        const Color3f&  color)
    {
        const int dx = v1.x - v0.x;
        const int ax = 2 * (dx < 0 ? -dx : dx);
        const int sx = dx < 0 ? -1 : 1;

        const int dy = v1.y - v0.y;
        const int ay = 2 * (dy < 0 ? -dy : dy);
        const int sy = dy < 0 ? -1 : 1;

        int x = v0.x;
        int y = v0.y;

        if (ax > ay)
        {
            int d = ay - ax / 2;

            while (true)
            {
                image.set_pixel(x, y, color);

                if (x == v1.x)
                    return;

                if (d >= 0)
                {
                    y += sy;
                    d -= ax;
                }

                x += sx;
                d += ay;
            }
        }
        else
        {
            int d = ax - ay / 2;

            while (true)
            {
                image.set_pixel(x, y, color);

                if (y == v1.y)
                    return;

                if (d >= 0)
                {
                    x += sx;
                    d -= ay;
                }

                y += sy;
                d += ax;
            }
        }
    }

    void trapezoid_to_ellipse(
        const Vector2i& v00,
        const Vector2i& v10,
        const Vector2i& v01,
        const Vector2i& v11,
        Vector2i&       center,
        Vector2i&       du,
        Vector2i&       dv)
    {
        const Vector2i middle_00_10 = (v00 + v10) / 2;
        const Vector2i middle_01_11 = (v01 + v11) / 2;
        const Vector2i middle_10_11 = (v10 + v11) / 2;

        center = (middle_00_10 + middle_01_11) / 2;

        du = middle_10_11 - center;
        dv = middle_01_11 - center;
    }

    TEST_CASE(EllipseInclusionTesting)
    {
        const int Width = 512;
        const int Height = 512;

        Image image(Width, Height, Width, Height, 3, PixelFormatFloat);
        draw_checkerboard(image, 32, Color3f(0.3f), Color3f(1.0f));

        // Corners of the trapezoid.
        const Vector2i V00(150, 300);
        const Vector2i V10(300, 380);
        const Vector2i V01(200, 60);
        const Vector2i V11(480, 230);

        // Draw the trapezoid.
        const Color3f TrapezoidColor(1.0f, 0.0f, 1.0f);
        draw_line(image, V00, V10, TrapezoidColor);
        draw_line(image, V10, V11, TrapezoidColor);
        draw_line(image, V11, V01, TrapezoidColor);
        draw_line(image, V01, V00, TrapezoidColor);

        // Compute the parameters of the inscribed ellipse.
        Vector2i center, du, dv;
        trapezoid_to_ellipse(V00, V10, V01, V11, center, du, dv);

        // Draw the axes of the ellipse.
        draw_line(image, center, center + du, Color3f(1.0f, 0.0, 0.0));
        draw_line(image, center, center + dv, Color3f(0.0f, 1.0, 0.0));

        // Compute the ellipse coefficients.
        const double A = du.y * du.y + dv.y * dv.y;
        const double B = -2.0 * (du.x * du.y + dv.x * dv.y);
        const double C = du.x * du.x + dv.x * dv.x;

        // Compute the inclusion threshold.
        const double F = square(du.x * dv.y - dv.x * du.y);

        // Make sure we have an elliptical paraboloid, concave upward.
        assert(A > 0.0);
        assert(A * C - B * B / 4.0 > 0.0);

        const double Ddq = 2.0 * A;
        const double u = 0 - center.x;

        for (int y = 0; y < Height; ++y)
        {
            const double v = y - center.y;

            double dq = A * (2.0 * u + 1.0) + B * v;
            double q = (C * v + B * u) * v + A * u * u;

            for (int x = 0; x < Width; ++x)
            {
                if (q < F)
                {
                    Color3f base;
                    image.get_pixel(size_t(x), size_t(y), base);

                    const Color3f EllipseColor(0.0f, 0.0f, 1.0f);

                    image.set_pixel(size_t(x), size_t(y), lerp(base, EllipseColor, 0.2f));
                }

                q += dq;
                dq += Ddq;
            }
        }

        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa_ellipse_inclusion_testing.png", image);
    }

    Color3f ewa(
        const Image&    texture,
        const Vector2d& center,
        const Vector2d& du,
        const Vector2d& dv)
    {
        return Color3f(0.0f);
    }

    TEST_CASE(EWA)
    {
        const size_t Width = 512;
        const size_t Height = 512;

        Image texture(Width, Height, Width, Height, 3, PixelFormatFloat);
        draw_checkerboard(texture);

        const Vector2d Center(0.5, 0.5);
        const Vector2d Du(0.1, 0.0);
        const Vector2d Dv(0.0, 0.2);

        const Color3f result = ewa(texture, Center, Du, Dv);
    }
}
