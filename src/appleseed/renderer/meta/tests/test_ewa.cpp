
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
#include "foundation/math/aabb.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstring>

using namespace foundation;
using namespace std;

TEST_SUITE(EWAFilteringExploration)
{
    //---------------------------------------------------------------------------------------------
    //--- Drawing Primitives Used For Debugging ---------------------------------------------------
    //---------------------------------------------------------------------------------------------

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
                const size_t b = ((x / scale) ^ (y / scale)) & 1;
                image.set_pixel(x, y, b ? color1 : color2);
            }
        }
    }

    bool is_inside(
        const Image&    image, 
        const int       x,
        const int       y)
    {
        const CanvasProperties& props = image.properties();

        return
            x >= 0 &&
            y >= 0 &&
            x < static_cast<int>(props.m_canvas_width) &&
            y < static_cast<int>(props.m_canvas_height);
    }

    void draw_line(
        Image&          image,
        const Vector2i& v0,
        const Vector2i& v1,
        const Color3f&  color)
    {
        const int dx = v1.x - v0.x;
        const int dy = v1.y - v0.y;

        const int ax = 2 * (dx < 0 ? -dx : dx);
        const int ay = 2 * (dy < 0 ? -dy : dy);

        const int sx = dx < 0 ? -1 : 1;
        const int sy = dy < 0 ? -1 : 1;

        int x = v0.x;
        int y = v0.y;

        if (ax > ay)
        {
            int d = ay - ax / 2;

            while (true)
            {
                if (is_inside(image, x, y))
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
                if (is_inside(image, x, y))
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

    void draw_box(
        Image&          image,
        const AABB2i&   bbox,
        const Color3f&  color)
    {
        draw_line(image, Vector2i(bbox.min.x, bbox.min.y), Vector2i(bbox.max.x, bbox.min.y), color);
        draw_line(image, Vector2i(bbox.max.x, bbox.min.y), Vector2i(bbox.max.x, bbox.max.y), color);
        draw_line(image, Vector2i(bbox.max.x, bbox.max.y), Vector2i(bbox.min.x, bbox.max.y), color);
        draw_line(image, Vector2i(bbox.min.x, bbox.max.y), Vector2i(bbox.min.x, bbox.min.y), color);
    }

    void tint_pixel(
        Image&          image,
        const int       x,
        const int       y,
        const Color3f&  color,
        const float     intensity)
    {
        if (is_inside(image, x, y))
        {
            Color3f base;
            image.get_pixel(x, y, base);
            image.set_pixel(x, y, lerp(base, color, intensity));
        }
    }

    //---------------------------------------------------------------------------------------------
    //--- Reference EWA Filter Implementation -----------------------------------------------------
    //--- http://www.cs.cmu.edu/~ph/texfund/texfund.pdf -------------------------------------------
    //---------------------------------------------------------------------------------------------

    class EWAFilterRef
    {
      public:
        explicit EWAFilterRef(
            Image&          debug_image)
          : m_debug_image(debug_image)
        {
            compute_weights();
        }

        // The lookup point is expressed in [0,width)x[0,height) (note: open on the right).
        Color3f filter_bilinear(
            const Image&    texture,
            const float     texture_gamma,
            const Vector2f& point) const
        {
            // Fetch neighboring texels.
            const CanvasProperties& props = texture.properties();
            const Vector2f p(
                point.x / props.m_canvas_width * (props.m_canvas_width - 1),
                point.y / props.m_canvas_height * (props.m_canvas_height - 1));
            const int ix0 = truncate<int>(p.x);
            const int iy0 = truncate<int>(p.y);
            const int ix1 = min(ix0 + 1, static_cast<int>(props.m_canvas_width - 1));
            const int iy1 = min(iy0 + 1, static_cast<int>(props.m_canvas_height - 1));
            Color3f c00, c10, c01, c11;
            texture.get_pixel(ix0, iy0, c00);
            texture.get_pixel(ix1, iy0, c10);
            texture.get_pixel(ix0, iy1, c01);
            texture.get_pixel(ix1, iy1, c11);

            // Ungamma texels.
            if (texture_gamma != 1.0f)
            {
                c00 = pow(c00, texture_gamma);
                c10 = pow(c10, texture_gamma);
                c01 = pow(c01, texture_gamma);
                c11 = pow(c11, texture_gamma);
            }

            // Compute weights.
            const float wx1 = p.x - ix0;
            const float wy1 = p.y - iy0;
            const float wx0 = 1.0f - wx1;
            const float wy0 = 1.0f - wy1;

            // Apply weights.
            c00 *= wx0 * wy0;
            c10 *= wx1 * wy0;
            c01 *= wx0 * wy1;
            c11 *= wx1 * wy1;

            // Accumulate.
            c00 += c10;
            c00 += c01;
            c00 += c11;

            return c00;
        }

        // Trapezoid vertices are expressed in [0,width)x[0,height) (note: open on the right).
        Color3f filter_trapezoid(
            const Image&    texture,
            const float     texture_gamma,
            const Vector2f& v00,
            const Vector2f& v10,
            const Vector2f& v01,
            const Vector2f& v11) const
        {
            // Draw the input trapezoid.
            const Color3f TrapezoidColor(1.0f, 0.0f, 1.0f);
            draw_line(m_debug_image, f2i(v00), f2i(v10), TrapezoidColor);
            draw_line(m_debug_image, f2i(v10), f2i(v11), TrapezoidColor);
            draw_line(m_debug_image, f2i(v11), f2i(v01), TrapezoidColor);
            draw_line(m_debug_image, f2i(v01), f2i(v00), TrapezoidColor);

            // Compute the parameters of the inscribed ellipse.
            Vector2f center, du, dv;
            trapezoid_to_ellipse(v00, v10, v01, v11, center, du, dv);

            // Draw the axes of the ellipse.
            draw_line(m_debug_image, f2i(center), f2i(center + du), Color3f(1.0f, 0.0f, 0.0f));
            draw_line(m_debug_image, f2i(center), f2i(center + dv), Color3f(0.0f, 1.0f, 0.0f));

            // Compute the inclusion threshold.
            const float F = static_cast<float>(WeightCount);

            const float K_den = square(du.x * dv.y - dv.x * du.y);
            if (K_den == 0.0f)
                return filter_bilinear(texture, texture_gamma, center);

            // Compute the ellipse coefficients.
            const float K = F / K_den;
            const float A = K * (du.y * du.y + dv.y * dv.y);
            const float B = K * (-2.0f * (du.x * du.y + dv.x * dv.y));
            const float C = K * (du.x * du.x + dv.x * dv.x);

            // Make sure we have an elliptical paraboloid, concave upward.
            assert(A > 0.0f);
            assert(A * C - B * B / 4.0f > 0.0f);

            // Compute the bounding box of the ellipse.
            const CanvasProperties& props = texture.properties();
            const float ku = 2.0f * C * sqrt(F / (4.0f * A * C * C - C * B * B));
            const float kv = 2.0f * A * sqrt(F / (4.0f * A * A * C - A * B * B));
            AABB2i bbox;
            bbox.min.x = max(truncate<int>(center.x - ku), 0);
            bbox.min.y = max(truncate<int>(center.y - kv), 0);
            bbox.max.x = min(truncate<int>(ceil(center.x + ku)), static_cast<int>(props.m_canvas_width - 1));
            bbox.max.y = min(truncate<int>(ceil(center.y + kv)), static_cast<int>(props.m_canvas_height - 1));

            // Draw the bounding box of the ellipse.
            draw_box(m_debug_image, bbox, Color3f(1.0f, 1.0f, 0.0f));

            Color3f num(0.0f);
            float den = 0.0f;

            const float u = (bbox.min.x + 0.5f) - center.x;
            const float Ddq = 2.0f * A;

            for (int y = bbox.min.y; y <= bbox.max.y; ++y)
            {
                const float v = (y + 0.5f) - center.y;
                float dq = A * (2.0f * u + 1.0f) + B * v;
                float q = (C * v + B * u) * v + A * u * u;

                for (int x = bbox.min.x; x <= bbox.max.x; ++x)
                {
                    if (q < F)
                    {
                        const float w = m_weights[truncate<int>(q)];

                        Color3f texel;
                        texture.get_pixel(x, y, texel);

                        if (texture_gamma != 1.0f)
                            texel = pow(texel, texture_gamma);

                        num += texel * w;
                        den += w;

                        tint_pixel(
                            m_debug_image,
                            x, y,
                            Color3f(0.0f, 1.0f, 0.0f),
                            w);
                    }
                    else
                    {
                        tint_pixel(
                            m_debug_image,
                            x, y,
                            Color3f(0.0f, 0.0f, 1.0f),
                            0.2f);
                    }

                    q += dq;
                    dq += Ddq;
                }
            }

            return den > 0.0f
                ? num / den
                : filter_bilinear(texture, texture_gamma, center);
        }

      private:
        Image& m_debug_image;

        enum { WeightCount = 256 };
        float m_weights[WeightCount];

        void compute_weights()
        {
            for (int i = 0; i < WeightCount; ++i)
            {
                const float Alpha = 2.0f;
                const float r2 = static_cast<float>(i) / (WeightCount - 1);
                m_weights[i] = exp(-Alpha * r2);
            }
        }

        static void trapezoid_to_ellipse(
            const Vector2f& v00,
            const Vector2f& v10,
            const Vector2f& v01,
            const Vector2f& v11,
            Vector2f&       center,
            Vector2f&       du,
            Vector2f&       dv)
        {
            const Vector2f middle_00_10 = 0.5f * (v00 + v10);
            const Vector2f middle_01_11 = 0.5f * (v01 + v11);
            const Vector2f middle_10_11 = 0.5f * (v10 + v11);

            center = 0.5f * (middle_00_10 + middle_01_11);
            du = middle_10_11 - center;
            dv = middle_01_11 - center;
        }

        static Color3f pow(const Color3f& c, const float e)
        {
            return Color3f(std::pow(c[0], e), std::pow(c[1], e), std::pow(c[2], e));
        }

        static Vector2i f2i(const Vector2f& v)
        {
            return Vector2i(truncate<int>(v[0]), truncate<int>(v[1]));
        }
    };

    //---------------------------------------------------------------------------------------------
    //--- Test Code -------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------------

    TEST_CASE(EWAFiltering)
    {
        // Generate a checkerboard texture.
        Image texture(512, 512, 512, 512, 3, PixelFormatFloat);
        draw_checkerboard(texture, 32, Color3f(0.3f), Color3f(1.0f));

        // Corners of the input trapezoid.
        const Vector2f V00(130.0f, 400.0f);
        const Vector2f V10(220.0f, 440.0f);
        const Vector2f V01(200.0f,  60.0f);
        const Vector2f V11(480.0f, 230.0f);

        // Run the reference filter.
        Image debug_image(texture);
        EWAFilterRef ref_filter(debug_image);
        const Color3f ref_result =
            ref_filter.filter_trapezoid(texture, 1.0f, V00, V10, V01, V11);

        // Write the debug image to disk.
        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa.png", debug_image);

        // Run the AK filter.
        EWAFilterAK ak_filter;
        Color3f ak_result;
        ak_filter.filter_trapezoid(
            reinterpret_cast<const float*>(texture.tile(0, 0).get_storage()),
            texture.properties().m_canvas_width,
            texture.properties().m_canvas_height,
            texture.properties().m_channel_count,
            1.0f,
            V00.x, V00.y,
            V10.x, V10.y,
            V01.x, V01.y,
            V11.x, V11.y,
            &ak_result[0]);

        // Verify that the results match.
        EXPECT_FEQ(ref_result, ak_result);
    }

    TEST_CASE(BilinearFilteringFallback)
    {
        // Generate a 2x2 checkerboard texture.
        Image texture(2, 2, 2, 2, 3, PixelFormatFloat);
        draw_checkerboard(texture, 1, Color3f(0.0f), Color3f(1.0f));

        // Corners of the input trapezoid.
        const Vector2f V00(0.99f, 0.99f);
        const Vector2f V10(1.01f, 0.99f);
        const Vector2f V01(0.99f, 1.01f);
        const Vector2f V11(1.01f, 1.01f);

        // Run the reference filter.
        Image debug_image(texture);
        EWAFilterRef ref_filter(debug_image);
        const Color3f ref_result =
            ref_filter.filter_trapezoid(texture, 1.0f, V00, V10, V01, V11);
        EXPECT_FEQ(Color3f(0.5f), ref_result);

        // Run the AK filter.
        EWAFilterAK ak_filter;
        Color3f ak_result;
        ak_filter.filter_trapezoid(
            reinterpret_cast<const float*>(texture.tile(0, 0).get_storage()),
            texture.properties().m_canvas_width,
            texture.properties().m_canvas_height,
            texture.properties().m_channel_count,
            1.0f,
            V00.x, V00.y,
            V10.x, V10.y,
            V01.x, V01.y,
            V11.x, V11.y,
            &ak_result[0]);
        EXPECT_FEQ(Color3f(0.5f), ak_result);
    }

    Vector2f random_point(MersenneTwister& rng, const AABB2f& bbox)
    {
        Vector2f v;
        v.x = rand_float2(rng, bbox.min.x, bbox.max.x);
        v.y = rand_float2(rng, bbox.min.y, bbox.max.y);
        return v;
    }

    TEST_CASE(StressTest)
    {
        // Generate a checkerboard texture.
        const size_t Width = 32;
        const size_t Height = 32;
        Image texture(Width, Height, Width, Height, 3, PixelFormatFloat);
        draw_checkerboard(texture, 1, Color3f(0.3f), Color3f(1.0f));

        // Create the filters.
        Image debug_image(texture);
        EWAFilterRef ref_filter(debug_image);
        EWAFilterAK ak_filter;

        const AABB2f domain(
            Vector2f(0.0f, 0.0f),
            Vector2f(static_cast<float>(Width), static_cast<float>(Height)));
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            // Corners of the input trapezoid.
            const Vector2f v00 = random_point(rng, domain);
            const Vector2f v10 = random_point(rng, domain);
            const Vector2f v01 = random_point(rng, domain);
            const Vector2f v11 = random_point(rng, domain);

            // Run the reference filter.
            const Color3f ref_result =
                ref_filter.filter_trapezoid(texture, 1.0f, v00, v10, v01, v11);

            // Run the AK filter.
            Color3f ak_result;
            ak_filter.filter_trapezoid(
                reinterpret_cast<const float*>(texture.tile(0, 0).get_storage()),
                texture.properties().m_canvas_width,
                texture.properties().m_canvas_height,
                texture.properties().m_channel_count,
                1.0f,
                v00.x, v00.y,
                v10.x, v10.y,
                v01.x, v01.y,
                v11.x, v11.y,
                &ak_result[0]);

            // Verify that the results match.
            EXPECT_FEQ(ref_result, ak_result);
        }
    }
}
