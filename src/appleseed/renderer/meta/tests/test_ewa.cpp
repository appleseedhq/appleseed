
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

// EWA filter implementation for AtomKraft.
#include "renderer/kernel/texturing/ewa.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilereader.h"
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
    // Drawing primitives used for debugging.
    //---------------------------------------------------------------------------------------------

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

    void draw_rectangle(
        Image&          image,
        const Vector2i& v0,
        const Vector2i& v1,
        const Color3f&  color)
    {
        draw_line(image, Vector2i(v0.x, v0.y), Vector2i(v1.x, v0.y), color);
        draw_line(image, Vector2i(v1.x, v0.y), Vector2i(v1.x, v1.y), color);
        draw_line(image, Vector2i(v1.x, v1.y), Vector2i(v0.x, v1.y), color);
        draw_line(image, Vector2i(v0.x, v1.y), Vector2i(v0.x, v0.y), color);
    }

    void tint_pixel(
        Image&          image,
        const Vector2i& p,
        const Color3f&  color,
        const float     intensity)
    {
        if (is_inside(image, p.x, p.y))
        {
            Color3f base;
            image.get_pixel(p.x, p.y, base);
            image.set_pixel(p.x, p.y, lerp(base, color, intensity));
        }
    }

    void tint_solid_rectangle(
        Image&          image,
        const Vector2i& v0,
        const Vector2i& v1,
        const Color3f&  color,
        const float     intensity)
    {
        for (int y = v0.y; y <= v1.y; ++y)
        {
            for (int x = v0.x; x <= v1.x; ++x)
                tint_pixel(image, Vector2i(x, y), color, intensity);
        }
    }

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

    void draw_image(
        Image&          dest,
        const Image&    source)
    {
        const size_t dest_width = dest.properties().m_canvas_width;
        const size_t dest_height = dest.properties().m_canvas_height;
        const size_t source_width = source.properties().m_canvas_width;
        const size_t source_height = source.properties().m_canvas_height;

        for (size_t y = 0; y < dest_height; ++y)
        {
            for (size_t x = 0; x < dest_width; ++x)
            {
                Color3f color;
                source.get_pixel(
                    truncate<size_t>(static_cast<float>(x) / dest_width * source_width),
                    truncate<size_t>(static_cast<float>(y) / dest_height * source_height),
                    color);
                dest.set_pixel(x, y, color);
            }
        }
    }

    //---------------------------------------------------------------------------------------------
    // Reference EWA filter implementation with debugging aids.
    // http://www.cs.cmu.edu/~ph/texfund/texfund.pdf
    //---------------------------------------------------------------------------------------------

    class EWAFilterRef
    {
      public:
        explicit EWAFilterRef(Image& debug_image)
          : m_debug_image(debug_image)
        {
            compute_weights();
        }

#define CONV(v) convert(texture.properties(), m_debug_image.properties(), v)

        // Coordinates are expressed in [0,width)x[0,height) (note: open on the right).
        Color3f filter_ellipse(
            const Image&        texture,
            const float         texture_gamma,
            const Vector2f&     center,
            const float         dudx,
            const float         dudy,
            const float         dvdx,
            const float         dvdy) const
        {
            // Compute the inclusion threshold.
            const float F = static_cast<float>(WeightCount);

            // Compute the ellipse coefficients.
            float A = dvdx * dvdx + dvdy * dvdy + 1.0f;
            float B = -2.0f * (dudx * dvdx + dudy * dvdy);
            float C = dudx * dudx + dudy * dudy + 1.0f;
            const float K = F / (A * C - B * B * 0.25f);
            A *= K;
            B *= K;
            C *= K;

            // Compute the bounding box of the ellipse.
            const CanvasProperties& props = texture.properties();
            const float ku = 2.0f * C * sqrt(F / (4.0f * A * C * C - C * B * B));
            const float kv = 2.0f * A * sqrt(F / (4.0f * A * A * C - A * B * B));
            AABB2i bbox;
            bbox.min.x = max(truncate<int>(center.x - ku), 0);
            bbox.min.y = max(truncate<int>(center.y - kv), 0);
            bbox.max.x = min(truncate<int>(ceil(center.x + ku)), static_cast<int>(props.m_canvas_width));
            bbox.max.y = min(truncate<int>(ceil(center.y + kv)), static_cast<int>(props.m_canvas_height));

            Color3f num(0.0f);
            float den = 0.0f;

            const float u = (bbox.min.x + 0.5f) - center.x;
            const float Ddq = 2.0f * A;

            for (int y = bbox.min.y; y < bbox.max.y; ++y)
            {
                const float v = (y + 0.5f) - center.y;
                float dq = A * (2.0f * u + 1.0f) + B * v;
                float q = (C * v + B * u) * v + A * u * u;

                for (int x = bbox.min.x; x < bbox.max.x; ++x)
                {
                    if (q < F)
                    {
                        const float w = m_weights[q <= 0.0f ? 0 : truncate<size_t>(q)];

                        Color3f texel;
                        texture.get_pixel(x, y, texel);

                        if (texture_gamma != 1.0f)
                            texel = pow(texel, texture_gamma);

                        num += texel * w;
                        den += w;

                        tint_solid_rectangle(
                            m_debug_image,
                            CONV(Vector2i(x, y)),
                            CONV(Vector2i(x + 1, y + 1)) - Vector2i(1, 1),
                            Color3f(0.0f, 0.0f, 1.0f),
                            w);
                    }
                    else
                    {
                        tint_solid_rectangle(
                            m_debug_image,
                            CONV(Vector2i(x, y)),
                            CONV(Vector2i(x + 1, y + 1)) - Vector2i(1, 1),
                            Color3f(1.0f, 0.0f, 0.0f),
                            0.4f);
                    }

                    q += dq;
                    dq += Ddq;
                }
            }

            // Draw the axes of the ellipse.
            draw_line(m_debug_image, CONV(center), CONV(center + Vector2f(dudx, dvdx)), Color3f(1.0f, 0.0f, 0.0f));
            draw_line(m_debug_image, CONV(center), CONV(center + Vector2f(dudy, dvdy)), Color3f(0.0f, 1.0f, 0.0f));

            // Draw the bounding box of the ellipse.
            draw_rectangle(
                m_debug_image,
                CONV(bbox.min),
                CONV(bbox.max) - Vector2i(1, 1),
                Color3f(1.0f, 1.0f, 0.0f));

            assert(den > 0.0f);

            return num / den;
        }

        // Trapezoid vertices are expressed in [0,width)x[0,height) (note: open on the right).
        Color3f filter_trapezoid(
            const Image&        texture,
            const float         texture_gamma,
            const Vector2f&     v00,
            const Vector2f&     v10,
            const Vector2f&     v01,
            const Vector2f&     v11) const
        {
            // Compute the parameters of the inscribed ellipse.
            Vector2f center, du, dv;
            trapezoid_to_ellipse(v00, v10, v01, v11, center, du, dv);

            // Filter the ellipse.
            const Color3f result =
                filter_ellipse(
                    texture,
                    texture_gamma,
                    center,
                    du.x,
                    dv.x,
                    du.y,
                    dv.y);

            // Draw the input trapezoid.
            const Color3f TrapezoidColor(1.0f, 0.0f, 1.0f);
            draw_line(m_debug_image, CONV(v00), CONV(v10), TrapezoidColor);
            draw_line(m_debug_image, CONV(v10), CONV(v11), TrapezoidColor);
            draw_line(m_debug_image, CONV(v11), CONV(v01), TrapezoidColor);
            draw_line(m_debug_image, CONV(v01), CONV(v00), TrapezoidColor);

            return result;
        }

#undef CONV

      private:
        Image& m_debug_image;

        enum { WeightCount = 256 };
        float m_weights[WeightCount];

        void compute_weights()
        {
            for (int i = 0; i < WeightCount; ++i)
            {
                const float Alpha = 2.0f;
                const float q = static_cast<float>(i) / (WeightCount - 1);
                m_weights[i] = exp(-Alpha * q);
            }
        }

        static void trapezoid_to_ellipse(
            const Vector2f&     v00,
            const Vector2f&     v10,
            const Vector2f&     v01,
            const Vector2f&     v11,
            Vector2f&           center,
            Vector2f&           du,
            Vector2f&           dv)
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

        static Vector2i convert(
            const CanvasProperties& source,
            const CanvasProperties& dest,
            const Vector2f&         v)
        {
            return
                Vector2i(
                    truncate<int>(v[0] / source.m_canvas_width * dest.m_canvas_width),
                    truncate<int>(v[1] / source.m_canvas_height * dest.m_canvas_height));
        }

        static Vector2i convert(
            const CanvasProperties& source,
            const CanvasProperties& dest,
            const Vector2i&         v)
        {
            return convert(source, dest, Vector2f(v));
        }
    };

    //---------------------------------------------------------------------------------------------
    // Tests.
    //---------------------------------------------------------------------------------------------

    TEST_CASE(FilterTrapezoid)
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
        Image debug_image(512, 512, 512, 512, 3, PixelFormatFloat);
        draw_image(debug_image, texture);
        EWAFilterRef ref_filter(debug_image);
        const Color3f ref_result =
            ref_filter.filter_trapezoid(texture, 1.0f, V00, V10, V01, V11);

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

        // Write the debug image to disk.
        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa_filtertrapezoid.png", debug_image);
    }

    void small_trapezoid_test(
        const char*         filepath,
        const Vector2f&     v00,
        const Vector2f&     v10,
        const Vector2f&     v01,
        const Vector2f&     v11)
    {
        // Generate a 8x8 checkerboard texture.
        Image texture(8, 8, 8, 8, 3, PixelFormatFloat);
        draw_checkerboard(texture, 1, Color3f(0.3f), Color3f(1.0f));

        // Run the reference filter.
        Image debug_image(512, 512, 512, 512, 3, PixelFormatFloat);
        draw_image(debug_image, texture);
        EWAFilterRef ref_filter(debug_image);
        const Color3f ref_result =
            ref_filter.filter_trapezoid(
                texture,
                1.0f,
                v00, v10, v01, v11);

        // Write the debug image to disk.
        GenericImageFileWriter writer;
        writer.write(filepath, debug_image);
    }

    TEST_CASE(FilterTrapezoid_SmallTrapezoid)
    {
        small_trapezoid_test(
            "unit tests/outputs/test_ewa_filtertrapezoid_smalltrapezoid.png",
            Vector2f(3.1f, 3.1f),
            Vector2f(4.9f, 3.1f),
            Vector2f(3.1f, 4.9f),
            Vector2f(4.9f, 4.9f));
    }

    TEST_CASE(FilterTrapezoid_SubTexelTrapezoid)
    {
        small_trapezoid_test(
            "unit tests/outputs/test_ewa_filtertrapezoid_subtexeltrapezoid.png",
            Vector2f(3.1f, 3.1f),
            Vector2f(3.4f, 3.1f),
            Vector2f(3.1f, 3.4f),
            Vector2f(3.4f, 3.4f));
    }

    TEST_CASE(FilterTrapezoid_Magnification)
    {
        // Load the input texture.
        GenericImageFileReader reader;
        auto_ptr<Image> source_texture(reader.read("unit tests/inputs/test_ewa_texture_3x3.exr"));
        Image texture(
            source_texture->properties().m_canvas_width,
            source_texture->properties().m_canvas_height,
            source_texture->properties().m_canvas_width,
            source_texture->properties().m_canvas_height,
            source_texture->properties().m_channel_count,
            PixelFormatFloat);
        texture.copy(*source_texture.get());
        assert(texture.properties().m_channel_count == 3);

        // Output image.
        Image output_image(512, 512, 512, 512, 3, PixelFormatFloat);

        const size_t texture_width = source_texture->properties().m_canvas_width;
        const size_t texture_height = source_texture->properties().m_canvas_height;
        const size_t output_width = output_image.properties().m_canvas_width;
        const size_t output_height = output_image.properties().m_canvas_height;
        const float scale_x = static_cast<float>(texture_width) / output_width;
        const float scale_y = static_cast<float>(texture_height) / output_height;

        EWAFilterAK ak_filter;

        for (size_t y = 0; y < output_height; ++y)
        {
            for (size_t x = 0; x < output_width; ++x)
            {
                // Compute the corners of the trapezoid.
                const float Size = 1.0f;
                const float x0 = scale_x * x;
                const float y0 = scale_y * y;
                const float x1 = scale_x * (x + Size);
                const float y1 = scale_y * (y + Size);

                // Run the AK filter.
                Color3f ak_result;
                ak_filter.filter_trapezoid(
                    reinterpret_cast<const float*>(texture.tile(0, 0).get_storage()),
                    texture.properties().m_canvas_width,
                    texture.properties().m_canvas_height,
                    texture.properties().m_channel_count,
                    1.0f,
                    x0, y0,
                    x1, y0,
                    x0, y1,
                    x1, y1,
                    &ak_result[0]);

                output_image.set_pixel(x, y, ak_result);
            }
        }

        // Write the output image to disk.
        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa_magnification.png", output_image);
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
        const size_t Width = 64;
        const size_t Height = 64;
        Image texture(Width, Height, Width, Height, 3, PixelFormatFloat);
        draw_checkerboard(texture, 8, Color3f(0.3f, 0.6f, 0.1f), Color3f(1.0f, 0.8f, 0.5f));

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

            // Draw a random gamma value.
            const float texture_gamma = rand_float1(rng, 0.1f, 10.0f);

            // Run the reference filter.
            const Color3f ref_result =
                ref_filter.filter_trapezoid(
                    texture,
                    texture_gamma,
                    v00, v10, v01, v11);

            // Run the AK filter.
            Color3f ak_result;
            ak_filter.filter_trapezoid(
                reinterpret_cast<const float*>(texture.tile(0, 0).get_storage()),
                texture.properties().m_canvas_width,
                texture.properties().m_canvas_height,
                texture.properties().m_channel_count,
                texture_gamma,
                v00.x, v00.y,
                v10.x, v10.y,
                v01.x, v01.y,
                v11.x, v11.y,
                &ak_result[0]);

            // Verify that the results match.
            EXPECT_FEQ_EPS(ref_result, ak_result, 0.05f);
        }
    }
}
