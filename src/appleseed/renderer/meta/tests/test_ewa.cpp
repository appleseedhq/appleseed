
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

// appleseed.renderer headers.
#include "renderer/kernel/texturing/ewa.h"
#include "renderer/kernel/texturing/ewa_texturesampler.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
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
using namespace renderer;
using namespace std;

TEST_SUITE(EWAFilteringExploration)
{
    //---------------------------------------------------------------------------------------------
    // Drawing primitives used for debugging.
    //---------------------------------------------------------------------------------------------

    bool is_inside(
        const Image&            image, 
        const int               x,
        const int               y)
    {
        const CanvasProperties& props = image.properties();

        return
            x >= 0 &&
            y >= 0 &&
            x < static_cast<int>(props.m_canvas_width) &&
            y < static_cast<int>(props.m_canvas_height);
    }

    void draw_line(
        Image&                  image,
        const Vector2i&         v0,
        const Vector2i&         v1,
        const Color3f&          color)
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
        Image&                  image,
        const Vector2i&         v0,
        const Vector2i&         v1,
        const Color3f&          color)
    {
        draw_line(image, Vector2i(v0.x, v0.y), Vector2i(v1.x, v0.y), color);
        draw_line(image, Vector2i(v1.x, v0.y), Vector2i(v1.x, v1.y), color);
        draw_line(image, Vector2i(v1.x, v1.y), Vector2i(v0.x, v1.y), color);
        draw_line(image, Vector2i(v0.x, v1.y), Vector2i(v0.x, v0.y), color);
    }

    void draw_trapezoid(
        Image&                  image,
        const Vector2i&         v00,
        const Vector2i&         v10,
        const Vector2i&         v01,
        const Vector2i&         v11,
        const Color3f&          color)
    {
        draw_line(image, v00, v10, color);
        draw_line(image, v10, v11, color);
        draw_line(image, v11, v01, color);
        draw_line(image, v01, v00, color);
    }

    void tint_pixel(
        Image&                  image,
        const Vector2i&         p,
        const Color3f&          color,
        const float             intensity)
    {
        if (is_inside(image, p.x, p.y))
        {
            Color3f base;
            image.get_pixel(p.x, p.y, base);
            image.set_pixel(p.x, p.y, lerp(base, color, intensity));
        }
    }

    void tint_solid_rectangle(
        Image&                  image,
        const Vector2i&         v0,
        const Vector2i&         v1,
        const Color3f&          color,
        const float             intensity)
    {
        for (int y = v0.y; y <= v1.y; ++y)
        {
            for (int x = v0.x; x <= v1.x; ++x)
                tint_pixel(image, Vector2i(x, y), color, intensity);
        }
    }

    void draw_checkerboard(
        Image&                  image,
        const size_t            scale = 16,
        const Color3f&          color1 = Color3f(0.0f),
        const Color3f&          color2 = Color3f(1.0f))
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
        Image&                  dest,
        const Image&            source)
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

    template <typename T>
    Vector2i convert(
        const size_t            source_width,
        const size_t            source_height,
        const size_t            dest_width,
        const size_t            dest_height,
        const Vector<T, 2>&     v)
    {
        return
            Vector2i(
                truncate<int>(static_cast<float>(v[0]) / source_width * dest_width),
                truncate<int>(static_cast<float>(v[1]) / source_height * dest_height));
    }

    template <typename T>
    Vector2i convert(
        const CanvasProperties& source,
        const CanvasProperties& dest,
        const Vector<T, 2>&     v)
    {
        return
            convert(
                source.m_canvas_width,
                source.m_canvas_height,
                dest.m_canvas_width,
                dest.m_canvas_height,
                v);
    }

    //---------------------------------------------------------------------------------------------
    // Reference EWA filter implementation with debugging aids.
    // http://www.cs.cmu.edu/~ph/texfund/texfund.pdf
    //---------------------------------------------------------------------------------------------

    void trapezoid_to_ellipse(
        const Vector2f&         v00,
        const Vector2f&         v10,
        const Vector2f&         v01,
        const Vector2f&         v11,
        float&                  center_x,
        float&                  center_y,
        float&                  dudx,
        float&                  dudy,
        float&                  dvdx,
        float&                  dvdy)
    {
        const Vector2f middle_00_10 = 0.5f * (v00 + v10);
        const Vector2f middle_01_11 = 0.5f * (v01 + v11);
        const Vector2f middle_10_11 = 0.5f * (v10 + v11);

        center_x = 0.5f * (middle_00_10.x + middle_01_11.x);
        center_y = 0.5f * (middle_00_10.y + middle_01_11.y);

        dudx = middle_10_11.x - center_x;
        dudy = middle_01_11.x - center_x;
        dvdx = middle_10_11.y - center_y;
        dvdy = middle_01_11.y - center_y;
    }

    template <int NumChannels, typename Texture>
    class EWAFilterRef
    {
      public:
        explicit EWAFilterRef(Image& debug_image)
          : m_debug_image(debug_image)
        {
            compute_weights();
        }

#define CONV(v)                                     \
    convert(                                        \
        texture.width(),                            \
        texture.height(),                           \
        m_debug_image.properties().m_canvas_width,  \
        m_debug_image.properties().m_canvas_height, \
        v)

        // Coordinates are expressed in [0,width)x[0,height) (note: open on the right).
        void filter_ellipse(
            const Texture&  texture,
            const float     center_x,
            const float     center_y,
            const float     dudx,
            const float     dudy,
            const float     dvdx,
            const float     dvdy,
            float           result[])
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
            const float ku = 2.0f * C * sqrt(F / (4.0f * A * C * C - C * B * B));
            const float kv = 2.0f * A * sqrt(F / (4.0f * A * A * C - A * B * B));
            const int min_x = static_cast<int>(center_x - ku);
            const int min_y = static_cast<int>(center_y - kv);
            const int max_x = static_cast<int>(ceil(center_x + ku));
            const int max_y = static_cast<int>(ceil(center_y + kv));

            memset(result, 0, NumChannels * sizeof(float));
            float den = 0.0f;

            const float u = (min_x + 0.5f) - center_x;
            const float Ddq = 2.0f * A;

            for (int y = min_y; y < max_y; ++y)
            {
                const float v = (y + 0.5f) - center_y;
                float dq = A * (2.0f * u + 1.0f) + B * v;
                float q = (C * v + B * u) * v + A * u * u;

                for (int x = min_x; x < max_x; ++x)
                {
                    if (q < F)
                    {
                        float texel[NumChannels];
                        texture.get(x, y, texel);

                        const float w = m_weights[q <= 0.0f ? 0 : truncate<size_t>(q)];

                        for (int c = 0; c < NumChannels; ++c)
                            result[c] += w * texel[c];

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
            draw_line(
                m_debug_image,
                CONV(Vector2f(center_x, center_y)),
                CONV(Vector2f(center_x + dudx, center_y + dvdx)),
                Color3f(1.0f, 0.0f, 0.0f));
            draw_line(
                m_debug_image,
                CONV(Vector2f(center_x, center_y)),
                CONV(Vector2f(center_x + dudy, center_y + dvdy)),
                Color3f(0.0f, 1.0f, 0.0f));

            // Draw the bounding box of the ellipse.
            draw_rectangle(
                m_debug_image,
                CONV(Vector2i(min_x, min_y)),
                CONV(Vector2i(max_x - 1, max_y - 1)),
                Color3f(1.0f, 1.0f, 0.0f));

            assert(den > 0.0f);

            const float rcp_den = 1.0f / den;

            for (int c = 0; c < NumChannels; ++c)
                result[c] *= rcp_den;
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
    };

    //---------------------------------------------------------------------------------------------
    // Tests.
    //---------------------------------------------------------------------------------------------

    TEST_CASE(FilterEllipse)
    {
        // Generate a checkerboard texture.
        Image texture(512, 512, 512, 512, 3, PixelFormatFloat);
        draw_checkerboard(texture, 32, Color3f(0.3f), Color3f(1.0f));
        TextureSampler texture_sampler(texture);

        // Corners of the input trapezoid.
        const Vector2f V00(130.0f, 400.0f);
        const Vector2f V10(220.0f, 440.0f);
        const Vector2f V01(200.0f,  60.0f);
        const Vector2f V11(480.0f, 230.0f);

        // Convert the trapezoid into an ellipse.
        float center_x, center_y, dudx, dudy, dvdx, dvdy;
        trapezoid_to_ellipse(V00, V10, V01, V11, center_x, center_y, dudx, dudy, dvdx, dvdy);

        // Create the debug image.
        Image debug_image(512, 512, 512, 512, 3, PixelFormatFloat);
        draw_image(debug_image, texture);

        // Run the reference filter.
        EWAFilterRef<3, TextureSampler> ref_filter(debug_image);
        Color3f ref_result;
        ref_filter.filter_ellipse(
            texture_sampler,
            center_x,
            center_y,
            dudx,
            dudy,
            dvdx,
            dvdy,
            &ref_result[0]);

        // Draw the input trapezoid.
        draw_trapezoid(
            debug_image,
            convert(texture.properties(), debug_image.properties(), V00),
            convert(texture.properties(), debug_image.properties(), V10),
            convert(texture.properties(), debug_image.properties(), V01),
            convert(texture.properties(), debug_image.properties(), V11),
            Color3f(1.0f, 0.0f, 1.0f));

        // Run the AK filter.
        EWAFilterAK<3, TextureSampler> ak_filter;
        Color3f ak_result;
        ak_filter.filter_ellipse(
            texture_sampler,
            center_x,
            center_y,
            dudx,
            dudy,
            dvdx,
            dvdy,
            &ak_result[0]);

        // Verify that the results match.
        EXPECT_FEQ(ref_result, ak_result);

        // Write the debug image to disk.
        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa_filterellipse.png", debug_image);
    }

    void small_ellipse_test(
        const char*         filepath,
        const Vector2f&     v00,
        const Vector2f&     v10,
        const Vector2f&     v01,
        const Vector2f&     v11)
    {
        // Generate a 8x8 checkerboard texture.
        Image texture(8, 8, 8, 8, 3, PixelFormatFloat);
        draw_checkerboard(texture, 1, Color3f(0.3f), Color3f(1.0f));
        TextureSampler texture_sampler(texture);

        // Convert the trapezoid into an ellipse.
        float center_x, center_y, dudx, dudy, dvdx, dvdy;
        trapezoid_to_ellipse(v00, v10, v01, v11, center_x, center_y, dudx, dudy, dvdx, dvdy);

        // Create the debug image.
        Image debug_image(512, 512, 512, 512, 3, PixelFormatFloat);
        draw_image(debug_image, texture);

        // Run the reference filter.
        EWAFilterRef<3, TextureSampler> filter(debug_image);
        Color3f result;
        filter.filter_ellipse(
            texture_sampler,
            center_x,
            center_y,
            dudx,
            dudy,
            dvdx,
            dvdy,
            &result[0]);

        // Draw the input trapezoid.
        draw_trapezoid(
            debug_image,
            convert(texture.properties(), debug_image.properties(), v00),
            convert(texture.properties(), debug_image.properties(), v10),
            convert(texture.properties(), debug_image.properties(), v01),
            convert(texture.properties(), debug_image.properties(), v11),
            Color3f(1.0f, 0.0f, 1.0f));

        // Write the debug image to disk.
        GenericImageFileWriter writer;
        writer.write(filepath, debug_image);
    }

    TEST_CASE(FilterEllipse_SmallEllipse)
    {
        small_ellipse_test(
            "unit tests/outputs/test_ewa_filterellipse_smallellipse.png",
            Vector2f(3.1f, 3.1f),
            Vector2f(4.9f, 3.1f),
            Vector2f(3.1f, 4.9f),
            Vector2f(4.9f, 4.9f));
    }

    TEST_CASE(FilterEllipse_SubTexelEllipse)
    {
        small_ellipse_test(
            "unit tests/outputs/test_ewa_filterellipse_subtexelellipse.png",
            Vector2f(3.1f, 3.1f),
            Vector2f(3.4f, 3.1f),
            Vector2f(3.1f, 3.4f),
            Vector2f(3.4f, 3.4f));
    }

    TEST_CASE(FilterEllipse_Magnification)
    {
        // Load the input texture.
        GenericImageFileReader reader;
        auto_ptr<Image> texture(reader.read("unit tests/inputs/test_ewa_texture_3x3.exr"));
        assert(texture->properties().m_channel_count == 3);

        // Output image.
        Image output_image(512, 512, 512, 512, 3, PixelFormatFloat);

        const size_t texture_width = texture->properties().m_canvas_width;
        const size_t texture_height = texture->properties().m_canvas_height;
        const size_t output_width = output_image.properties().m_canvas_width;
        const size_t output_height = output_image.properties().m_canvas_height;
        const float scale_x = static_cast<float>(texture_width) / output_width;
        const float scale_y = static_cast<float>(texture_height) / output_height;

        // Create the filter.
        TextureSampler texture_sampler(*texture.get());
        EWAFilterAK<3, TextureSampler> filter;

        for (size_t y = 0; y < output_height; ++y)
        {
            for (size_t x = 0; x < output_width; ++x)
            {
                // Compute the parameters of the ellipse.
                const float center_x = (x + 0.5f) / output_width * texture_width;
                const float center_y = (y + 0.5f) / output_height * texture_height;
                const float dudx = 1.0f / output_width;
                const float dudy = 0.0f;
                const float dvdx = 0.0f;
                const float dvdy = 1.0f / output_height;

                // Run the filter.
                Color3f result;
                filter.filter_ellipse(
                    texture_sampler,
                    center_x,
                    center_y,
                    dudx,
                    dudy,
                    dvdx,
                    dvdy,
                    &result[0]);

                // Store the result.
                output_image.set_pixel(x, y, result);
            }
        }

        // Write the output image to disk.
        GenericImageFileWriter writer;
        writer.write("unit tests/outputs/test_ewa_filterellipse_magnification.png", output_image);
    }

    TEST_CASE(StressTest)
    {
        // Generate a checkerboard texture.
        const size_t Width = 64;
        const size_t Height = 64;
        Image texture(Width, Height, Width, Height, 3, PixelFormatFloat);
        draw_checkerboard(texture, 8, Color3f(0.3f, 0.6f, 0.1f), Color3f(1.0f, 0.8f, 0.5f));
        TextureSampler texture_sampler(texture);

        // Create the filters.
        Image debug_image(texture);
        EWAFilterRef<3, TextureSampler> ref_filter(debug_image);
        EWAFilterAK<3, TextureSampler> ak_filter;

        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            // Generate a random ellipse.
            const float center_x = rand_float2(rng, 0.0f, static_cast<float>(Width));
            const float center_y = rand_float2(rng, 0.0f, static_cast<float>(Height));
            const float dudx = rand_float1(rng, -10.0f, +10.0f);
            const float dudy = rand_float1(rng, -10.0f, +10.0f);
            const float dvdx = rand_float1(rng, -10.0f, +10.0f);
            const float dvdy = rand_float1(rng, -10.0f, +10.0f);

            // Run the reference filter.
            Color3f ref_result;
            ref_filter.filter_ellipse(
                texture_sampler,
                center_x,
                center_y,
                dudx,
                dudy,
                dvdx,
                dvdy,
                &ref_result[0]);

            // Run the AK filter.
            Color3f ak_result;
            ak_filter.filter_ellipse(
                texture_sampler,
                center_x,
                center_y,
                dudx,
                dudy,
                dvdx,
                dvdy,
                &ak_result[0]);

            // Verify that the results match.
            EXPECT_FEQ(ref_result, ak_result);
        }
    }
}
