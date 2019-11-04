
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

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <memory>
#include <utility>

using namespace foundation;

TEST_SUITE(ImageTools)
{
    static const Color3f IsoLumBlue(0.0f, 0.0f, 1.0f);                                              // sRGB
    static const Color3f Red(1.0f, 0.0f, 0.0f);                                                     // linear RGB
    static const float IsoLumRedComp = luminance(srgb_to_linear_rgb(IsoLumBlue)) / luminance(Red);  // linear RGB
    static const Color3f IsoLumRed = linear_rgb_to_srgb(Color3f(IsoLumRedComp, 0.0f, 0.0f));        // sRGB

    TEST_CASE(CheckPointsIsoluminance)
    {
        const size_t W = 256;
        const size_t H = 256;

        std::unique_ptr<Image> output(new Image(W, H, 32, 32, 3, PixelFormatFloat));

        MersenneTwister rng;

        for (size_t i = 0; i < W * H; ++i)
        {
            const size_t x = rand_int1(rng, 0, W - 1);
            const size_t y = rand_int1(rng, 0, H - 1);
            output->set_pixel(x, y, saturate((i % 2) ? IsoLumRed : IsoLumBlue));
        }

        GenericImageFileWriter writer("unit tests/outputs/test_compareimages_checkpointsisoluminance.png");
        writer.append_image(output.get());
        writer.write();
    }

    struct ExceptionNonMatchingImageCharacteristics : public Exception {};
    struct ExceptionUnsupportedChannelCount : Exception {};

    template <typename T, size_t N>
    Color<T, N> abs_diff(const Color<T, N>& lhs, const Color<T, N>& rhs)
    {
        Color<T, N> result;

        for (size_t i = 0; i < N; ++i)
            result[i] = std::abs(lhs[i] - rhs[i]);

        return result;
    }

    struct IOnePixelOp
    {
        virtual Color3f operator()(const Color3f& c) const = 0;
    };

    struct ColorMultiply : public IOnePixelOp
    {
        const float m_multiplier;

        explicit ColorMultiply(const float multiplier)
          : m_multiplier(multiplier)
        {
        }

        Color3f operator()(const Color3f& c) const override
        {
            return c * m_multiplier;
        }
    };

    struct ITwoPixelOp
    {
        virtual Color3f operator()(const Color3f& lhs, const Color3f& rhs) const = 0;
    };

    struct ColorDifference : public ITwoPixelOp
    {
        Color3f operator()(const Color3f& lhs, const Color3f& rhs) const override
        {
            return abs_diff(lhs, rhs);
        }
    };

    struct ColorRatio : public ITwoPixelOp
    {
        Color3f operator()(const Color3f& lhs, const Color3f& rhs) const override
        {
            Color3f result;

            for (size_t i = 0; i < result.Components; ++i)
                result[i] = lhs[i] == rhs[i] ? 1.0f : lhs[i] / rhs[i];

            return result;
        }
    };

    struct LuminanceDifference : public ITwoPixelOp
    {
        Color3f operator()(const Color3f& lhs, const Color3f& rhs) const override
        {
            const float result = luminance(lhs) - luminance(rhs);
            return result > 0.0f ? IsoLumRed : IsoLumBlue;
        }
    };

    struct LuminanceRatio : public ITwoPixelOp
    {
        Color3f operator()(const Color3f& lhs, const Color3f& rhs) const override
        {
            const float lhs_lum = luminance(lhs);
            const float rhs_lum = luminance(rhs);
            const float result = lhs_lum == rhs_lum ? 1.0f : lhs_lum / rhs_lum;
            return Color3f(result);
        }
    };

    struct LuminanceDifferenceSign : public ITwoPixelOp
    {
        const float m_threshold;

        explicit LuminanceDifferenceSign(const float threshold = 0.0f)
          : m_threshold(threshold)
        {
        }

        Color3f operator()(const Color3f& lhs, const Color3f& rhs) const override
        {
            const float diff = luminance(lhs) - luminance(rhs);
            return
                diff > m_threshold ? IsoLumRed :
                diff < -m_threshold ? IsoLumBlue :
                Color3f(0.0f);
        }
    };

    struct MaximumComponentDifferenceSign : public ITwoPixelOp
    {
        const float m_threshold;

        explicit MaximumComponentDifferenceSign(const float threshold = 0.0f)
          : m_threshold(threshold)
        {
        }

        Color3f operator()(const Color3f& lhs, const Color3f& rhs) const override
        {
            const Color3f delta = lhs - rhs;
            const float diff = delta[max_abs_index(delta)];
            return
                diff > m_threshold ? IsoLumRed :
                diff < -m_threshold ? IsoLumBlue :
                Color3f(0.0f);
        }
    };

    std::unique_ptr<Image> apply(const Image& image, const IOnePixelOp& op)
    {
        const CanvasProperties& props = image.properties();

        if (props.m_channel_count != 4)
            throw ExceptionUnsupportedChannelCount();

        std::unique_ptr<Image> output(new Image(props));

        for (size_t y = 0; y < props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < props.m_canvas_width; ++x)
            {
                Color4f color;
                image.get_pixel(x, y, color);

                Color4f result;
                result.rgb() = op(color.rgb());
                result.a = color.a;

                output->set_pixel(x, y, saturate(result));
            }
        }

        return output;
    }

    std::unique_ptr<Image> apply(const Image& lhs, const Image& rhs, const ITwoPixelOp& op)
    {
        const CanvasProperties& lhs_props = lhs.properties();
        const CanvasProperties& rhs_props = rhs.properties();

        if (lhs_props.m_canvas_width != rhs_props.m_canvas_width ||
            lhs_props.m_canvas_height != rhs_props.m_canvas_height ||
            lhs_props.m_channel_count != rhs_props.m_channel_count)
            throw ExceptionNonMatchingImageCharacteristics();

        if (lhs_props.m_channel_count != 4)
            throw ExceptionUnsupportedChannelCount();

        std::unique_ptr<Image> output(new Image(lhs));

        for (size_t y = 0; y < lhs_props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < lhs_props.m_canvas_width; ++x)
            {
                Color4f lhs_color;
                lhs.get_pixel(x, y, lhs_color);

                Color4f rhs_color;
                rhs.get_pixel(x, y, rhs_color);

                Color4f result;

                if (lhs_color.a == 0.0f && rhs_color.a == 0.0f)
                    result.set(0.0f);
                else
                {
                    result.rgb() = op(lhs_color.rgb(), rhs_color.rgb());
                    result.a = 1.0f;
                }

                output->set_pixel(x, y, saturate(result));
            }
        }

        return output;
    }

#if 0

    TEST_CASE(CompareImages)
    {
        GenericImageFileReader reader;
        std::unique_ptr<Image> left_image(reader.read("left.exr"));
        std::unique_ptr<Image> right_image(reader.read("right.exr"));

        ASSERT_TRUE(left_image.get());
        ASSERT_TRUE(right_image.get());

        // ColorDifference op;
        // ColorRatio op;
        // LuminanceDifference op;
        // LuminanceRatio op;
        // LuminanceDifferenceSign op(1.0e-9f);
        MaximumComponentDifferenceSign op(1.0e-9f);

        std::unique_ptr<Image> result =
            apply(
                *apply(*left_image.get(), *right_image.get(), op).get(),
                ColorMultiply(1.0f));

        GenericImageFileWriter writer("unit tests/outputs/test_imagetools_compareimages.png");
        writer.append_image(result.get());
        writer.write();
    }

#endif

#if 0

    TEST_CASE(TweakImage)
    {
        const float DitherAmplitude = 1.0f;
        const float GammaCorrection = 1.0f;
        const float DarksBoost = 0.0f;
        const float DarksGammaCorrection = 10.0f;

        GenericImageFileReader reader;
        std::unique_ptr<Image> input_image(reader.read("input.exr"));

        ASSERT_TRUE(input_image.get());

        const CanvasProperties& props = input_image->properties();

        Image output_image(
            props.m_canvas_width,
            props.m_canvas_height,
            32,
            32,
            4,
            PixelFormatUInt8);

        MersenneTwister rng;

        for (size_t y = 0; y < props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < props.m_canvas_width; ++x)
            {
                Color4f input_color;
                input_image->get_pixel(x, y, input_color);

                Color4b output_color;

                for (size_t c = 0; c < 4; ++c)
                {
                    // Get floating-point component value.
                    float val = input_color[c];
                    assert(val >= 0.0f && val <= 1.0f);

                    if (c < 3)  // skip the alpha channel
                    {
                        // Apply gamma correction.
                        val = std::pow(val, 1.0f / GammaCorrection);

                        // Slightly brighten dark areas.
                        val += DarksBoost * (1.0f - std::pow(val, 1.0f / DarksGammaCorrection));

                        // Apply dithering.
                        const float NormalizedDitherAmp = 0.5f * DitherAmplitude / 256.0f;
                        val += rand_float2(rng, -NormalizedDitherAmp, NormalizedDitherAmp);

                        // Clamp back to [0, 1].
                        val = saturate(val);
                    }

                    // Convert to 8-bit integer.
                    val = std::min(val * 256.0f, 255.0f);
                    output_color[c] = truncate<std::uint8_t>(val);
                }

                output_image.set_pixel(x, y, output_color);
            }
        }

        GenericImageFileWriter writer("unit tests/outputs/test_imagetools_tweakimage.png");
        writer.append_image(&output_image);
        writer.write();
    }

#endif

#if 0

    TEST_CASE(ComputeAverageImageValue)
    {
        GenericImageFileReader reader;
        std::unique_ptr<Image> input_image(reader.read("input.exr"));

        ASSERT_TRUE(input_image.get());

        Color3d avg(0.0);

        const CanvasProperties& props = input_image->properties();
        for (size_t y = 0; y < props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < props.m_canvas_width; ++x)
            {
                Color4f input_color;
                input_image->get_pixel(x, y, input_color);
                avg += Color3d(input_color.rgb());
            }
        }

        avg /= static_cast<double>(props.m_pixel_count);

        std::cerr << "Average: " << avg[0] << " " << avg[1] << " " << avg[2] << std::endl;
    }

#endif

#if 0

    TEST_CASE(GenerateSphereBumpMap)
    {
        const size_t ImageSize = 512;

        Image image(
            ImageSize,
            ImageSize,
            32,
            32,
            4,
            PixelFormatFloat);

        for (size_t y = 0; y < ImageSize; ++y)
        {
            for (size_t x = 0; x < ImageSize; ++x)
            {
                const float fx = fit<size_t, float>(x, 0, ImageSize - 1, -1.0f, +1.0f);
                const float fy = fit<size_t, float>(y, 0, ImageSize - 1, -1.0f, +1.0f);
                const float dist = std::sqrt(fx * fx + fy * fy);
                const float height =
                    dist < 0.5f
                        ? 0.5f + std::sqrt(1.0f - square(dist / 0.5f)) / 2.0f
                        : 0.5f;
                const Color4f color(height, height, height, 1.0f);
                image.set_pixel(x, y, color);
            }
        }

        GenericImageFileWriter writer("unit tests/outputs/test_imagetools_spherebump.exr");
        writer.append_image(&image);
        writer.write();
    }

#endif

#if 0

    TEST_CASE(GenerateSineBumpMap)
    {
        const size_t ImageSize = 512;

        Image image(
            ImageSize,
            ImageSize,
            32,
            32,
            4,
            PixelFormatFloat);

        for (size_t y = 0; y < ImageSize; ++y)
        {
            for (size_t x = 0; x < ImageSize; ++x)
            {
                const float fx = fit<size_t, float>(x, 0, ImageSize - 1, -1.0f, +1.0f);
                const float height = 0.5f + 0.5f * std::cos(fx * TwoPi<float>());
                const Color4f color(height, height, height, 1.0f);
                image.set_pixel(x, y, color);
            }
        }

        GenericImageFileWriter writer("unit tests/outputs/test_imagetools_sinebump.exr");
        writer.append_image(&image);
        writer.write();
    }

#endif

#if 0

    TEST_CASE(GenerateAnisotropyMap)
    {
        static constexpr size_t ImageSize = 2048;
        static constexpr size_t SubPixelGrid = 6;
        static constexpr enum class Vector { Normal, Tangent } Vector = Vector::Tangent;
        static constexpr enum class FieldMode { RedGreen, RedBlue } FieldMode = FieldMode::RedGreen;
        static constexpr enum class BaseLevel { Zero, Middle } BaseLevel = BaseLevel::Middle;
        static constexpr bool ClampToDisk = false;

        Image image(ImageSize, ImageSize, 32, 32, 3, PixelFormatHalf);
        const float half_image_size = static_cast<float>(ImageSize) / 2.0f;

        for (size_t y = 0; y < ImageSize; ++y)
        {
            for (size_t x = 0; x < ImageSize; ++x)
            {
                Color3f pixel_color(0.0f);

                for (size_t sy = 0; sy < SubPixelGrid; ++sy)
                {
                    for (size_t sx = 0; sx < SubPixelGrid; ++sx)
                    {
                        Color3f subpixel_color = BaseLevel == BaseLevel::Zero ? Color3f(0.0f) : Color3f(0.5f);

                        const float fx = x + (0.5f + sx) / SubPixelGrid;
                        const float fy = y + (0.5f + sy) / SubPixelGrid;

                        const float dx = fx - half_image_size;
                        const float dy = half_image_size - fy;

                        const float d = std::sqrt(dx * dx + dy * dy);
                        assert(d > 0.0f);

                        if (!ClampToDisk || d <= half_image_size)
                        {
                            const Vector2f n(dx / d, dy / d);
                            const Vector2f t(n.y, -n.x);

                            const Vector2f v = Vector == Vector::Normal ? n : t;

                            switch (FieldMode)
                            {
                              case FieldMode::RedGreen:
                                subpixel_color.r = saturate((v.x + 1.0f) * 0.5f);
                                subpixel_color.g = saturate((v.y + 1.0f) * 0.5f);
                                break;

                              case FieldMode::RedBlue:
                                subpixel_color.r = saturate((v.x + 1.0f) * 0.5f);
                                subpixel_color.b = saturate((v.y + 1.0f) * 0.5f);
                                break;
                            }
                        }

                        pixel_color += subpixel_color;
                    }
                }

                pixel_color /= static_cast<float>(SubPixelGrid * SubPixelGrid);

                image.set_pixel(x, y, pixel_color);
            }
        }

        GenericImageFileWriter writer("unit tests/outputs/test_imagetools_anisotropymap.png");
        writer.append_image(&image);
        writer.write();
    }

#endif
}
