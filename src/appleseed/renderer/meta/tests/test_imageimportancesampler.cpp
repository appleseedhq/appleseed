
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
#include "renderer/global/global.h"
#include "renderer/kernel/lighting/imageimportancesampler.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/image/drawing.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/math/qmc.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdio>

TEST_SUITE(Renderer_Kernel_Lighting_ImageImportanceSampler)
{
    using namespace foundation;
    using namespace renderer;
    using namespace std;

    class HorizontalGradientSampler
    {
      public:
        HorizontalGradientSampler(const size_t width, const size_t height)
          : m_width(width)
          , m_height(height)
        {
        }

        double operator()(const size_t x, const size_t y) const
        {
            return static_cast<double>(x) / (m_width - 1);
        }

      private:
        const size_t m_width;
        const size_t m_height;
    };

    TEST_CASE(GetPDF_GivenImageAndSample_ReturnsSameProbabilityAsSample)
    {
        const size_t Width = 5;
        const size_t Height = 5;

        HorizontalGradientSampler sampler(Width, Height);
        ImageImportanceSampler<double, HorizontalGradientSampler>
            importance_sampler(
                Width,
                Height,
                sampler);

        size_t x, y;
        double prob_xy;
        importance_sampler.sample(Vector2d(0.3, 0.7), x, y, prob_xy);

        const double pdf = importance_sampler.get_pdf(x, y);

        EXPECT_FEQ(prob_xy, pdf);
    }

    auto_ptr<Image> load_raw_image(
        const string&   filename,
        const size_t    width,
        const size_t    height)
    {
        auto_ptr<Image> image(
            new Image(
                width,
                height,
                width,
                height,
                3,
                PixelFormatUInt8));

        FILE* file = fopen(filename.c_str(), "rb");

        if (file == 0)
            return auto_ptr<Image>(0);

        fread(image->pixel(0, 0), 3, width * height, file);
        fclose(file);

        return image;
    }

    class ImageSampler
    {
      public:
        explicit ImageSampler(const Image& image)
          : m_image(image)
        {
        }

        float operator()(const size_t x, const size_t y) const
        {
            Color3f color;
            m_image.get_pixel(x, y, color);
            return luminance(color);
        }

      private:
        const Image&    m_image;
    };

    void generate_image(
        const string&   input_filename,
        const size_t    width,
        const size_t    height,
        const string&   output_image,
        const size_t    sample_count)
    {
        auto_ptr<Image> image = load_raw_image(input_filename, width, height);

        ImageSampler sampler(*image.get());

        ImageImportanceSampler<float, ImageSampler> importance_sampler(
            width,
            height,
            sampler);

        for (size_t i = 0; i < sample_count; ++i)
        {
            const size_t Bases[1] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, i, sample_count);

            size_t x, y;
            float prob_xy;

            importance_sampler.sample(s, x, y, prob_xy);

            const float pdf = importance_sampler.get_pdf(x, y);
            assert(pdf == prob_xy);

            // Compute the coordinates of the center of the pixel.
            const Vector2d point(
                (2.0 * x + 1.0) / (2.0 * width),
                (2.0 * y + 1.0) / (2.0 * height));

            Drawing::draw_dot(
                *image.get(),
                point,
                Color3f(1.0f, 0.0f, 1.0f));
        }

        GenericImageFileWriter writer;
        writer.write(output_image, *image.get());
    }

    TEST_CASE(ImportanceSampleWhiteImage)
    {
        generate_image(
            "data/white.raw",
            512,
            512,
            "output/test_imageimportancesampler_white.png",
            256);
    }

    TEST_CASE(ImportanceSampleBlackImage)
    {
        generate_image(
            "data/black.raw",
            512,
            512,
            "output/test_imageimportancesampler_black.png",
            256);
    }

    TEST_CASE(ImportanceSampleCheckerboardImage)
    {
        generate_image(
            "data/checkerboard.raw",
            512,
            512,
            "output/test_imageimportancesampler_checkerboard.png",
            256);
    }

    TEST_CASE(ImportanceSampleGradientImage)
    {
        generate_image(
            "data/gradient.raw",
            512,
            512,
            "output/test_imageimportancesampler_gradient.png",
            256);
    }

    TEST_CASE(ImportanceSampleLightProbe)
    {
        generate_image(
            "data/lightprobe.raw",
            1024,
            512,
            "output/test_imageimportancesampler_lightprobe.png",
            256);
    }

    struct UniformBlackSampler
    {
        double operator()(const size_t x, const size_t y) const
        {
            return y == 0 ? 0.0 : 1.0;
        }
    };

    TEST_CASE(GetPDF_GivenBlackImage_ReturnsZeroDotFive)
    {
        UniformBlackSampler sampler;
        ImageImportanceSampler<double, UniformBlackSampler> importance_sampler(2, 2, sampler);

        const double pdf = importance_sampler.get_pdf(0, 1);

        EXPECT_EQ(0.5, pdf);
    }
}
