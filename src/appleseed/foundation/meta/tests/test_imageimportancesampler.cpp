
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
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/drawing.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/math/qmc.h"
#include "foundation/math/sampling/imageimportancesampler.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>

using namespace foundation;

TEST_SUITE(Foundation_Math_Sampling_ImageImportanceSampler)
{
    class HorizontalGradientSampler
    {
      public:
        struct Payload {};

        explicit HorizontalGradientSampler(const size_t width)
          : m_width(width)
        {
        }

        void sample(const size_t x, const size_t y, Payload& payload, float& importance) const
        {
            importance = static_cast<float>(x) / (m_width - 1);
        }

      private:
        const size_t m_width;
    };

    TEST_CASE(GetPDF_ReturnsSameProbabilityAsSample)
    {
        const size_t Width = 5;
        const size_t Height = 5;

        ImageImportanceSampler<HorizontalGradientSampler::Payload, float> importance_sampler(Width, Height);
        HorizontalGradientSampler sampler(Width);
        importance_sampler.rebuild(sampler);

        size_t x, y;
        float prob_xy;
        importance_sampler.sample(Vector2f(0.3f, 0.7f), x, y, prob_xy);

        const float pdf = importance_sampler.get_pdf(x, y);

        EXPECT_EQ(prob_xy, pdf);
    }

    void generate_image(
        const char*     input_filename,
        const char*     output_image,
        const size_t    sample_count)
    {
        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read(input_filename));

        const size_t width = image->properties().m_canvas_width;
        const size_t height = image->properties().m_canvas_height;

        ImageImportanceSampler<ImageSampler::Payload, float> importance_sampler(width, height);
        ImageSampler sampler(*image.get());
        importance_sampler.rebuild(sampler);

        for (size_t i = 0; i < sample_count; ++i)
        {
            const size_t Bases[1] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, sample_count, i);

            size_t x, y;
            float prob_xy;
            importance_sampler.sample(s, x, y, prob_xy);

            APPLESEED_UNUSED const float pdf = importance_sampler.get_pdf(x, y);
            assert(pdf == prob_xy);

            // Compute the coordinates of the center of the pixel.
            const Vector2d point(
                (2.0 * x + 1.0) / (2.0 * width),
                (2.0 * y + 1.0) / (2.0 * height));

            Drawing::draw_dot(
                *image.get(),
                point,
                Color4f(1.0f, 0.0f, 1.0f, 1.0f));
        }

        GenericImageFileWriter writer(output_image);
        writer.append_image(image.get());
        writer.write();
    }

    TEST_CASE(ImportanceSampleWhiteImage)
    {
        generate_image(
            "unit tests/inputs/test_imageimportancesampler_white.exr",
            "unit tests/outputs/test_imageimportancesampler_white.png",
            256);
    }

    TEST_CASE(ImportanceSampleBlackImage)
    {
        generate_image(
            "unit tests/inputs/test_imageimportancesampler_black.exr",
            "unit tests/outputs/test_imageimportancesampler_black.png",
            256);
    }

    TEST_CASE(ImportanceSampleCheckerboardImage)
    {
        generate_image(
            "unit tests/inputs/test_imageimportancesampler_checkerboard.exr",
            "unit tests/outputs/test_imageimportancesampler_checkerboard.png",
            256);
    }

    TEST_CASE(ImportanceSampleGradientImage)
    {
        generate_image(
            "unit tests/inputs/test_imageimportancesampler_gradient.exr",
            "unit tests/outputs/test_imageimportancesampler_gradient.png",
            256);
    }

    TEST_CASE(ImportanceSampleLightProbe)
    {
        generate_image(
            "unit tests/inputs/test_imageimportancesampler_lightprobe.exr",
            "unit tests/outputs/test_imageimportancesampler_lightprobe.png",
            256);
    }

    struct UniformBlackImageSampler
    {
        struct Payload {};

        void sample(const size_t x, const size_t y, Payload& payload, float& importance) const
        {
            importance = 0.0f;
        }
    };

    TEST_CASE(Sample_GivenUniformBlackImage)
    {
        ImageImportanceSampler<UniformBlackImageSampler::Payload, float> importance_sampler(2, 2);
        UniformBlackImageSampler sampler;
        importance_sampler.rebuild(sampler);

        size_t x, y;
        float prob_xy;
        importance_sampler.sample(Vector2f(0.0f, 0.0f), x, y, prob_xy);

        EXPECT_EQ(0, x);
        EXPECT_EQ(0, y);
        EXPECT_EQ(0.25f, prob_xy);
    }

    TEST_CASE(GetPDF_GivenUniformBlackImage)
    {
        ImageImportanceSampler<UniformBlackImageSampler::Payload, float> importance_sampler(2, 2);
        UniformBlackImageSampler sampler;
        importance_sampler.rebuild(sampler);

        const float pdf = importance_sampler.get_pdf(0, 1);

        EXPECT_EQ(0.25f, pdf);
    }

    TEST_CASE(Sample_SinglePrecision)
    {
        // With single precision weights and this particular image, we used to hit an assertion
        // checking that begin != end in sample_cdf(). This was due to the last few entries in
        // the CDF to be smaller than 1 while corresponding to items with null importance.

        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read("unit tests/inputs/test_imageimportancesampler_doge2.exr"));

        const size_t width = image->properties().m_canvas_width;
        const size_t height = image->properties().m_canvas_height;

        ImageImportanceSampler<ImageSampler::Payload, float> importance_sampler(width, height);
        ImageSampler sampler(*image.get());
        importance_sampler.rebuild(sampler);

        size_t x, y;
        float prob_xy;
        importance_sampler.sample(Vector2f(0.466647536f, 0.999999762f), x, y, prob_xy);

        EXPECT_GT(0.0f, prob_xy);
    }
}
