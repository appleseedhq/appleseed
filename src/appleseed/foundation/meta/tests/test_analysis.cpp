
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
#include "foundation/image/analysis.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/fp.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Image_Analysis)
{
    TEST_CASE(ComputeAverageLuminance_GivenImageFilledWithZeroes_ReturnsZero)
    {
        Image image(4, 4, 2, 2, 4, PixelFormatFloat);
        image.clear(Color4f(0.0f));

        const double average_luminance = compute_average_luminance(image);

        EXPECT_EQ(0.0, average_luminance);
    }

    TEST_CASE(ComputeAverageLuminance_GivenImageFilledWithOnes_ReturnsOne)
    {
        Image image(4, 4, 2, 2, 4, PixelFormatFloat);
        image.clear(Color4f(1.0f));

        const double average_luminance = compute_average_luminance(image);

        EXPECT_FEQ_EPS(1.0, average_luminance, 1.0e-6);
    }

    TEST_CASE(ComputeAverageLuminance_GivenImageFilledWithMinusOnes_ReturnsZero)
    {
        Image image(4, 4, 2, 2, 4, PixelFormatFloat);
        image.clear(Color4f(-1.0f));

        const double average_luminance = compute_average_luminance(image);

        EXPECT_EQ(0.0, average_luminance);
    }

    TEST_CASE(ComputeAverageLuminance_GivenImageFilledWithOnesAndOnePixelSetToSNaN_ReturnsOne)
    {
        Image image(4, 4, 2, 2, 4, PixelFormatFloat);
        image.clear(Color4f(1.0f));
        image.tile(0, 0).set_pixel(1, 1, Color4f(FP<float>::snan()));

        const double average_luminance = compute_average_luminance(image);

        EXPECT_FEQ_EPS(1.0, average_luminance, 1.0e-6);
    }

    TEST_CASE(ComputeRMSDeviation_GivenBothImagesFilledWithZeroes_ReturnsZero)
    {
        Image image1(4, 4, 2, 2, 4, PixelFormatFloat);
        Image image2(4, 4, 2, 2, 4, PixelFormatFloat);

        image1.clear(Color4f(0.0f));
        image2.clear(Color4f(0.0f));

        const double rmsd = compute_rms_deviation(image1, image2);

        EXPECT_EQ(0.0, rmsd);
    }

    TEST_CASE(ComputeRMSDeviation_GivenBothImagesFilledWithOnes_ReturnsZero)
    {
        Image image1(4, 4, 2, 2, 4, PixelFormatFloat);
        Image image2(4, 4, 2, 2, 4, PixelFormatFloat);

        image1.clear(Color4f(1.0f));
        image2.clear(Color4f(1.0f));

        const double rmsd = compute_rms_deviation(image1, image2);

        EXPECT_EQ(0.0, rmsd);
    }

    TEST_CASE(ComputeRMSDeviation_GivenOneImageFilledWithZeroesAndOneImageFilledWithOnes_ReturnsOne)
    {
        Image image1(4, 4, 2, 2, 4, PixelFormatFloat);
        Image image2(4, 4, 2, 2, 4, PixelFormatFloat);

        image1.clear(Color4f(0.0f));
        image2.clear(Color4f(1.0f));

        const double rmsd = compute_rms_deviation(image1, image2);

        EXPECT_FEQ(1.0, rmsd);
    }

    TEST_CASE(ComputeRMSDeviation_GivenImagesOfDifferentChannelSizes_ReturnsOne)
    {
        Image image1(4, 4, 2, 2, 3, PixelFormatFloat);
        Image image2(4, 4, 2, 2, 4, PixelFormatFloat);

        image1.clear(Color3f(0.0f));
        image2.clear(Color4f(1.0f));

        const double rmsd = compute_rms_deviation(image1, image2);

        EXPECT_FEQ(1.0, rmsd);
    }

    TEST_CASE(ComputeRMSDeviation_GivenImagesOfDifferentTileSizes_ReturnsOne)
    {
        Image image1(4, 4, 1, 1, 4, PixelFormatFloat);
        Image image2(4, 4, 2, 2, 4, PixelFormatFloat);

        image1.clear(Color4f(0.0f));
        image2.clear(Color4f(1.0f));

        const double rmsd = compute_rms_deviation(image1, image2);

        EXPECT_FEQ(1.0, rmsd);
    }

    TEST_CASE(ComputeRMSDeviation_GivenRedImageAndBlueImage_ReturnsRootTwoThirds)
    {
        Image image1(4, 4, 2, 2, 4, PixelFormatFloat);
        Image image2(4, 4, 2, 2, 4, PixelFormatFloat);

        image1.clear(Color4f(1.0f, 0.0f, 0.0f, 0.0f));
        image2.clear(Color4f(0.0f, 1.0f, 0.0f, 0.0f));

        const double rmsd = compute_rms_deviation(image1, image2);

        EXPECT_FEQ_EPS(std::sqrt(2.0 / 3.0), rmsd, 1.0e-6);
    }
}
