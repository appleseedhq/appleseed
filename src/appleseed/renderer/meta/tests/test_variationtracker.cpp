
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/rendering/final/variationtracker.h"

// appleseed.foundation headers.
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/vector.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Kernel_Rendering_Final_VariationTracker)
{
    TEST_CASE(GetSize_GivenDefaultState_ReturnsZero)
    {
        VariationTracker tracker;

        EXPECT_EQ(0, tracker.get_size());
    }

    TEST_CASE(GetMean_GivenDefaultState_ReturnsZero)
    {
        VariationTracker tracker;

        EXPECT_EQ(0.0f, tracker.get_mean());
    }

    TEST_CASE(GetVariation_GivenDefaultState_ReturnsZero)
    {
        VariationTracker tracker;

        EXPECT_EQ(0.0f, tracker.get_variation());
    }

    TEST_CASE(GetSize_GivenSingleValue_ReturnsOne)
    {
        VariationTracker tracker;
        tracker.insert(5.0f);

        EXPECT_EQ(1, tracker.get_size());
    }

    TEST_CASE(GetMean_GivenSingleValue_ReturnsValue)
    {
        VariationTracker tracker;
        tracker.insert(5.0f);

        EXPECT_EQ(5.0f, tracker.get_mean());
    }

    TEST_CASE(GetVariation_GivenSingleValue_ReturnsZero)
    {
        VariationTracker tracker;
        tracker.insert(5.0f);

        EXPECT_EQ(0.0f, tracker.get_variation());
    }

    TEST_CASE(GetMean_GivenTwoValues_ReturnsMeanValue)
    {
        VariationTracker tracker;
        tracker.insert(2.0f);
        tracker.insert(4.0f);

        EXPECT_EQ(3.0f, tracker.get_mean());
    }

    TEST_CASE(GetVariation_GivenTwoValues_ReturnsNormalizedMeanSpread)
    {
        VariationTracker tracker;
        tracker.insert(2.0f);       // spread is now 0, mean is now 2
        tracker.insert(4.0f);       // spread is now 1, mean is now 3

        EXPECT_FEQ(1.0f / 3.0f, tracker.get_variation());
    }

    TEST_CASE(GeneratePlotFile)
    {
        MersenneTwister rng;
        VariationTracker tracker;
        vector<Vector2f> mean, variation;

        for (size_t i = 0; i < 1024; ++i)
        {
            if (i % 64 == 0)
                tracker.reset_variation();

            tracker.insert(rand_float1(rng));

            const float s = static_cast<float>(i);

            mean.push_back(Vector2f(s, tracker.get_mean()));
            variation.push_back(Vector2f(s, tracker.get_variation()));
        }

        GnuplotFile plotfile;
        plotfile.set_xlabel("Samples");
        plotfile
            .new_plot()
            .set_points(mean)
            .set_title("Mean");
        plotfile
            .new_plot()
            .set_points(variation)
            .set_title("Variation");
        plotfile.write("unit tests/outputs/test_variationtracker.gnuplot");
    }
}
