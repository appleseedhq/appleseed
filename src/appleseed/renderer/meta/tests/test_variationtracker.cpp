
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/rendering/generic/variationtracker.h"

// appleseed.foundation headers.
#include "foundation/math/rng.h"
#include "foundation/utility/maplefile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Kernel_Rendering_Generic_VariationTracker)
{
    TEST_CASE(Visualize)
    {
        VariationTracker tracker;
        MersenneTwister rng;
        vector<float> samples, mean, variation;

        for (size_t i = 0; i < 256; ++i)
        {
            tracker.insert(rand_float1(rng));
            samples.push_back(static_cast<float>(i));
            mean.push_back(tracker.get_mean());
            variation.push_back(tracker.get_variation());
        }

        MapleFile file("unit tests/outputs/test_variationtracker.mpl");
        file.define("mean", samples, mean);
        file.define("variation", samples, variation);
        file.plot("variation", "Variation");
        file.plot("mean", "Mean");
    }
}
