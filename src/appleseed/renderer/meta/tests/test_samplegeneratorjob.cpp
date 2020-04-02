
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/rendering/progressive/samplegeneratorjob.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdint>
#include <vector>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Kernel_Rendering_Progressive_SampleGeneratorJob_SamplingProfile)
{
    TEST_CASE(PlotSamplesPerJob)
    {
        const std::uint64_t N = 1500000;

        const SampleGeneratorJob::SamplingProfile sampling_profile;

        std::vector<Vector2d> points;
        points.reserve(N);

        for (std::uint64_t x = 0; x < N; x += 100)
        {
            const std::uint64_t y = sampling_profile.get_job_sample_count(x);
            points.emplace_back(
                static_cast<double>(x),
                static_cast<double>(y));
        }

        GnuplotFile plotfile;
        plotfile.set_title("Number of samples/job as a function of the number of samples already rendered");
        plotfile.set_yrange(0.0, 300000.0);
        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_samplegeneratorjob.gnuplot");
    }
}
