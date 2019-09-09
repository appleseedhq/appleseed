
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/cdf.h"
#include "foundation/math/filter.h"
#include "foundation/math/filtersamplingtable.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/xoroshiro128plus.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <fstream>

using namespace foundation;

namespace
{
    template <typename Filter1, typename Filter2>
    void generate_sample_plot_compare(
        const Filter1&  filter1,
        const Filter2&  filter2,
        const size_t    sample_count,
        const char*     filename)
    {
        std::ofstream ofs(filename);
        Xoroshiro128plus rng;

        // Generate samples using the table.
        FilterSamplingTable table(filter1);

        ofs << "points1 = [\n";

        for (size_t i = 0; i < sample_count; ++i)
        {
            const float sx = rand_float1(rng);
            const float sy = rand_float1(rng);
            const Vector2f p(table.sample(sx), table.sample(sy));

            if (p.x < 0.0f)
            {
                ofs << "(" << p.x << ", " << p.y << ")";

                if (i != sample_count - 1)
                    ofs << ", ";

                ofs << "\n";
            }
        }

        ofs << "]\n";
        ofs << "\n";

        // Generate samples using the 2D filter and rejection sampling.
        const float rx = filter2.get_xradius();
        const float ry = filter2.get_yradius();
        const float rcp_norm = 1.0f / compute_normalization_factor(filter2);

        ofs << "points2 = [\n";

        for (size_t i = 0; i < sample_count; ++i)
        {
            while (true)
            {
                const float sx = rand_float1(rng);
                const float sy = rand_float1(rng);

                const float x = lerp(-rx, rx, sx);
                const float y = lerp(-ry, ry, sy);

                const float value = filter2.evaluate(x, y) * rcp_norm;

                if (value > rand_float1(rng))
                {
                    if (x > 0.0f)
                    {
                        ofs << "(" << x << ", " << y << ")";

                        if (i != sample_count - 1)
                            ofs << ", ";

                        ofs << "\n";
                        break;
                    }
                }
            }
        }

        ofs << "]\n";
        ofs << "\n";

        ofs << "import matplotlib.pyplot as plt\n";

        ofs << "plt.axis('equal')\n";
        ofs << "plt.axis(["
            << -rx << ", " << rx * 1.25f << ", "
            << -ry << ", " << ry * 1.25f << "])\n";

        ofs << "x, y = zip(*points1)\n";
        ofs << "plt.scatter(x, y,s=1, c='r')\n";

        ofs << "x, y = zip(*points2)\n";
        ofs << "plt.scatter(x, y,s=1, c='b')\n";

        ofs << "plt.show()\n";

        ofs.close();
    }
}

TEST_SUITE(Foundation_Image_FilterSamplingTable_BoxFilter)
{
    TEST_CASE(PlotSamples)
    {
        const float Radius = 1.5f;
        const size_t SampleCount = 2048;

        const BoxFilter1<float> filter1(Radius);
        const BoxFilter2<float> filter2(Radius, Radius);
        generate_sample_plot_compare(
            filter1,
            filter2,
            SampleCount,
            "unit tests/outputs/test_fis_plot_box.py");
    }
}

TEST_SUITE(Foundation_Image_FilterSamplingTable_TriangleFilter)
{
    TEST_CASE(PlotSamples)
    {
        const float Radius = 1.5f;
        const size_t SampleCount = 2048;

        const TriangleFilter1<float> filter1(Radius);
        const TriangleFilter2<float> filter2(Radius, Radius);
        generate_sample_plot_compare(
            filter1,
            filter2,
            SampleCount,
            "unit tests/outputs/test_fis_plot_triangle.py");
    }
}

TEST_SUITE(Foundation_Image_FilterSamplingTable_GaussianFilter)
{
    TEST_CASE(PlotSamples)
    {
        const float Alpha = 8.0f;
        const float Radius = 1.5f;
        const size_t SampleCount = 2048;

        const GaussianFilter1<float> filter1(Radius, Alpha);
        const GaussianFilter2<float> filter2(Radius, Radius, Alpha);
        generate_sample_plot_compare(
            filter1,
            filter2,
            SampleCount,
            "unit tests/outputs/test_fis_plot_gaussian.py");
    }
}

TEST_SUITE(Foundation_Image_FilterSamplingTable_BlackmanFilter)
{
    TEST_CASE(PlotSamples)
    {
        const float Radius = 1.5f;
        const size_t SampleCount = 2048;

        const BlackmanHarrisFilter1<float> filter1(Radius);
        const BlackmanHarrisFilter2<float> filter2(Radius, Radius);
        generate_sample_plot_compare(
            filter1,
            filter2,
            SampleCount,
            "unit tests/outputs/test_fis_plot_blackman.py");
    }
}
