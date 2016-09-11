
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/qmc.h"
#include "foundation/math/sampling/sphericalimportancesampler.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"
#include "foundation/utility/vpythonfile.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Math_Sampling_SphericalImportanceSampler)
{
    TEST_CASE(Test)
    {
        const SphericalImportanceSampler<double> sampler(2);

        EXPECT_TRUE(sampler.dump_as_obj("unit tests/outputs/test_sphericalimportancesampler.obj"));

        VPythonFile file("unit tests/outputs/test_sphericalimportancesampler.py");
        file.draw_axes();

        sampler.dump_to_vpython_file(file);

        const size_t N = 20000;

        for (size_t i = 0; i < N; ++i)
        {
            static const size_t Bases[] = { 2, 3 };
            const Vector3d s = hammersley_sequence<double, 3>(Bases, N, i);

            const Vector3d p = sampler.sample(s);
            file.draw_point(p, "color.white", 1);
        }
    }
}
