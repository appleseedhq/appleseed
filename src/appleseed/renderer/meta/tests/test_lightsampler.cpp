
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
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/rng.h"
#include "foundation/utility/test.h"

TEST_SUITE(Renderer_Kernel_Lighting_LightSampler)
{
    using namespace foundation;
    using namespace renderer;

    TEST_CASE(Sample_GivenSceneWithoutLights_ReturnsZeroSample)
    {
        const Scene scene;
        LightSampler light_sampler(scene);

        MersenneTwister rng;
        SamplingContext sampling_context(rng);
        LightSampleVector samples;

        light_sampler.sample(
            sampling_context,
            Vector3d(0.0),              // point
            Vector3d(1.0, 0.0, 0.0),    // normal
            1,                          // number of samples
            samples);

        EXPECT_EQ(0, samples.size());
    }
}
