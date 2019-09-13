
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

// appleseed.foundation headers.
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/xorshift32.h"
#include "foundation/math/sampling/imageimportancesampler.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>
#include <memory>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Sampling_ImageImportanceSampler)
{
    struct Fixture
    {
        typedef ImageImportanceSampler<ImageSampler::Payload, float> ImportanceSamplerType;

        std::unique_ptr<ImportanceSamplerType>   m_importance_sampler;
        Xorshift32                               m_rng;

        Vector2u                                 m_texel_coords_sum;
        float                                    m_texel_prob_sum;

        Fixture()
          : m_texel_coords_sum(0, 0)
          , m_texel_prob_sum(0.0f)
        {
            GenericImageFileReader reader;
            std::unique_ptr<Image> image(reader.read("unit tests/inputs/test_imageimportancesampler_doge2.exr"));

            const size_t width = image->properties().m_canvas_width;
            const size_t height = image->properties().m_canvas_height;

            m_importance_sampler.reset(new ImportanceSamplerType(width, height));
            ImageSampler sampler(*image.get());
            m_importance_sampler->rebuild(sampler);
        }
    };

    BENCHMARK_CASE_F(Sample, Fixture)
    {
        const Vector2f s = rand_vector2<Vector2f>(m_rng);

        Vector2u texel_coords;
        float texel_prob;
        m_importance_sampler->sample(s, texel_coords.x, texel_coords.y, texel_prob);

        m_texel_coords_sum += texel_coords;
        m_texel_prob_sum += texel_prob;
    }
}
