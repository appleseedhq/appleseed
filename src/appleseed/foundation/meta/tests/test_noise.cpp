
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
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/noise.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Math_Noise)
{
    template <typename Function>
    void generate_image(
        const char*     image_filename,
        const size_t    image_width,
        const size_t    image_height,
        Function&       function)
    {
        Image image(
            image_width,
            image_height,
            32,
            32,
            3,
            PixelFormatFloat);

        for (size_t y = 0; y < image_height; ++y)
        {
            for (size_t x = 0; x < image_width; ++x)
            {
                const Vector2f p(
                    static_cast<float>(x) / (image_width - 1),
                    static_cast<float>(y) / (image_height - 1));

                const float value = function(p);

                image.set_pixel(x, y, Color3f(value));
            }
        }

        GenericImageFileWriter writer(image_filename);
        writer.append_image(&image);
        writer.write();
    }

    float noise_function(const Vector2f& p)
    {
        const Vector3f q(p.x * 8.0f, p.y * 8.0f, 0.5f);
        const float value = noise(q);
        return 0.5f * (1.0f + value);
    }

    float fbm_function(const Vector2f& p)
    {
        const Vector3f q(p.x * 8.0f, p.y * 8.0f, 0.5f);
        const float value =
            fbm(
                q,
                3,          // octaves
                2.0f,       // lacunarity
                0.5f);      // gain
        return 0.5f * (1.0f + value);
    }

    float turbulence_function(const Vector2f& p)
    {
        const Vector3f q(p.x * 4.0f, p.y * 4.0f, 0.5f);
        return
            turbulence(
                q,
                3,          // octaves
                2.0f,       // lacunarity
                0.5f);      // gain
    }

    TEST_CASE(GenerateNoise3Image)
    {
        generate_image(
            "unit tests/outputs/test_noise_noise.png",
            64,
            64,
            noise_function);
    }

    TEST_CASE(GenerateFBM3Image)
    {
        generate_image(
            "unit tests/outputs/test_noise_fbm.png",
            64,
            64,
            fbm_function);
    }

    TEST_CASE(GenerateTurbulence3Image)
    {
        generate_image(
            "unit tests/outputs/test_noise_turbulence.png",
            64,
            64,
            turbulence_function);
    }
}
