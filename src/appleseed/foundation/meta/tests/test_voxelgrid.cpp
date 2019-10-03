
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
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/math/voxelgrid.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Math_VoxelGrid3)
{
    const size_t ChannelCount = 4;

    struct Fixture
    {
        VoxelGrid3<float, double> m_grid;

        Fixture()
          : m_grid(3, 3, 3, ChannelCount)
        {
            for (int z = 0; z < 3; ++z)
            {
                for (int y = 0; y < 3; ++y)
                {
                    for (int x = 0; x < 3; ++x)
                    {
                        float* values = m_grid.voxel(x, y, z);
                        values[0] = static_cast<float>(1 - std::abs(x - 1));
                        values[1] = static_cast<float>(1 - std::abs(y - 1));
                        values[2] = static_cast<float>(1 - std::abs(z - 1));
                        values[3] = 1.0f;
                    }
                }
            }
        }

        enum Interpolator
        {
            Nearest,
            Linear,
            Quadratic
        };

        void dump_constant_z_slice(
            const char*         filename,
            const Interpolator  interpolator,
            const double        z) const
        {
            const size_t ImageWidth = 512;
            const size_t ImageHeight = 512;

            Image image(
                ImageWidth,
                ImageHeight,
                ImageWidth,
                ImageHeight,
                3,
                PixelFormatFloat);

            for (size_t y = 0; y < ImageHeight; ++y)
            {
                for (size_t x = 0; x < ImageWidth; ++x)
                {
                    const Vector3d point(
                        static_cast<double>(x) / (ImageWidth - 1),
                        static_cast<double>(y) / (ImageHeight - 1),
                        z);

                    float values[ChannelCount];

                    switch (interpolator)
                    {
                      case Nearest:
                        m_grid.nearest_lookup(point, values);
                        break;

                      case Linear:
                        m_grid.linear_lookup(point, values);
                        break;

                      case Quadratic:
                        m_grid.quadratic_lookup(point, values);
                        break;
                    }

                    image.set_pixel(x, y, saturate(Color3f::from_array(values)));
                }
            }

            GenericImageFileWriter writer(filename);
            writer.append_image(&image);
            writer.write();
        }
    };

    //
    // Unfiltered lookup.
    //

    TEST_CASE_F(NearestLookup_AtOrigin, Fixture)
    {
        float values[ChannelCount];
        m_grid.nearest_lookup(Vector3d(0.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(NearestLookup_AtCenter, Fixture)
    {
        float values[ChannelCount];
        m_grid.nearest_lookup(Vector3d(0.5), values);

        EXPECT_FEQ(1.0f, values[0]);
        EXPECT_FEQ(1.0f, values[1]);
        EXPECT_FEQ(1.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(NearestLookup_AtExtremity, Fixture)
    {
        float values[ChannelCount];
        m_grid.nearest_lookup(Vector3d(1.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(NearestLookup_BeyondOrigin, Fixture)
    {
        float values[ChannelCount];
        m_grid.nearest_lookup(Vector3d(-1.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(NearestLookup_BeyondExtremity, Fixture)
    {
        float values[ChannelCount];
        m_grid.nearest_lookup(Vector3d(2.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(NearestLookup_Slice, Fixture)
    {
        dump_constant_z_slice(
            "unit tests/outputs/test_voxelgrid_nearest_lookup_slice.png",
            Nearest,
            0.5);
    }

    //
    // Linear lookup.
    //

    TEST_CASE_F(LinearLookup_AtOrigin, Fixture)
    {
        float values[ChannelCount];
        m_grid.linear_lookup(Vector3d(0.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(LinearLookup_HalfwayToCenter, Fixture)
    {
        float values[ChannelCount];
        m_grid.linear_lookup(Vector3d(0.25), values);

        EXPECT_FEQ(0.5f, values[0]);
        EXPECT_FEQ(0.5f, values[1]);
        EXPECT_FEQ(0.5f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(LinearLookup_AtCenter, Fixture)
    {
        float values[ChannelCount];
        m_grid.linear_lookup(Vector3d(0.5), values);

        EXPECT_FEQ(1.0f, values[0]);
        EXPECT_FEQ(1.0f, values[1]);
        EXPECT_FEQ(1.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(LinearLookup_AtExtremity, Fixture)
    {
        float values[ChannelCount];
        m_grid.linear_lookup(Vector3d(1.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(LinearLookup_BeyondOrigin, Fixture)
    {
        float values[ChannelCount];
        m_grid.linear_lookup(Vector3d(-1.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(LinearLookup_BeyondExtremity, Fixture)
    {
        float values[ChannelCount];
        m_grid.linear_lookup(Vector3d(2.0), values);

        EXPECT_FEQ(0.0f, values[0]);
        EXPECT_FEQ(0.0f, values[1]);
        EXPECT_FEQ(0.0f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(LinearLookup_Slice, Fixture)
    {
        dump_constant_z_slice(
            "unit tests/outputs/test_voxelgrid_linear_lookup_slice.png",
            Linear,
            0.5);
    }

    //
    // Quadratic lookup.
    //

    TEST_CASE_F(QuadraticLookup_AtOrigin, Fixture)
    {
        float values[ChannelCount];
        m_grid.quadratic_lookup(Vector3d(0.0), values);

        EXPECT_FEQ(0.125f, values[0]);
        EXPECT_FEQ(0.125f, values[1]);
        EXPECT_FEQ(0.125f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(QuadraticLookup_HalfwayToCenter, Fixture)
    {
        float values[ChannelCount];
        m_grid.quadratic_lookup(Vector3d(0.25), values);

        EXPECT_FEQ(0.5f, values[0]);
        EXPECT_FEQ(0.5f, values[1]);
        EXPECT_FEQ(0.5f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(QuadraticLookup_AtCenter, Fixture)
    {
        float values[ChannelCount];
        m_grid.quadratic_lookup(Vector3d(0.5), values);

        EXPECT_FEQ(0.75f, values[0]);
        EXPECT_FEQ(0.75f, values[1]);
        EXPECT_FEQ(0.75f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(QuadraticLookup_AtExtremity, Fixture)
    {
        float values[ChannelCount];
        m_grid.quadratic_lookup(Vector3d(1.0), values);

        EXPECT_FEQ(0.125f, values[0]);
        EXPECT_FEQ(0.125f, values[1]);
        EXPECT_FEQ(0.125f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(QuadraticLookup_BeyondOrigin, Fixture)
    {
        float values[ChannelCount];
        m_grid.quadratic_lookup(Vector3d(-1.0), values);

        EXPECT_FEQ(0.125f, values[0]);
        EXPECT_FEQ(0.125f, values[1]);
        EXPECT_FEQ(0.125f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(QuadraticLookup_BeyondExtremity, Fixture)
    {
        float values[ChannelCount];
        m_grid.quadratic_lookup(Vector3d(2.0), values);

        EXPECT_FEQ(0.125f, values[0]);
        EXPECT_FEQ(0.125f, values[1]);
        EXPECT_FEQ(0.125f, values[2]);
        EXPECT_FEQ(1.0f, values[3]);
    }

    TEST_CASE_F(QuadraticLookup_Slice, Fixture)
    {
        dump_constant_z_slice(
            "unit tests/outputs/test_voxelgrid_quadratic_lookup_slice.png",
            Quadratic,
            0.5);
    }

    //
    // Reference code for bilinear filtering.
    //

    Image create_test_image(const size_t w, const size_t h)
    {
        Image source(w, h, w, h, 3, PixelFormatFloat);

        for (size_t y = 0; y < h; ++y)
        {
            for (size_t x = 0; x < w; ++x)
            {
                const float intensity = static_cast<float>(~(x ^ y) & 1);
                source.set_pixel(x, y, Color3f(intensity));
            }
        }

        return source;
    }

    TEST_CASE(BilinearFiltering_Exploration)
    {
        const size_t SourceWidth = 3;
        const size_t SourceHeight = 2;

        const size_t TargetWidth = 96;
        const size_t TargetHeight = 64;

        const Image source = create_test_image(SourceWidth, SourceHeight);

        Image target(
            TargetWidth,
            TargetHeight,
            TargetWidth,
            TargetHeight,
            3,
            PixelFormatFloat);

        for (size_t y = 0; y < TargetHeight; ++y)
        {
            for (size_t x = 0; x < TargetWidth; ++x)
            {
                float fx = static_cast<float>(x) / (TargetWidth - 1);
                float fy = static_cast<float>(y) / (TargetHeight - 1);

                fx *= SourceWidth - 1;
                fy *= SourceHeight - 1;

                const size_t tx0 = truncate<size_t>(fx);
                const size_t ty0 = truncate<size_t>(fy);
                const size_t tx1 = std::min<size_t>(tx0 + 1, SourceWidth - 1);
                const size_t ty1 = std::min<size_t>(ty0 + 1, SourceHeight - 1);

                const float wx1 = fx - tx0;
                const float wy1 = fy - ty0;
                const float wx0 = 1.0f - wx1;
                const float wy0 = 1.0f - wy1;

                Color3f c00; source.get_pixel(tx0, ty0, c00);
                Color3f c10; source.get_pixel(tx1, ty0, c10);
                Color3f c01; source.get_pixel(tx0, ty1, c01);
                Color3f c11; source.get_pixel(tx1, ty1, c11);

                const Color3f result =
                    c00 * wx0 * wy0 +
                    c10 * wx1 * wy0 +
                    c01 * wx0 * wy1 +
                    c11 * wx1 * wy1;

                target.set_pixel(x, y, result);
            }
        }

        GenericImageFileWriter source_writer("unit tests/outputs/test_voxelgrid_bilinear_filtering_exploration_source.png");
        source_writer.append_image(&source);
        source_writer.write();

        GenericImageFileWriter target_writer("unit tests/outputs/test_voxelgrid_bilinear_filtering_exploration_target.png");
        target_writer.append_image(&target);
        target_writer.write();
    }
}
