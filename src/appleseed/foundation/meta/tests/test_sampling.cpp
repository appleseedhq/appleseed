
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/math/fp.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/sampling.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"
#include "foundation/utility/testutils.h"
#include "foundation/utility/vpythonfile.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Sampling_QMCSamplingContext)
{
    typedef MersenneTwister RNG;
    typedef QMCSamplingContext<RNG> QMCSamplingContext;
    typedef QMCSamplingContext::VectorType VectorType;

    TEST_CASE(InitialStateIsCorrect)
    {
        RNG rng;
        QMCSamplingContext context(rng, 2, 64, 7);

        EXPECT_EQ(0, context.m_base_dimension);
        EXPECT_EQ(0, context.m_base_instance);
        EXPECT_EQ(2, context.m_dimension);
        EXPECT_EQ(7, context.m_instance);
        EXPECT_EQ(VectorType(0.0), context.m_offset);
    }

    TEST_CASE(TestAssignmentOperator)
    {
        RNG rng;
        QMCSamplingContext original_parent(rng, 2, 64, 7);
        QMCSamplingContext original = original_parent.split(3, 16);
        original.set_instance(6);

        QMCSamplingContext copy(rng, 4, 16, 9);
        copy = original;

        EXPECT_EQ(2, copy.m_base_dimension);
        EXPECT_EQ(7, copy.m_base_instance);
        EXPECT_EQ(3, copy.m_dimension);
        EXPECT_EQ(6, copy.m_instance);
    }

    TEST_CASE(TestSplitting)
    {
        RNG rng;
        QMCSamplingContext context(rng, 2, 64, 7);
        QMCSamplingContext child_context = context.split(3, 16);

        EXPECT_EQ(2, child_context.m_base_dimension);
        EXPECT_EQ(7, child_context.m_base_instance);
        EXPECT_EQ(3, child_context.m_dimension);
        EXPECT_EQ(0, child_context.m_instance);
    }

    TEST_CASE(TestDoubleSplitting)
    {
        RNG rng;
        QMCSamplingContext context(rng, 2, 64, 7);
        QMCSamplingContext child_context = context.split(3, 16);
        QMCSamplingContext child_child_context = child_context.split(4, 8);

        EXPECT_EQ(5, child_child_context.m_base_dimension);
        EXPECT_EQ(7, child_child_context.m_base_instance);
        EXPECT_EQ(4, child_child_context.m_dimension);
        EXPECT_EQ(0, child_child_context.m_instance);
    }
}

TEST_SUITE(Foundation_Math_Sampling_QMCSamplingContext_DirectIlluminationSimulation)
{
    typedef MersenneTwister RNG;
    typedef QMCSamplingContext<RNG> QMCSamplingContext;

    void shade(
        const QMCSamplingContext&   context,
        const size_t                light_sample_count,
        vector<Vector2d>&           light_samples)
    {
        QMCSamplingContext child_context = context.split(2, light_sample_count);

        for (size_t i = 0; i < light_sample_count; ++i)
        {
            const Vector2d s = child_context.next_vector2<2>();
            light_samples.push_back(s);
        }
    }

    void render(
        const size_t                pixel_sample_count,
        const size_t                light_sample_count)
    {
        RNG rng;
        QMCSamplingContext sampling_context(rng, 2, pixel_sample_count, 0);

        vector<Vector2d> pixel_samples;
        vector<Vector2d> light_samples;

        for (size_t i = 0; i < pixel_sample_count; ++i)
        {
            const Vector2d s = sampling_context.next_vector2<2>();

            pixel_samples.push_back(s);

            shade(sampling_context, light_sample_count, light_samples);
        }

        const string title =
            string("unit tests/outputs/test_sampling_") +
            "P" + to_string(pixel_sample_count) + "_" +
            "L" + to_string(light_sample_count);

        write_point_cloud_image(title + "_pixel_samples.png", pixel_samples);
        write_point_cloud_image(title + "_light_samples.png", light_samples);
    }

    TEST_CASE(TestWith1PixelSampleAnd256LightSamples)
    {
        render(1, 256);
    }

    TEST_CASE(TestWith16PixelSamplesAnd16LightSamples)
    {
        render(16, 16);
    }

    TEST_CASE(TestWith256PixelSamplesAnd1LightSample)
    {
        render(256, 1);
    }
}

TEST_SUITE(Foundation_Math_Sampling_Mappings)
{
    TEST_CASE(SampleHemisphereUniform_GivenZeroZero_ReturnsSampleWithYComponentGreaterThanZero)
    {
        EXPECT_GT(0.0, sample_hemisphere_uniform(Vector2d(0.0)).y);
    }

    TEST_CASE(SampleHemisphereUniform_GivenAlmostOneOne_ReturnsSampleWithYComponentGreaterThanZero)
    {
        EXPECT_GT(0.0, sample_hemisphere_uniform(Vector2d(shift(1.0, -1))).y);
    }

    TEST_CASE(SampleHemisphereCosinePower1_GivenZeroZero_ReturnsSampleWithYComponentGreaterThanZero)
    {
        EXPECT_GT(0.0, sample_hemisphere_cosine(Vector2d(0.0)).y);
    }

    TEST_CASE(SampleHemisphereCosinePower1_GivenAlmostOneOne_ReturnsSampleWithYComponentGreaterThanZero)
    {
        EXPECT_GT(0.0, sample_hemisphere_cosine(Vector2d(shift(1.0, -1))).y);
    }

    TEST_CASE(SampleHemisphereCosinePowerN_GivenZeroZero_ReturnsSampleWithYComponentGreaterThanZero)
    {
        EXPECT_GT(0.0, sample_hemisphere_cosine_power(Vector2d(0.0), 10.0).y);
    }

    TEST_CASE(SampleHemisphereCosinePowerN_GivenAlmostOneOne_ReturnsSampleWithYComponentGreaterThanZero)
    {
        EXPECT_GT(0.0, sample_hemisphere_cosine_power(Vector2d(shift(1.0, -1)), 10.0).y);
    }

    template <typename T>
    Vector<T, 2> to_unit_square(const Vector<T, 2>& p)
    {
        return Vector<T, 2>(0.5) + T(0.4) * p;
    }

    template <typename SamplingFunction>
    void visualize_2d_function_as_image(
        const string&       filename,
        SamplingFunction&   sampling_function,
        const size_t        point_count)
    {
        vector<Vector2d> points(point_count);

        for (size_t i = 0; i < point_count; ++i)
        {
            const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, i, point_count);
            points[i] = to_unit_square(sampling_function(s));
        }

        write_point_cloud_image(filename, 512, 512, points);
    }

    template <typename SamplingFunction>
    void visualize_3d_function_as_vpython_program(
        const string&       filename,
        SamplingFunction&   sampling_function,
        const size_t        point_count)
    {
        vector<Vector3d> points(point_count);

        for (size_t i = 0; i < point_count; ++i)
        {
            const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, i, point_count);
            points[i] = sampling_function(s);
        }

        VPythonFile file(filename);
        file.draw_points(points.size(), &points.front());
    }

    TEST_CASE(SampleSphereUniform_GenerateVPythonProgram)
    {
        visualize_3d_function_as_vpython_program(
            "unit tests/outputs/test_sampling_sample_sphere_uniform.py",
            sample_sphere_uniform<double>,
            1024);
    }

    TEST_CASE(SampleHemisphereUniform_GenerateVPythonProgram)
    {
        visualize_3d_function_as_vpython_program(
            "unit tests/outputs/test_sampling_sample_hemisphere_uniform.py",
            sample_hemisphere_uniform<double>,
            512);
    }

    TEST_CASE(SampleHemisphereCosinePower1_GenerateVPythonProgram)
    {
        visualize_3d_function_as_vpython_program(
            "unit tests/outputs/test_sampling_sample_hemisphere_cosine_power_1.py",
            sample_hemisphere_cosine<double>,
            512);
    }

    template <typename T>
    Vector<T, 3> sample_hemisphere_cosine_power_10(const Vector<T, 2>& s)
    {
        return sample_hemisphere_cosine_power(s, T(10.0));
    }

    TEST_CASE(SampleHemisphereCosinePowerN_GenerateVPythonProgram)
    {
        visualize_3d_function_as_vpython_program(
            "unit tests/outputs/test_sampling_sample_hemisphere_cosine_power_10.py",
            sample_hemisphere_cosine_power_10<double>,
            512);
    }

    TEST_CASE(SampleDiskUniform_GenerateImage)
    {
        visualize_2d_function_as_image(
            "unit tests/outputs/test_sampling_sample_disk_uniform.png",
            sample_disk_uniform<double>,
            256);
    }

    TEST_CASE(SampleDiskUniformAlt_GenerateImage)
    {
        visualize_2d_function_as_image(
            "unit tests/outputs/test_sampling_sample_disk_uniform_alt.png",
            sample_disk_uniform_alt<double>,
            256);
    }

    template <typename T>
    Vector<T, 3> sample_cone_uniform(const Vector<T, 2>& s)
    {
        return sample_cone_uniform(s, cos(deg_to_rad(30.0)));
    }

    TEST_CASE(SampleConeUniform_GenerateVPythonProgram)
    {
        visualize_3d_function_as_vpython_program(
            "unit tests/outputs/test_sampling_sample_cone_uniform.py",
            sample_cone_uniform<double>,
            256);
    }

    TEST_CASE(SampleTriangleUniform_GenerateVPythonProgram)
    {
        visualize_3d_function_as_vpython_program(
            "unit tests/outputs/test_sampling_sample_triangle_uniform.py",
            sample_triangle_uniform<double>,
            256);
    }

    TEST_CASE(SampleRegularPolygonUniform_GenerateVPythonProgram)
    {
        const size_t VertexCount = 6;
        const size_t PointCount = 512;

        Vector2d vertices[VertexCount];
        build_regular_polygon(VertexCount, 0.0, vertices);

        vector<Vector2d> points(PointCount);

        for (size_t i = 0; i < PointCount; ++i)
        {
            const size_t Bases[] = { 2, 3 };
            const Vector3d s = hammersley_sequence<double, 3>(Bases, i, PointCount);
            const Vector2d p = sample_regular_polygon_uniform(s, VertexCount, vertices);
            points[i] = to_unit_square(p);
        }

        write_point_cloud_image(
            "unit tests/outputs/test_sampling_sample_regular_polygon_uniform.png",
            512, 512,
            points);
    }
}
