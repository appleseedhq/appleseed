
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
#include "foundation/math/permutation.h"
#include "foundation/math/primes.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/arch.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"
#include "foundation/utility/testutils.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_QMC)
{
    TEST_CASE(RadicalInverseBase2)
    {
        EXPECT_FEQ(0.0,     radical_inverse_base2<double>(0));
        EXPECT_FEQ(0.5,     radical_inverse_base2<double>(1));
        EXPECT_FEQ(0.25,    radical_inverse_base2<double>(2));
        EXPECT_FEQ(0.75,    radical_inverse_base2<double>(3));
        EXPECT_FEQ(0.125,   radical_inverse_base2<double>(4));
        EXPECT_FEQ(0.625,   radical_inverse_base2<double>(5));
        EXPECT_FEQ(0.375,   radical_inverse_base2<double>(6));
        EXPECT_FEQ(0.875,   radical_inverse_base2<double>(7));
    }

    TEST_CASE(RadicalInverse_Base2)
    {
        EXPECT_FEQ(0.0,     radical_inverse<double>(2, 0));
        EXPECT_FEQ(0.5,     radical_inverse<double>(2, 1));
        EXPECT_FEQ(0.25,    radical_inverse<double>(2, 2));
        EXPECT_FEQ(0.75,    radical_inverse<double>(2, 3));
        EXPECT_FEQ(0.125,   radical_inverse<double>(2, 4));
        EXPECT_FEQ(0.625,   radical_inverse<double>(2, 5));
        EXPECT_FEQ(0.375,   radical_inverse<double>(2, 6));
        EXPECT_FEQ(0.875,   radical_inverse<double>(2, 7));
    }

    TEST_CASE(PermutedRadicalInverse_Base2_IdentityPermutation)
    {
        static const size_t Perm[] = { 0, 1 };
        EXPECT_FEQ(0.0,     permuted_radical_inverse<double>(2, Perm, 0));
        EXPECT_FEQ(0.5,     permuted_radical_inverse<double>(2, Perm, 1));
        EXPECT_FEQ(0.25,    permuted_radical_inverse<double>(2, Perm, 2));
        EXPECT_FEQ(0.75,    permuted_radical_inverse<double>(2, Perm, 3));
        EXPECT_FEQ(0.125,   permuted_radical_inverse<double>(2, Perm, 4));
        EXPECT_FEQ(0.625,   permuted_radical_inverse<double>(2, Perm, 5));
        EXPECT_FEQ(0.375,   permuted_radical_inverse<double>(2, Perm, 6));
        EXPECT_FEQ(0.875,   permuted_radical_inverse<double>(2, Perm, 7));
    }

    TEST_CASE(PermutedRadicalInverse_Base2_ReversePermutation)
    {
        static const size_t Perm[] = { 1, 0 };
        EXPECT_FEQ(1.0,     permuted_radical_inverse<double>(2, Perm, 0));
        EXPECT_FEQ(0.5,     permuted_radical_inverse<double>(2, Perm, 1));
        EXPECT_FEQ(0.75,    permuted_radical_inverse<double>(2, Perm, 2));
        EXPECT_FEQ(0.25,    permuted_radical_inverse<double>(2, Perm, 3));
        EXPECT_FEQ(0.875,   permuted_radical_inverse<double>(2, Perm, 4));
        EXPECT_FEQ(0.375,   permuted_radical_inverse<double>(2, Perm, 5));
        EXPECT_FEQ(0.625,   permuted_radical_inverse<double>(2, Perm, 6));
        EXPECT_FEQ(0.125,   permuted_radical_inverse<double>(2, Perm, 7));
    }

    TEST_CASE(FastPermutedRadicalInverse_Base2_IdentityPermutation)
    {
        static const size_t Perm[] = { 0, 1 };
        EXPECT_FEQ(0.0,     fast_permuted_radical_inverse<double>(0, Perm, 0));
        EXPECT_FEQ(0.5,     fast_permuted_radical_inverse<double>(0, Perm, 1));
        EXPECT_FEQ(0.25,    fast_permuted_radical_inverse<double>(0, Perm, 2));
        EXPECT_FEQ(0.75,    fast_permuted_radical_inverse<double>(0, Perm, 3));
        EXPECT_FEQ(0.125,   fast_permuted_radical_inverse<double>(0, Perm, 4));
        EXPECT_FEQ(0.625,   fast_permuted_radical_inverse<double>(0, Perm, 5));
        EXPECT_FEQ(0.375,   fast_permuted_radical_inverse<double>(0, Perm, 6));
        EXPECT_FEQ(0.875,   fast_permuted_radical_inverse<double>(0, Perm, 7));
    }

    TEST_CASE(FastPermutedRadicalInverse_Base2_ReversePermutation)
    {
        static const size_t Perm[] = { 1, 0 };
        EXPECT_FEQ(1.0,     fast_permuted_radical_inverse<double>(0, Perm, 0));
        EXPECT_FEQ(0.5,     fast_permuted_radical_inverse<double>(0, Perm, 1));
        EXPECT_FEQ(0.75,    fast_permuted_radical_inverse<double>(0, Perm, 2));
        EXPECT_FEQ(0.25,    fast_permuted_radical_inverse<double>(0, Perm, 3));
        EXPECT_FEQ(0.875,   fast_permuted_radical_inverse<double>(0, Perm, 4));
        EXPECT_FEQ(0.375,   fast_permuted_radical_inverse<double>(0, Perm, 5));
        EXPECT_FEQ(0.625,   fast_permuted_radical_inverse<double>(0, Perm, 6));
        EXPECT_FEQ(0.125,   fast_permuted_radical_inverse<double>(0, Perm, 7));
    }

    TEST_CASE(RadicalInverse_Base3)
    {
        EXPECT_FEQ(0.0,     radical_inverse<double>(3, 0));
        EXPECT_FEQ(1.0 / 3, radical_inverse<double>(3, 1));
        EXPECT_FEQ(2.0 / 3, radical_inverse<double>(3, 2));
        EXPECT_FEQ(1.0 / 9, radical_inverse<double>(3, 3));
        EXPECT_FEQ(4.0 / 9, radical_inverse<double>(3, 4));
        EXPECT_FEQ(7.0 / 9, radical_inverse<double>(3, 5));
        EXPECT_FEQ(2.0 / 9, radical_inverse<double>(3, 6));
        EXPECT_FEQ(5.0 / 9, radical_inverse<double>(3, 7));
    }

    TEST_CASE(PermutedRadicalInverse_Base3_IdentityPermutation)
    {
        static const size_t Perm[] = { 0, 1, 2 };
        EXPECT_FEQ(0.0,     permuted_radical_inverse<double>(3, Perm, 0));
        EXPECT_FEQ(1.0 / 3, permuted_radical_inverse<double>(3, Perm, 1));
        EXPECT_FEQ(2.0 / 3, permuted_radical_inverse<double>(3, Perm, 2));
        EXPECT_FEQ(1.0 / 9, permuted_radical_inverse<double>(3, Perm, 3));
        EXPECT_FEQ(4.0 / 9, permuted_radical_inverse<double>(3, Perm, 4));
        EXPECT_FEQ(7.0 / 9, permuted_radical_inverse<double>(3, Perm, 5));
        EXPECT_FEQ(2.0 / 9, permuted_radical_inverse<double>(3, Perm, 6));
        EXPECT_FEQ(5.0 / 9, permuted_radical_inverse<double>(3, Perm, 7));
    }

    TEST_CASE(PermutedRadicalInverse_Base3_ReversePermutation)
    {
        static const size_t Perm[] = { 0, 2, 1 };
        EXPECT_FEQ(0.0,     permuted_radical_inverse<double>(3, Perm, 0));
        EXPECT_FEQ(2.0 / 3, permuted_radical_inverse<double>(3, Perm, 1));
        EXPECT_FEQ(1.0 / 3, permuted_radical_inverse<double>(3, Perm, 2));
        EXPECT_FEQ(2.0 / 9, permuted_radical_inverse<double>(3, Perm, 3));
        EXPECT_FEQ(8.0 / 9, permuted_radical_inverse<double>(3, Perm, 4));
        EXPECT_FEQ(5.0 / 9, permuted_radical_inverse<double>(3, Perm, 5));
        EXPECT_FEQ(1.0 / 9, permuted_radical_inverse<double>(3, Perm, 6));
        EXPECT_FEQ(7.0 / 9, permuted_radical_inverse<double>(3, Perm, 7));
    }

    static const size_t PointCount = 256;

    TEST_CASE(Generate2DRandomSequenceImage)
    {
        vector<Vector2d> points;
        MersenneTwister rng;

        for (size_t i = 0; i < PointCount; ++i)
        {
            Vector2d p;
            p.x = rand_double2(rng);
            p.y = rand_double2(rng);
            points.push_back(p);
        }

        write_point_cloud_image("unit tests/outputs/test_qmc_random.png", points);
    }

    void apply_permutation(
        const string&   permutation,
        const size_t    size,
        size_t          perm[])
    {
        if (permutation == "identity")
            identity_permutation(size, perm);
        else if (permutation == "faure")
            faure_qmc_permutation(size, perm);
        else if (permutation == "reverse")
            reverse_qmc_permutation(size, perm);
        else assert(!"Invalid permutation method.");
    }

    void generate_halton_sequence_image(
        const size_t    b0,
        const size_t    b1,
        const string&   permutation,
        const size_t    initial_instance = 0)
    {
        const size_t bases[2] = { b0, b1 };

        size_t perms[100];
        apply_permutation(permutation, b0, perms);
        apply_permutation(permutation, b1, perms + b0);

        vector<Vector2d> points;

        for (size_t i = 0; i < PointCount; ++i)
            points.push_back(halton_sequence<double, 2>(bases, perms, initial_instance + i));

        const string filename =
              "unit tests/outputs/test_qmc_halton_" + permutation + "_permuted_"
            + to_string(b0) + "_" + to_string(b1) + "_"
            + (initial_instance > 0 ? to_string(initial_instance) : "")
            + ".png";

        write_point_cloud_image(filename, points);
    }

    TEST_CASE(Generate2DHaltonSequenceImages)
    {
        generate_halton_sequence_image(2, 3, "identity");
        generate_halton_sequence_image(5, 7, "identity");
        generate_halton_sequence_image(11, 13, "identity");
        generate_halton_sequence_image(17, 19, "identity");
        generate_halton_sequence_image(23, 29, "identity");
    }

    TEST_CASE(Generate2DFaureScrambledHaltonSequenceImages)
    {
        generate_halton_sequence_image(2, 3, "faure");
        generate_halton_sequence_image(5, 7, "faure");
        generate_halton_sequence_image(11, 13, "faure");
        generate_halton_sequence_image(17, 19, "faure");
        generate_halton_sequence_image(23, 29, "faure");

        generate_halton_sequence_image(5, 7, "faure", 5000);
    }

    TEST_CASE(Generate2DReverseScrambledHaltonSequenceImages)
    {
        generate_halton_sequence_image(2, 3, "reverse");
        generate_halton_sequence_image(5, 7, "reverse");
        generate_halton_sequence_image(11, 13, "reverse");
        generate_halton_sequence_image(17, 19, "reverse");
        generate_halton_sequence_image(23, 29, "reverse");
    }

    void generate_hammersley_sequence_image(
        const size_t    b,
        const string&   permutation)
    {
        const size_t bases[1] = { b };

        size_t perms[100];
        apply_permutation(permutation, b, perms);

        vector<Vector2d> points;

        for (size_t i = 0; i < PointCount; ++i)
            points.push_back(hammersley_sequence<double, 2>(bases, perms, PointCount, i));

        write_point_cloud_image(
            "unit tests/outputs/test_qmc_hammersley_" + permutation + "_permuted_" + to_string(b) + ".png",
            points);
    }

    TEST_CASE(Generate2DHammersleySequenceImages)
    {
        generate_hammersley_sequence_image(2, "identity");
        generate_hammersley_sequence_image(3, "identity");
        generate_hammersley_sequence_image(5, "identity");
        generate_hammersley_sequence_image(7, "identity");
        generate_hammersley_sequence_image(11, "identity");
    }

    TEST_CASE(Generate2DFaureScrambledHammersleySequenceImages)
    {
        generate_hammersley_sequence_image(3, "faure");
        generate_hammersley_sequence_image(5, "faure");
        generate_hammersley_sequence_image(7, "faure");
        generate_hammersley_sequence_image(11, "faure");
    }

    TEST_CASE(Generate2DReverseScrambledHammersleySequenceImages)
    {
        generate_hammersley_sequence_image(3, "reverse");
        generate_hammersley_sequence_image(5, "reverse");
        generate_hammersley_sequence_image(7, "reverse");
        generate_hammersley_sequence_image(11, "reverse");
    }

    void generate_hammersley_zaremba_sequence_image(
        const size_t    b)
    {
        const size_t bases[1] = { b };

        vector<Vector2d> points;

        for (size_t i = 0; i < PointCount; ++i)
            points.push_back(hammersley_zaremba_sequence<double, 2>(bases, PointCount, i));

        write_point_cloud_image(
            "unit tests/outputs/test_qmc_hammersley_zaremba_" + to_string(b) + ".png",
            points);
    }

    TEST_CASE(Generate2DHammersleyZarembaSequenceImages)
    {
        generate_hammersley_zaremba_sequence_image(2);
        generate_hammersley_zaremba_sequence_image(3);
        generate_hammersley_zaremba_sequence_image(5);
        generate_hammersley_zaremba_sequence_image(7);
        generate_hammersley_zaremba_sequence_image(11);
    }

    // 2D scrambled Hammersley sequence.
    template <typename T>
    inline Vector<T, 2> scrambled_hammersley_sequence(
        const size_t    r,
        size_t          count,
        size_t          n)
    {
        Vector<T, 2> p;
        p[0] = static_cast<T>(n) / count;
        p[1] = radical_inverse_base2<T>(n ^ r);
        return p;
    }

    TEST_CASE(Generate2DScrambledHammersleySequenceImage)
    {
        // This test illustrates what happens when each sample of the Hammersley set
        // is scrambled with a distinct random value: the resulting sample set is
        // indistinguishable from what would be obtained using pure random sampling.

        vector<Vector2d> points;
        MersenneTwister rng;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const size_t r = rand_int31(rng);
            points.push_back(scrambled_hammersley_sequence<double>(r, PointCount, i));
        }

        write_point_cloud_image(
            "unit tests/outputs/test_qmc_hammersley_2d_scrambled.png",
            points);
    }

    TEST_CASE(SampleImagePlaneWithHaltonSequence)
    {
        //
        // This test shows that the first W*H points of the Halton sequence in bases 2 and 3
        // don't cover all pixels of a WxH image uniformly: some pixels will contain 0 point
        // while others will contain several points.
        //

        const size_t Width = 640;
        const size_t Height = 480;
        const size_t PixelCount = Width * Height;

        Image image(Width, Height, 32, 32, 3, PixelFormatFloat);
        image.clear(Color3f(0.0f));

        for (size_t i = 0; i < PixelCount; ++i)
        {
            const size_t Bases[] = { 2, 3 };
            const Vector2d s = halton_sequence<double, 2>(Bases, i);

            const size_t x = truncate<size_t>(s[0] * Width);
            const size_t y = truncate<size_t>(s[1] * Height);

            Color3f c;
            image.get_pixel(x, y, c);

            c += Color3f(0.2f);
            c = saturate(c);

            image.set_pixel(x, y, c);
        }

        GenericImageFileWriter writer("unit tests/outputs/test_qmc_sampleimageplanewithhaltonsequence.png");
        writer.append_image(&image);
        writer.write();
    }

    TEST_CASE(SampleImagePlaneWithHaltonSequence_Uniformized)
    {
        //
        // This test builds on the observation that the first W*H points of the Halton sequence
        // in bases 2 and 3 cover all pixels of a WxH image uniformly as long as W is a power
        // of 2 and H is a power of 3.
        //

        const size_t Width = 640;
        const size_t Height = 480;
        const size_t PixelCount = Width * Height;

        const double NextWidth = next_power(static_cast<double>(Width), 2.0);
        const double NextHeight = next_power(static_cast<double>(Height), 3.0);

        Image image(Width, Height, 32, 32, 3, PixelFormatFloat);
        image.clear(Color3f(0.0f));

        for (size_t k = 0, n = 0; n < PixelCount * 10; ++k)
        {
            const size_t Bases[] = { 2, 3 };    // can't change these
            const Vector2d s = halton_sequence<double, 2>(Bases, k);

            const size_t x = truncate<size_t>(s[0] * NextWidth);
            const size_t y = truncate<size_t>(s[1] * NextHeight);

            if (x >= Width || y >= Height)
                continue;

            ++n;

            Color3f c;
            image.get_pixel(x, y, c);

            c += Color3f(0.02f);

            image.set_pixel(x, y, c);
        }

        GenericImageFileWriter writer("unit tests/outputs/test_qmc_sampleimageplanewithhaltonsequence_uniformized.png");
        writer.append_image(&image);
        writer.write();
    }

#ifdef APPLESEED_ARCH64

    TEST_CASE(SampleImagePlaneWithHaltonSequence_Uniformized_64BitOffset)
    {
        //
        // The same test as above, but this time the Halton sequence is offset by a value larger than 2^32.
        //

        const size_t Width = 640;
        const size_t Height = 480;
        const size_t PixelCount = Width * Height;

        const double NextWidth = next_power(static_cast<double>(Width), 2.0);
        const double NextHeight = next_power(static_cast<double>(Height), 3.0);
        const size_t StartOffset = 10000000000ULL;

        Image image(Width, Height, 32, 32, 3, PixelFormatFloat);
        image.clear(Color3f(0.0f));

        for (size_t k = StartOffset, n = StartOffset; n < StartOffset + PixelCount * 10; ++k)
        {
            const size_t Bases[] = { 2, 3 };    // can't change these
            const Vector2d s = halton_sequence<double, 2>(Bases, k);

            const size_t x = truncate<size_t>(s[0] * NextWidth);
            const size_t y = truncate<size_t>(s[1] * NextHeight);

            if (x >= Width || y >= Height)
                continue;

            ++n;

            Color3f c;
            image.get_pixel(x, y, c);

            c += Color3f(0.02f);

            image.set_pixel(x, y, c);
        }

        GenericImageFileWriter writer("unit tests/outputs/test_qmc_sampleimageplanewithhaltonsequence_uniformized_64bitoffset.png");
        writer.append_image(&image);
        writer.write();
    }

#endif

    TEST_CASE(Integrate1DFunction)
    {
        const double ExactArea = 2.0;
        const size_t SampleCount = 500;

        MersenneTwister rng;

        double rng_area = 0.0;
        double qmc_area = 0.0;

        vector<Vector2d> rng_rmsd(SampleCount);
        vector<Vector2d> qmc_rmsd(SampleCount);

        for (size_t i = 0; i < SampleCount; ++i)
        {
            rng_area += sin(rand_double2(rng) * Pi<double>());
            qmc_area += sin(radical_inverse_base2<double>(i) * Pi<double>());

            const double n = static_cast<double>(i + 1);
            const double v = Pi<double>() / n;

            rng_rmsd[i] = Vector2d(n, abs(rng_area * v - ExactArea));
            qmc_rmsd[i] = Vector2d(n, abs(qmc_area * v - ExactArea));
        }

        GnuplotFile plotfile;
        plotfile.set_title("RMS Deviation");
        plotfile.set_xlabel("Samples");
        plotfile
            .new_plot()
            .set_points(rng_rmsd)
            .set_title("RNG")
            .set_color("blue");
        plotfile
            .new_plot()
            .set_points(qmc_rmsd)
            .set_title("QMC")
            .set_color("red");
        plotfile.write("unit tests/outputs/test_qmc_integrate1dfunction.gnuplot");
    }

#if 0

    TEST_CASE(PrecomputeHaltonSequence)
    {
        const size_t Dimension = 4;
        const size_t SampleCount = 256;

        FILE* file = fopen("unit tests/outputs/test_qmc_precomputed_halton.txt", "wt");
        ASSERT_NEQ(0, file);

        fprintf(file, "static const double HaltonSequence4d[] =\n");
        fprintf(file, "{\n");

        for (size_t i = 0; i < SampleCount; ++i)
        {
            fprintf(file, "    ");

            for (size_t d = 0; d < Dimension; ++d)
            {
                const double s = radical_inverse<double>(Primes[d], i);

                fprintf(file, "%.16f", s);

                if (d < Dimension - 1)
                    fprintf(file, ", ");
                else if (i < SampleCount - 1)
                    fprintf(file, ",");
            }

            fprintf(file, "\n");
        }

        fprintf(file, "};\n");

        fclose(file);
    }

#endif
}
