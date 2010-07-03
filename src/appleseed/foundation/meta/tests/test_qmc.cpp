
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/drawing.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/math/permutation.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/vector.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

FOUNDATION_TEST_SUITE(Foundation_Math_QMC)
{
    using namespace foundation;
    using namespace std;

    FOUNDATION_TEST_CASE(TestFastRadicalInverseBase2)
    {
        FOUNDATION_EXPECT_FEQ(0.0,     radical_inverse_base2<double>(0));
        FOUNDATION_EXPECT_FEQ(0.5,     radical_inverse_base2<double>(1));
        FOUNDATION_EXPECT_FEQ(0.25,    radical_inverse_base2<double>(2));
        FOUNDATION_EXPECT_FEQ(0.75,    radical_inverse_base2<double>(3));
        FOUNDATION_EXPECT_FEQ(0.125,   radical_inverse_base2<double>(4));
        FOUNDATION_EXPECT_FEQ(0.625,   radical_inverse_base2<double>(5));
        FOUNDATION_EXPECT_FEQ(0.375,   radical_inverse_base2<double>(6));
        FOUNDATION_EXPECT_FEQ(0.875,   radical_inverse_base2<double>(7));
    }

    FOUNDATION_TEST_CASE(TestRadicalInverseInBase2)
    {
        FOUNDATION_EXPECT_FEQ(0.0,     radical_inverse<double>(2, 0));
        FOUNDATION_EXPECT_FEQ(0.5,     radical_inverse<double>(2, 1));
        FOUNDATION_EXPECT_FEQ(0.25,    radical_inverse<double>(2, 2));
        FOUNDATION_EXPECT_FEQ(0.75,    radical_inverse<double>(2, 3));
        FOUNDATION_EXPECT_FEQ(0.125,   radical_inverse<double>(2, 4));
        FOUNDATION_EXPECT_FEQ(0.625,   radical_inverse<double>(2, 5));
        FOUNDATION_EXPECT_FEQ(0.375,   radical_inverse<double>(2, 6));
        FOUNDATION_EXPECT_FEQ(0.875,   radical_inverse<double>(2, 7));
    }

    FOUNDATION_TEST_CASE(TestRadicalInverseInBase3)
    {
        FOUNDATION_EXPECT_FEQ(0.0,     radical_inverse<double>(3, 0));
        FOUNDATION_EXPECT_FEQ(1.0 / 3, radical_inverse<double>(3, 1));
        FOUNDATION_EXPECT_FEQ(2.0 / 3, radical_inverse<double>(3, 2));
        FOUNDATION_EXPECT_FEQ(1.0 / 9, radical_inverse<double>(3, 3));
        FOUNDATION_EXPECT_FEQ(4.0 / 9, radical_inverse<double>(3, 4));
        FOUNDATION_EXPECT_FEQ(7.0 / 9, radical_inverse<double>(3, 5));
        FOUNDATION_EXPECT_FEQ(2.0 / 9, radical_inverse<double>(3, 6));
        FOUNDATION_EXPECT_FEQ(5.0 / 9, radical_inverse<double>(3, 7));
    }

    FOUNDATION_TEST_CASE(TestFastPermutedRadicalInverseBase3IdentityPermutation)
    {
        static const size_t Perm[] = { 0, 1, 2 };
        FOUNDATION_EXPECT_FEQ(0.0,     permuted_radical_inverse<double>(3, Perm, 0));
        FOUNDATION_EXPECT_FEQ(1.0 / 3, permuted_radical_inverse<double>(3, Perm, 1));
        FOUNDATION_EXPECT_FEQ(2.0 / 3, permuted_radical_inverse<double>(3, Perm, 2));
        FOUNDATION_EXPECT_FEQ(1.0 / 9, permuted_radical_inverse<double>(3, Perm, 3));
        FOUNDATION_EXPECT_FEQ(4.0 / 9, permuted_radical_inverse<double>(3, Perm, 4));
        FOUNDATION_EXPECT_FEQ(7.0 / 9, permuted_radical_inverse<double>(3, Perm, 5));
        FOUNDATION_EXPECT_FEQ(2.0 / 9, permuted_radical_inverse<double>(3, Perm, 6));
        FOUNDATION_EXPECT_FEQ(5.0 / 9, permuted_radical_inverse<double>(3, Perm, 7));
    }

    FOUNDATION_TEST_CASE(TestFastPermutedRadicalInverseBase3ReversePermutation)
    {
        static const size_t Perm[] = { 0, 2, 1 };
        FOUNDATION_EXPECT_FEQ(0.0,     permuted_radical_inverse<double>(3, Perm, 0));
        FOUNDATION_EXPECT_FEQ(2.0 / 3, permuted_radical_inverse<double>(3, Perm, 1));
        FOUNDATION_EXPECT_FEQ(1.0 / 3, permuted_radical_inverse<double>(3, Perm, 2));
        FOUNDATION_EXPECT_FEQ(2.0 / 9, permuted_radical_inverse<double>(3, Perm, 3));
        FOUNDATION_EXPECT_FEQ(8.0 / 9, permuted_radical_inverse<double>(3, Perm, 4));
        FOUNDATION_EXPECT_FEQ(5.0 / 9, permuted_radical_inverse<double>(3, Perm, 5));
        FOUNDATION_EXPECT_FEQ(1.0 / 9, permuted_radical_inverse<double>(3, Perm, 6));
        FOUNDATION_EXPECT_FEQ(7.0 / 9, permuted_radical_inverse<double>(3, Perm, 7));
    }

    void generate_image(
        const string&               image_filename,
        const vector<Vector2d>&     points)
    {
        Image image(
            512, 512,
            32, 32,
            3,
            PixelFormatFloat);

        image.clear(Color3f(0.0f));

        for (size_t i = 0; i < points.size(); ++i)
            Drawing::draw_dot(image, points[i], Color3f(1.0f));

        GenericImageFileWriter().write(image_filename, image);
    }

    static const size_t PointCount = 256;

    FOUNDATION_TEST_CASE(Generate2DRandomSequenceImage)
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

        generate_image("output/test_qmc_random.png", points);
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

        string filename =
              "output/test_qmc_halton_" + permutation + "_scrambled_"
            + to_string(b0) + "_" + to_string(b1) + "_"
            + (initial_instance > 0 ? to_string(initial_instance) : "")
            + ".png";

        generate_image(filename, points);
    }

    FOUNDATION_TEST_CASE(Generate2DHaltonSequenceImages)
    {
        generate_halton_sequence_image(2, 3, "identity");
        generate_halton_sequence_image(5, 7, "identity");
        generate_halton_sequence_image(11, 13, "identity");
        generate_halton_sequence_image(17, 19, "identity");
        generate_halton_sequence_image(23, 29, "identity");
    }

    FOUNDATION_TEST_CASE(Generate2DFaureScrambledHaltonSequenceImages)
    {
        generate_halton_sequence_image(2, 3, "faure");
        generate_halton_sequence_image(5, 7, "faure");
        generate_halton_sequence_image(11, 13, "faure");
        generate_halton_sequence_image(17, 19, "faure");
        generate_halton_sequence_image(23, 29, "faure");

        generate_halton_sequence_image(5, 7, "faure", 5000);
    }

    FOUNDATION_TEST_CASE(Generate2DReverseScrambledHaltonSequenceImages)
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
            points.push_back(hammersley_sequence<double, 2>(bases, perms, i, PointCount));

        generate_image(
            "output/test_qmc_hammersley_" + permutation + "_scrambled_" + to_string(b) + ".png",
            points);
    }

    FOUNDATION_TEST_CASE(Generate2DHammersleySequenceImages)
    {
        generate_hammersley_sequence_image(2, "identity");
        generate_hammersley_sequence_image(3, "identity");
        generate_hammersley_sequence_image(5, "identity");
        generate_hammersley_sequence_image(7, "identity");
        generate_hammersley_sequence_image(11, "identity");
    }

    FOUNDATION_TEST_CASE(Generate2DFaureScrambledHammersleySequenceImages)
    {
        generate_hammersley_sequence_image(3, "faure");
        generate_hammersley_sequence_image(5, "faure");
        generate_hammersley_sequence_image(7, "faure");
        generate_hammersley_sequence_image(11, "faure");
    }

    FOUNDATION_TEST_CASE(Generate2DReverseScrambledHammersleySequenceImages)
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
            points.push_back(hammersley_zaremba_sequence<double, 2>(bases, i, PointCount));

        generate_image(
            "output/test_qmc_hammersley_zaremba_" + to_string(b) + ".png",
            points);
    }

    FOUNDATION_TEST_CASE(Generate2DHammersleyZarembaSequenceImages)
    {
        generate_hammersley_zaremba_sequence_image(2);
        generate_hammersley_zaremba_sequence_image(3);
        generate_hammersley_zaremba_sequence_image(5);
        generate_hammersley_zaremba_sequence_image(7);
        generate_hammersley_zaremba_sequence_image(11);
    }
}
