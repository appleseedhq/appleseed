
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2019 Francois Beaune, The appleseedhq Organization
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
#include "foundation/hash/hash.h"
#include "foundation/image/color.h"
#include "foundation/image/colormap.h"
#include "foundation/image/colormapdata.h"
#include "foundation/image/conversion.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/rng/xoroshiro128plus.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/string/string.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Hash_Hash)
{
    struct HistogramFixture
    {
        static constexpr size_t BinCount = 256;             // best when a power of two
        static constexpr size_t SampleCount = 1024 * 1024;  // best when a multiple of BinCount

        size_t m_bins[BinCount];

        HistogramFixture()
        {
            for (size_t i = 0; i < BinCount; ++i)
                m_bins[i] = 0;
        }

        void insert_value(const double x)
        {
            assert(x >= 0.0 && x < 1.0);

            const size_t i = truncate<size_t>(x * BinCount);
            assert(i < BinCount);

            ++m_bins[i];
        }

        //
        // Compute a kind of normalized Pearson's chi-squared test.
        //
        // References:
        //
        //   https://en.wikipedia.org/wiki/Pearson%27s_chi-squared_test
        //   https://en.wikipedia.org/wiki/Index_of_dispersion
        //

        double compute_dispersion() const
        {
            constexpr double ExpectedCount = SampleCount / BinCount;

            double variance = 0.0;
            for (size_t i = 0; i < BinCount; ++i)
                variance += square(m_bins[i] - ExpectedCount);
            variance /= BinCount;

            return variance / ExpectedCount;
        }

        void plot_histogram(const char* filename, const char* title) const
        {
            std::vector<Vector2d> points;
            points.reserve(BinCount);

            for (size_t i = 0; i < BinCount; ++i)
                points.emplace_back(static_cast<double>(i), static_cast<double>(m_bins[i]));

            const double dispersion = compute_dispersion();

            GnuplotFile gnuplot_file;
            gnuplot_file.set_title(format("{0} - Dispersion: {1}", title, dispersion));
            gnuplot_file.set_xlabel("Bin");
            gnuplot_file.set_ylabel("Number of values");
            gnuplot_file.set_xrange(0.0, BinCount - 1);
            gnuplot_file.new_plot().set_points(points);
            gnuplot_file.write(format("unit tests/outputs/{0}", filename));
        }
    };

    TEST_CASE_F(NoHashing_Histogram_Sequential, HistogramFixture)
    {
        Xoroshiro128plus rng;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const double x = static_cast<double>(i) / SampleCount;
            insert_value(x);
        }

        plot_histogram(
            "test_hash_01_nohashing_histogram_sequential.gnuplot",
            "No Hashing - Sequential Numbers");
    }

    TEST_CASE_F(NoHashing_Histogram_Random, HistogramFixture)
    {
        Xoroshiro128plus rng;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const double x = rng.rand_uint32() * Rcp2Pow32<double>();
            insert_value(x);
        }

        plot_histogram(
            "test_hash_02_nohashing_histogram_random.gnuplot",
            "No Hashing - Random Numbers");
    }

    TEST_CASE_F(HashUInt32_Histogram_Sequential, HistogramFixture)
    {
        for (std::uint32_t i = 0; i < SampleCount; ++i)
        {
            const double x = hash_uint32(i) * Rcp2Pow32<double>();
            insert_value(x);
        }

        plot_histogram(
            "test_hash_03_hashuint32_histogram_sequential.gnuplot",
            "foundation::hash_uint32() - Sequential Numbers");
    }

    TEST_CASE_F(HashUInt32_Histogram_Random, HistogramFixture)
    {
        Xoroshiro128plus rng;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const double x = hash_uint32(rng.rand_uint32()) * Rcp2Pow32<double>();
            insert_value(x);
        }

        plot_histogram(
            "test_hash_04_hashuint32_histogram_random.gnuplot",
            "foundation::hash_uint32() - Random Numbers");
    }

    TEST_CASE_F(HashUInt32Wang_Histogram_Sequential, HistogramFixture)
    {
        for (std::uint32_t i = 0; i < SampleCount; ++i)
        {
            const double x = hash_uint32_wang(i) * Rcp2Pow32<double>();
            insert_value(x);
        }

        plot_histogram(
            "test_hash_05_hashuint32wang_histogram_sequential.gnuplot",
            "foundation::hash_uint32_wang() - Sequential Numbers");
    }

    TEST_CASE_F(HashUInt32Wang_Histogram_Random, HistogramFixture)
    {
        Xoroshiro128plus rng;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const double x = hash_uint32_wang(rng.rand_uint32()) * Rcp2Pow32<double>();
            insert_value(x);
        }

        plot_histogram(
            "test_hash_06_hashuint32wang_histogram_random.gnuplot",
            "foundation::hash_uint32_wang() - Random Numbers");
    }

    TEST_CASE_F(MixUInt32_Histogram, HistogramFixture)
    {
        for (std::uint32_t i = 0; i < SampleCount / 10; ++i)
        {
            for (std::uint32_t j = 0; j < 10; ++j)
            {
                const double x = mix_uint32(i, j) * Rcp2Pow32<double>();
                insert_value(x);
            }
        }

        plot_histogram(
            "test_hash_07_mixuint32_histogram.gnuplot",
            "foundation::mix_uint32()");
    }

    template <typename UInt> UInt rand(Xoroshiro128plus& rng);
    template <> std::uint32_t rand(Xoroshiro128plus& rng)
    {
        return rng.rand_uint32();
    }
    template <> std::uint64_t rand(Xoroshiro128plus& rng)
    {
        std::uint64_t x = rng.rand_uint32();
        x <<= 32;
        x |= rng.rand_uint32();
        return x;
    }

    struct AvalancheFixture
    {
        static const size_t Trials = 100000;

        template <typename UInt, typename Hash>
        void plot_avalanche(
            const char*     filename,
            const size_t    trials,
            Hash            hash)
        {
            constexpr size_t N = std::numeric_limits<UInt>::digits;

            // y = input bit, x = output bit
            size_t counters[N * N];

            // Initialize counters to zero.
            for (size_t i = 0; i < N * N; ++i)
                counters[i] = 0;

            Xoroshiro128plus rng;

            for (size_t trial = 0; trial < trials; ++trial)
            {
                // Generate an initial value.
                const UInt initial_value = rand<UInt>(rng);

                // Hash the initial value.
                const UInt initial_hash = hash(initial_value);

                // Iterate over all bits of a word.
                for (size_t y = 0; y < N; ++y)
                {
                    // Flip the i'th bit of the initial value.
                    const UInt new_value = initial_value ^ (UInt(1) << y);

                    // Hash the new value.
                    const UInt new_hash = hash(new_value);

                    // Compute the difference between the two hashes.
                    const UInt hash_diff = initial_hash ^ new_hash;

                    // Update flipped bits counters.
                    for (size_t x = 0; x < N; ++x)
                        counters[y * N + x] += (hash_diff & (UInt(1) << x)) >> x;
                }
            }

            // Construct a scaled, transposed image from the counters.
            constexpr size_t Resolution = 512;
            constexpr size_t Scaling = Resolution / N;
            Image image(Resolution, Resolution, Resolution, Resolution, 3, PixelFormatFloat);
            for (size_t y = 0; y < Resolution; ++y)
            {
                for (size_t x = 0; x < Resolution; ++x)
                {
                    const size_t counter = counters[(y / Scaling) * N + (x / Scaling)];
                    image.set_pixel(y, x, Color3f(static_cast<float>(counter)));
                }
            }

            // Construct a color map.
            ColorMap color_map;
            color_map.set_palette_from_array(InfernoColorMapLinearRGB, countof(InfernoColorMapLinearRGB) / 3);

            // Color map the image.
            float min_value, max_value;
            color_map.find_min_max_red_channel(image, min_value, max_value);
            color_map.remap_red_channel(image, 0.0f, max_value);    // ignore min value

            // Convert the image to sRGB.
            convert_linear_rgb_to_srgb(image);

            // Write the image to disk.
            GenericImageFileWriter writer(format("unit tests/outputs/{0}", filename).c_str());
            writer.append_image(&image);
            writer.write();
        }
    };

    TEST_CASE_F(Identity_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_08_identity_avalanche.png",
            Trials,
            [](const std::uint32_t value) { return value; });
    }

    TEST_CASE_F(Addition_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_09_addition_avalanche.png",
            Trials,
            [](const std::uint32_t value) { return value + 0xDEADBEEFul; });
    }

    TEST_CASE_F(PrimeMultiplication_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_10_primemultiplication_avalanche.png",
            Trials,
            [](const std::uint32_t value) { return value * 1536399377ul; });
    }

    TEST_CASE_F(HashUInt32_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_11_hashuint32_avalanche.png",
            Trials,
            hash_uint32);
    }

    TEST_CASE_F(HashUInt32Wang_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_12_hashuint32wang_avalanche.png",
            Trials,
            [](const std::uint32_t value) { return hash_uint32_wang(value); });
    }

    TEST_CASE_F(BobJenkinsHalf_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_13_bobjenkinshalf_avalanche.png",
            Trials,
            [](std::uint32_t value)
            {
                value = (value + 0x479ab41d) + (value << 8);
                value = (value ^ 0xe4aa10ce) ^ (value >> 5);
                value = (value + 0x9942f0a6) - (value << 14);
                value = (value ^ 0x5aedd67d) ^ (value >> 3);
                value = (value + 0x17bea992) + (value << 7);
                return value;
            });
    }

    TEST_CASE_F(BobJenkinsFull_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_14_bobjenkinsfull_avalanche.png",
            Trials,
            [](std::uint32_t value)
            {
                value = (value + 0x7ed55d16) + (value << 12);
                value = (value ^ 0xc761c23c) ^ (value >> 19);
                value = (value + 0x165667b1) + (value << 5);
                value = (value + 0xd3a2646c) ^ (value << 9);
                value = (value + 0xfd7046c5) + (value << 3);
                value = (value ^ 0xb55a4f09) ^ (value >> 16);
                return value;
            });
    }

    TEST_CASE_F(BobJenkinsFullNoBigConstants_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_15_bobjenkinsfullnobigconstants_avalanche.png",
            Trials,
            [](std::uint32_t value)
            {
                value -= value << 6;
                value ^= value >> 17;
                value -= value << 9;
                value ^= value << 4;
                value -= value << 3;
                value ^= value << 10;
                value ^= value >> 15;
                return value;
            });
    }

    TEST_CASE_F(HashUInt64ToUInt32_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_16_hashuint64touint32_avalanche.png",
            Trials,
            hash_uint64_to_uint32);
    }

    TEST_CASE_F(MixUInt32_Avalanche, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_17_mixuint32_avalanche.png",
            Trials,
            [](const std::uint32_t value) { return mix_uint32(value, 0xDEADBEEFul); });
    }

    static std::uint32_t boost_hash_combine(const std::uint32_t h1, const std::uint32_t h2)
    {
        // See foundation::combine_hashes() in foundation/hash/hash.h for the constant derivation.
        return h1 ^ (h2 + 0x9E3779B9ul + (h1 << 6) + (h1 >> 2));
    }

    TEST_CASE_F(BoostHashCombine_Avalanche_FirstValue, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_18_boosthashcombine_firstvalue_avalanche.png",
            Trials,
            [](const std::uint32_t value)
            {
                const std::uint32_t h1 = hash_uint32(value);
                const std::uint32_t h2 = 0xDEADBEEFul;
                return boost_hash_combine(h1, h2);
            });
    }

    TEST_CASE_F(BoostHashCombine_Avalanche_SecondValue, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_19_boosthashcombine_secondvalue_avalanche.png",
            Trials,
            [](const std::uint32_t value)
            {
                const std::uint32_t h1 = 0xDEADBEEFul;
                const std::uint32_t h2 = hash_uint32(value);
                return boost_hash_combine(h1, h2);
            });
    }

    TEST_CASE_F(CombineHashes_32_Avalanche_FirstValue, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_20_combinehashes_32_firstvalue_avalanche.png",
            Trials,
            [](const std::uint32_t value)
            {
                const std::uint32_t h1 = hash_uint32(value);
                const std::uint32_t h2 = 0xDEADBEEFul;
                return combine_hashes(h1, h2);
            });
    }

    TEST_CASE_F(CombineHashes_32_Avalanche_SecondValue, AvalancheFixture)
    {
        plot_avalanche<std::uint32_t>(
            "test_hash_21_combinehashes_32_secondvalue_avalanche.png",
            Trials,
            [](const std::uint32_t value)
            {
                const std::uint32_t h1 = 0xDEADBEEFul;
                const std::uint32_t h2 = hash_uint32(value);
                return combine_hashes(h1, h2);
            });
    }

    TEST_CASE_F(CombineHashes_64_Avalanche_FirstValue, AvalancheFixture)
    {
        plot_avalanche<std::uint64_t>(
            "test_hash_22_combinehashes_64_firstvalue_avalanche.png",
            Trials,
            [](const std::uint64_t value)
            {
                const std::uint64_t h1 = hash_uint64(value);
                const std::uint64_t h2 = 0xDEADBEEFCAFEBABEul;
                return combine_hashes(h1, h2);
            });
    }

    TEST_CASE_F(CombineHashes_64_Avalanche_SecondValue, AvalancheFixture)
    {
        plot_avalanche<std::uint64_t>(
            "test_hash_23_combinehashes_64_secondvalue_avalanche.png",
            Trials,
            [](const std::uint64_t value)
            {
                const std::uint64_t h1 = 0xDEADBEEFCAFEBABEul;
                const std::uint64_t h2 = hash_uint64(value);
                return combine_hashes(h1, h2);
            });
    }
}
