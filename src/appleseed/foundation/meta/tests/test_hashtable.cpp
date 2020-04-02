
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/containers/hashtable.h"
#include "foundation/hash/hash.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/xoroshiro128plus.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

using namespace foundation;

TEST_SUITE(Foundation_Containers_HashTable)
{
    struct KeyHasher
    {
        size_t operator()(const size_t key) const
        {
            return
                static_cast<size_t>(
                    hash_uint64(
                        static_cast<std::uint64_t>(key)));
        }
    };

    TEST_CASE(Get_TableIsEmpty_ReturnsNullptr)
    {
        KeyHasher key_hasher;
        HashTable<int, KeyHasher, float> hash_table(key_hasher);

        const float* val_ptr = hash_table.get(12);

        EXPECT_EQ(0, val_ptr);
    }

    TEST_CASE(StressTest)
    {
        const size_t N = 16 * 1024;

        KeyHasher key_hasher;
        HashTable<size_t, KeyHasher, float> hash_table(key_hasher);

        hash_table.resize(N);

        {
            Xoroshiro128plus rng;

            for (size_t i = 0; i < N; ++i)
                hash_table.insert(i, static_cast<float>(2 * i));
        }

        {
            Xoroshiro128plus rng;

            for (size_t i = 0; i < N; ++i)
            {
                const float* val_ptr = hash_table.get(i);

                ASSERT_NEQ(0, val_ptr);
                EXPECT_EQ(static_cast<float>(2 * i), *val_ptr);
            }
        }
    }
}
