
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

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/math/rng.h"
#include "foundation/utility/cache.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace
{
    typedef size_t MyKey;
    typedef int MyElement;

    const MyKey MyInvalidKey = ~MyKey(0);

    struct MyKeyHasher
    {
        size_t operator()(const MyKey key) const
        {
            return static_cast<size_t>(key);
        }
    };

    struct MyElementSwapper
    {
        const size_t m_cache_size;

        explicit MyElementSwapper(const size_t cache_size)
          : m_cache_size(cache_size)
        {
        }

        void load(const MyKey key, MyElement*& element) const
        {
            element = new MyElement(key * 10);
        }

        void unload(const MyKey key, MyElement*& element) const
        {
            delete element;
            element = 0;
        }

        bool is_full(const size_t element_count) const
        {
            assert(element_count <= m_cache_size);
            return element_count == m_cache_size;
        }
    };

    struct MyIntegrityChecker
    {
        void operator()(const MyKey key, const MyElement* const & element) const
        {
            if (key != MyInvalidKey)
            {
                const MyElement expected = 10 * key;

                if (expected != *element)
                    throw Exception("Cache integrity check failed");
            }
        }
    };
}

TEST_SUITE(Foundation_Utility_Cache_DualStageCache)
{
    TEST_CASE(StressTest)
    {
        const size_t Stage0LineCount = 128;
        const size_t Stage0WayCount = 1;
        const size_t Stage1LineCount = 4;

        MyKeyHasher key_hasher;
        MyElementSwapper element_swapper(Stage1LineCount);

        DualStageCache<
            MyKey,
            MyKeyHasher,
            MyElement*,
            MyElementSwapper,
            Stage0LineCount,
            Stage0WayCount> cache(key_hasher, element_swapper, MyInvalidKey);

        MyIntegrityChecker checker;

        cache.check_integrity(checker);

        LCG rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const MyKey key = rand_int1(rng, 0, 1000);
            const MyElement expected = key * 10;

            const MyElement* value = cache.get(key);
            EXPECT_EQ(expected, *value);

            cache.check_integrity(checker);
        }
    }
}
