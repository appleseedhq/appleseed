
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
#include <cstddef>

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
        void load(const MyKey key, MyElement*& element) const
        {
            element = new MyElement(key * 10);
        }

        void unload(const MyKey key, MyElement*& element) const
        {
            delete element;
            element = 0;
        }

        bool is_full(
            const size_t    element_count,
            const size_t    memory_size) const
        {
            return element_count >= 4;
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
                    throw foundation::Exception("Cache integrity check failed");
            }
        }
    };
}

TEST_SUITE(Foundation_Utility_Cache_DualStageCache)
{
    using namespace foundation;

    TEST_CASE(StressTest)
    {
        MyKeyHasher key_hasher;
        MyElementSwapper element_swapper;

        DualStageCache<
            MyKey,
            MyKeyHasher,
            MyElement*,
            MyElementSwapper,
            128,
            1> cache(key_hasher, element_swapper, MyInvalidKey);

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
