
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
#include "foundation/core/exceptions/exception.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/lcg.h"
#include "foundation/utility/cache.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;

namespace
{
    typedef size_t Key;
    typedef size_t Element;

    const Key InvalidKey = ~Key(0);

    struct KeyHasher
    {
        size_t operator()(const Key key) const
        {
            return static_cast<size_t>(key);
        }
    };

    struct ElementSwapperCountingUnloads
    {
        size_t m_unload_count;

        ElementSwapperCountingUnloads()
          : m_unload_count(0)
        {
        }

        void load(const Key key, Element& element) const
        {
        }

        bool unload(const Key key, Element& element)
        {
            ++m_unload_count;
            return true;
        }

        bool is_full(const size_t element_count) const
        {
            return false;
        }
    };
}

TEST_SUITE(Foundation_Utility_Cache_SACache)
{
    TEST_CASE(Destructor_UnloadsElementsStillInCache)
    {
        ElementSwapperCountingUnloads element_swapper;

        {
            KeyHasher key_hasher;
            SACache<Key, KeyHasher, Element, ElementSwapperCountingUnloads, 4, 1> cache(
                key_hasher,
                element_swapper,
                InvalidKey);

            cache.get(1);
            cache.get(2);
            cache.get(3);
        }

        EXPECT_EQ(3, element_swapper.m_unload_count);
    }

    TEST_CASE(Get_DoesNotCallUnloadOnEmptyCacheLine)
    {
        KeyHasher key_hasher;
        ElementSwapperCountingUnloads element_swapper;
        SACache<Key, KeyHasher, Element, ElementSwapperCountingUnloads, 4, 1> cache(
            key_hasher,
            element_swapper,
            InvalidKey);

        cache.get(0);

        EXPECT_EQ(0, element_swapper.m_unload_count);
    }
}

TEST_SUITE(Foundation_Utility_Cache_LRUCache)
{
    TEST_CASE(Destructor_UnloadsElementsStillInCache)
    {
        ElementSwapperCountingUnloads element_swapper;

        {
            KeyHasher key_hasher;
            LRUCache<Key, KeyHasher, Element, ElementSwapperCountingUnloads> cache(key_hasher, element_swapper);

            cache.get(1);
            cache.get(2);
            cache.get(3);
        }

        EXPECT_EQ(3, element_swapper.m_unload_count);
    }

    struct ElementSwapperTrackingSize
    {
        size_t m_memory_size;

        ElementSwapperTrackingSize()
          : m_memory_size(0)
        {
        }

        void load(const Key key, Element& element)
        {
            m_memory_size += key * 1000;
        }

        bool unload(const Key key, Element& element)
        {
            m_memory_size -= key * 1000;
            return true;
        }

        bool is_full(const size_t element_count) const
        {
            return m_memory_size >= 8 * 1000;
        }
    };

    TEST_CASE(MemoryLimitIsHonored)
    {
        KeyHasher key_hasher;
        ElementSwapperTrackingSize element_swapper;
        LRUCache<Key, KeyHasher, Element, ElementSwapperTrackingSize> cache(key_hasher, element_swapper);

        cache.get(1);
        ASSERT_EQ(1000, element_swapper.m_memory_size);

        cache.get(2);
        ASSERT_EQ(3000, element_swapper.m_memory_size);

        cache.get(3);
        ASSERT_EQ(6000, element_swapper.m_memory_size);

        cache.get(4);   // flushes 1 and 2, cache contains 3 and 4
        ASSERT_EQ(7000, element_swapper.m_memory_size);

        cache.get(5);   // flushes 3 and 4, cache contains 5
        ASSERT_EQ(5000, element_swapper.m_memory_size);

        cache.get(6);   // flushes 5, cache contains 6
        ASSERT_EQ(6000, element_swapper.m_memory_size);

        cache.get(9);   // flushes 6, cache contains 9
        ASSERT_EQ(9000, element_swapper.m_memory_size);
    }
}

TEST_SUITE(Foundation_Utility_Cache_DualStageCache)
{
    struct ElementSwapper
    {
        const size_t m_cache_size;

        explicit ElementSwapper(const size_t cache_size)
          : m_cache_size(cache_size)
        {
        }

        void load(const Key key, Element& element) const
        {
            element = static_cast<Element>(key * 10);
        }

        bool unload(const Key key, Element& element) const
        {
            element = 0;
            return true;
        }

        bool is_full(const size_t element_count) const
        {
            assert(element_count <= m_cache_size);
            return element_count == m_cache_size;
        }
    };

    struct IntegrityChecker
    {
        void operator()(const Key key, const Element element) const
        {
            if (key != InvalidKey)
            {
                const Element expected = 10 * key;

                if (expected != element)
                    throw Exception("cache integrity check failed");
            }
        }
    };

    TEST_CASE(StressTest)
    {
        const size_t Stage0LineCount = 128;
        const size_t Stage0WayCount = 1;
        const size_t Stage1LineCount = 4;

        KeyHasher key_hasher;
        ElementSwapper element_swapper(Stage1LineCount);

        DualStageCache<
            Key,
            KeyHasher,
            Element,
            ElementSwapper,
            Stage0LineCount,
            Stage0WayCount> cache(key_hasher, element_swapper, InvalidKey);

        IntegrityChecker checker;
        cache.check_integrity(checker);

        LCG rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const Key key = rand_int1(rng, 1, 1000);
            const Element expected = key * 10;

            const Element value = cache.get(key);
            EXPECT_EQ(expected, value);

            cache.check_integrity(checker);
        }
    }
}
