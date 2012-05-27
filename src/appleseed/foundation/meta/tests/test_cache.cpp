
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

TEST_SUITE(Foundation_Utility_Cache_SACache)
{
    typedef size_t MyKey;
    typedef size_t MyElement;

    const MyKey MyInvalidKey = ~0;

    struct MyKeyHasher
    {
        size_t operator()(const MyKey key) const
        {
            return static_cast<size_t>(key);
        }
    };

    struct MyElementSwapper
    {
        bool& m_unload_called;

        explicit MyElementSwapper(bool& unload_called)
          : m_unload_called(unload_called)
        {
        }

        void load(const MyKey key, MyElement& element) const
        {
        }

        void unload(const MyKey key, MyElement& element) const
        {
            m_unload_called = true;
        }
    };

    TEST_CASE(Get_DoesNotCallUnloadOnEmptyCacheLine)
    {
        MyKeyHasher key_hasher;

        bool unload_called = false;
        MyElementSwapper element_swapper(unload_called);

        SACache<MyKey, MyKeyHasher, MyElement, MyElementSwapper, 4, 1> cache(
            key_hasher,
            element_swapper,
            MyInvalidKey);

        cache.get(0);

        EXPECT_FALSE(unload_called);
    }
}

TEST_SUITE(Foundation_Utility_Cache_LRUCache)
{
    namespace case1
    {
        typedef size_t MyKey;
        typedef size_t MyElement;

        struct MyElementSwapper
        {
            size_t m_unloaded_element_count;

            MyElementSwapper()
              : m_unloaded_element_count(0)
            {
            }

            void load(const MyKey key, MyElement*& element) const
            {
                element = new MyElement(key);
            }

            void unload(const MyKey key, MyElement*& element)
            {
                delete element;
                element = 0;
                ++m_unloaded_element_count;
            }

            bool is_full(const size_t element_count) const
            {
                return false;
            }
        };

        TEST_CASE(Destructor_UnloadsElementsStillInCache)
        {
            MyElementSwapper element_swapper;

            {
                LRUCache<MyKey, MyElement*, MyElementSwapper> cache(element_swapper);

                cache.get(1);
                cache.get(2);
                cache.get(3);
            }

            EXPECT_EQ(3, element_swapper.m_unloaded_element_count);
        }
    }

    namespace case2
    {
        typedef size_t MyKey;
        typedef size_t MyElement;

        struct MyElementSwapper
        {
            size_t m_memory_size;

            MyElementSwapper()
              : m_memory_size(0)
            {
            }

            void load(const MyKey key, MyElement*& element)
            {
                element = new MyElement(key);
                m_memory_size += *element * 1000;
            }

            void unload(const MyKey key, MyElement*& element)
            {
                m_memory_size -= *element * 1000;
                delete element;
                element = 0;
            }

            bool is_full(const size_t element_count) const
            {
                return m_memory_size >= 8 * 1000;
            }
        };

        TEST_CASE(MemoryLimitIsHonored)
        {
            MyElementSwapper element_swapper;
            LRUCache<MyKey, MyElement*, MyElementSwapper> cache(element_swapper);

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
}

TEST_SUITE(Foundation_Utility_Cache_DualStageCache)
{
    typedef size_t MyKey;
    typedef size_t MyElement;

    const MyKey MyInvalidKey = ~0;

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
                    throw Exception("cache integrity check failed");
            }
        }
    };

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
