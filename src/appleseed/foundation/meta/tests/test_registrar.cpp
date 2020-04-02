
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
#include "foundation/core/concepts/iunknown.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/registrar.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Utility_Registrar)
{
    struct Item
      : public IUnknown
    {
        int m_value;

        explicit Item(const int value)
          : m_value(value)
        {
        }

        void release() override
        {
            delete this;
        }
    };

    TEST_CASE(Lookup_GivenNameOfRegisteredItem_ReturnsItem)
    {
        Registrar<Item> registrar;
        registrar.insert("key", auto_release_ptr<Item>(new Item(42)));

        const Item* item = registrar.lookup("key");

        ASSERT_NEQ(0, item);
        EXPECT_EQ(42, item->m_value);
    }

    TEST_CASE(Lookup_GivenNameOfNonRegisteredItem_ReturnsZero)
    {
        Registrar<Item> registrar;

        const Item* item = registrar.lookup("key");

        EXPECT_EQ(0, item);
    }

    TEST_CASE(Insert_GivenNameOfAlreadyRegisteredItem_ReplacesItem)
    {
        Registrar<Item> registrar;
        registrar.insert("key", auto_release_ptr<Item>(new Item(42)));
        registrar.insert("key", auto_release_ptr<Item>(new Item(66)));

        const Item* item = registrar.lookup("key");

        ASSERT_NEQ(0, item);
        EXPECT_EQ(66, item->m_value);
    }

    struct ItemWithReleaseCheck
      : public IUnknown
    {
        bool* m_release_was_called;

        explicit ItemWithReleaseCheck(bool* release_was_called = nullptr)
          : m_release_was_called(release_was_called)
        {
        }

        void release() override
        {
            if (m_release_was_called != nullptr)
                *m_release_was_called = true;

            delete this;
        }
    };

    TEST_CASE(Insert_GivenNameOfAlreadyRegisteredItem_DestructsPreviousItem)
    {
        bool release_was_called = false;

        Registrar<ItemWithReleaseCheck> registrar;
        registrar.insert("key", auto_release_ptr<ItemWithReleaseCheck>(new ItemWithReleaseCheck(&release_was_called)));
        registrar.insert("key", auto_release_ptr<ItemWithReleaseCheck>(new ItemWithReleaseCheck()));

        EXPECT_TRUE(release_was_called);
    }

    TEST_CASE(Destructor_DestructsItems)
    {
        bool release_was_called = false;

        {
            Registrar<ItemWithReleaseCheck> registrar;
            registrar.insert("key", auto_release_ptr<ItemWithReleaseCheck>(new ItemWithReleaseCheck(&release_was_called)));
        }

        EXPECT_TRUE(release_was_called);
    }
}
