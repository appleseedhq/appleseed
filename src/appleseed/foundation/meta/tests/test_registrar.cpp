
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
#include "foundation/utility/registrar.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

FOUNDATION_TEST_SUITE(Foundation_Utility_Registrar)
{
    FOUNDATION_TEST_CASE(Lookup_GivenNameOfRegisteredItem_ReturnsItem)
    {
        Registrar<int> registrar;
        registrar.insert("x", auto_ptr<int>(new int(42)));

        const int* x = registrar.lookup("x");

        FOUNDATION_ASSERT_NEQ(0, x);
        FOUNDATION_EXPECT_EQ(42, *x);
    }

    FOUNDATION_TEST_CASE(Lookup_GivenNameOfNonRegisteredItem_ReturnsZero)
    {
        Registrar<int> registrar;

        const int* x = registrar.lookup("x");

        FOUNDATION_EXPECT_EQ(0, x);
    }

    FOUNDATION_TEST_CASE(Insert_GivenNameOfAlreadyRegisteredItem_ReplacesItem)
    {
        Registrar<int> registrar;
        registrar.insert("x", auto_ptr<int>(new int(42)));
        registrar.insert("x", auto_ptr<int>(new int(66)));

        const int* x = registrar.lookup("x");

        FOUNDATION_ASSERT_NEQ(0, x);
        FOUNDATION_EXPECT_EQ(66, *x);
    }

    struct Item
    {
        bool* m_destructed;

        explicit Item(bool* destructed = 0)
          : m_destructed(destructed)
        {
        }

        ~Item()
        {
            if (m_destructed)
                *m_destructed = true;
        }
    };

    FOUNDATION_TEST_CASE(Insert_GivenNameOfAlreadyRegisteredItem_DestructsPreviousItem)
    {
        bool destructed = false;

        Registrar<Item> registrar;
        registrar.insert("x", auto_ptr<Item>(new Item(&destructed)));
        registrar.insert("x", auto_ptr<Item>(new Item()));

        FOUNDATION_EXPECT_TRUE(destructed);
    }

    FOUNDATION_TEST_CASE(Destructor_DestructsItems)
    {
        bool destructed = false;

        {
            Registrar<Item> registrar;
            registrar.insert("x", auto_ptr<Item>(new Item(&destructed)));
        }

        FOUNDATION_EXPECT_TRUE(destructed);
    }
}
