
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
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Utility_AutoReleasePtr)
{
    struct Base
      : public IUnknown
    {
    };

    struct Derived
      : public Base
    {
        int m_value;

        explicit Derived(const int value = 42)
          : m_value(value)
        {
        }

        void release() override
        {
            delete this;
        }
    };

    struct DerivedWithReleaseCheck
      : public Base
    {
        bool& m_release_was_called;

        explicit DerivedWithReleaseCheck(bool& release_was_called)
          : m_release_was_called(release_was_called)
        {
        }

        void release() override
        {
            m_release_was_called = true;
            delete this;
        }
    };

    TEST_CASE(DefaultConstruct)
    {
        auto_release_ptr<Derived> ptr;

        EXPECT_EQ(0, ptr.get());
    }

    TEST_CASE(ConstructFromNullPointer)
    {
        auto_release_ptr<Derived> ptr(nullptr);

        EXPECT_EQ(0, ptr.get());
    }

    TEST_CASE(ConstructFromRawPointerOfSameType)
    {
        auto_release_ptr<Derived> ptr(new Derived());

        EXPECT_NEQ(0, ptr.get());
    }

    TEST_CASE(ConstructFromRawPointerOfRelatedType)
    {
        auto_release_ptr<Base> ptr(new Derived());

        EXPECT_NEQ(0, ptr.get());
    }

    auto_release_ptr<Derived> source()
    {
        return auto_release_ptr<Derived>(new Derived());
    }

    TEST_CASE(ConstructFromSource)
    {
        auto_release_ptr<Derived> ptr(source());

        EXPECT_NEQ(0, ptr.get());
    }

    TEST_CASE(Destructor_CallsReleaseOnOwnedPointer)
    {
        bool release_was_called = false;

        {
            auto_release_ptr<DerivedWithReleaseCheck> ptr(
                new DerivedWithReleaseCheck(release_was_called));
        }

        EXPECT_TRUE(release_was_called);
    }

    TEST_CASE(AssignToAutoReleasePtrOfSameType)
    {
        auto_release_ptr<Derived> ptr1(new Derived());
        auto_release_ptr<Derived> ptr2;

        ptr2 = ptr1;

        EXPECT_EQ(0, ptr1.get());
        EXPECT_NEQ(0, ptr2.get());
    }

    TEST_CASE(AssignToAutoReleasePtrOfRelatedType)
    {
        auto_release_ptr<Derived> ptr1(new Derived());
        auto_release_ptr<Base> ptr2;

        ptr2 = ptr1;

        EXPECT_EQ(0, ptr1.get());
        EXPECT_NEQ(0, ptr2.get());
    }

    TEST_CASE(DereferenceUsingStarOperator)
    {
        auto_release_ptr<Derived> ptr(new Derived(42));

        EXPECT_EQ(42, (*ptr).m_value);
    }

    TEST_CASE(DereferenceUsingArrowOperator)
    {
        auto_release_ptr<Derived> ptr(new Derived(42));

        EXPECT_EQ(42, ptr->m_value);
    }

    TEST_CASE(Reset_GivenNullPointer_ReleasesPreviouslyOwnedPointerAndStoreNewPointer)
    {
        bool release_was_called = false;

        auto_release_ptr<DerivedWithReleaseCheck> ptr(
            new DerivedWithReleaseCheck(release_was_called));

        ptr.reset(nullptr);

        EXPECT_TRUE(release_was_called);
    }

    TEST_CASE(ImplicitConversionToBool)
    {
        auto_release_ptr<Derived> ptr(new Derived());

        const bool value = ptr ? true : false;

        EXPECT_TRUE(value);
    }
}
