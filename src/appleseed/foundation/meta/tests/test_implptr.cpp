
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

// Standard headers.
#include <cstddef>

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/implptr.h"
#include "foundation/utility/test.h"

FOUNDATION_TEST_SUITE(Foundation_Utility_ImplPtr)
{
    using namespace foundation;
    using namespace std;

    struct Watchers
    {
        size_t m_default_constructed;
        size_t m_copy_constructed;
        size_t m_destructed;
        size_t m_assigned;

        Watchers()
          : m_default_constructed(0)
          , m_copy_constructed(0)
          , m_destructed(0)
          , m_assigned(0)
        {
        }
    };

    class Foo
    {
      public:
        explicit Foo(Watchers& watchers);

      private:
        struct Impl;
        impl_ptr<Impl> impl;
    };

    struct Foo::Impl
    {
        Watchers& m_watchers;

        explicit Impl(Watchers& watchers)
          : m_watchers(watchers)
        {
            ++m_watchers.m_default_constructed;
        }

        Impl(const Impl& rhs)
          : m_watchers(rhs.m_watchers)
        {
            ++m_watchers.m_copy_constructed;
        }

        ~Impl()
        {
            ++m_watchers.m_destructed;
        }

        Impl& operator=(const Impl& rhs)
        {
            ++m_watchers.m_assigned;
            return *this;
        }
    };

    Foo::Foo(Watchers& watchers)
      : impl(new Impl(watchers))
    {
    }

    FOUNDATION_TEST_CASE(ImplDefaultConstructorIsProperlyCalled)
    {
        Watchers watchers;

        {
            Foo foo(watchers);
        }

        FOUNDATION_EXPECT_EQ(1, watchers.m_default_constructed);
    }

    FOUNDATION_TEST_CASE(ImplCopyConstructorIsProperlyCalled)
    {
        Watchers watchers;

        {
            Foo foo(watchers);
            Foo copy(foo);
        }

        FOUNDATION_EXPECT_EQ(1, watchers.m_copy_constructed);
    }

    FOUNDATION_TEST_CASE(ImplAssignmentOperatorIsProperlyCalled)
    {
        Watchers watchers;

        {
            Foo foo1(watchers);
            Foo foo2(watchers);
            foo1 = foo2;
        }

        FOUNDATION_EXPECT_EQ(1, watchers.m_assigned);
    }

    FOUNDATION_TEST_CASE(ImplDestructorIsProperlyCalled)
    {
        Watchers watchers;

        {
            Foo foo(watchers);
        }

        FOUNDATION_EXPECT_EQ(1, watchers.m_destructed);
    }

    class CopyableFoo
    {
      public:
        CopyableFoo(const int x);
        int get() const;

      private:
        struct Impl;
        impl_ptr<Impl> impl;
    };

    struct CopyableFoo::Impl
    {
        int x;

        explicit Impl(int x_) : x(x_) {}
    };

    CopyableFoo::CopyableFoo(const int x)
      : impl(new Impl(x))
    {
    }

    int CopyableFoo::get() const
    {
        return impl->x;
    }

    FOUNDATION_TEST_CASE(ConstructionOfCopyableClass)
    {
        CopyableFoo foo(42);

        FOUNDATION_EXPECT_EQ(42, foo.get());
    }

    FOUNDATION_TEST_CASE(CopyConstructionOfCopyableClass)
    {
        CopyableFoo foo(42);

        CopyableFoo copy(foo);

        FOUNDATION_EXPECT_EQ(42, copy.get());
    }

    FOUNDATION_TEST_CASE(AssignmentOfCopyableClass)
    {
        CopyableFoo foo(42);
        CopyableFoo copy(0);

        copy = foo;

        FOUNDATION_EXPECT_EQ(42, copy.get());
    }

    class NonCopyableFoo
    {
      public:
        NonCopyableFoo(const int x);
        int get() const;

      private:
        struct Impl;
        impl_ptr<Impl, false> impl;
    };

    struct NonCopyableFoo::Impl
      : public NonCopyable
    {
        int x;

        explicit Impl(int x_) : x(x_) {}
    };

    NonCopyableFoo::NonCopyableFoo(const int x)
      : impl(new Impl(x))
    {
    }

    int NonCopyableFoo::get() const
    {
        return impl->x;
    }

    FOUNDATION_TEST_CASE(ConstructionOfNonCopyableClass)
    {
        NonCopyableFoo foo(42);

        FOUNDATION_EXPECT_EQ(42, foo.get());
    }

    class NonCopyableFoo2
      : public NonCopyable
    {
      public:
        NonCopyableFoo2(const int x);
        int get() const;

      private:
        FOUNDATION_PIMPL(NonCopyableFoo2);
    };

    struct NonCopyableFoo2::Impl
    {
        int x;

        explicit Impl(int x_) : x(x_) {}
    };

    NonCopyableFoo2::NonCopyableFoo2(const int x)
      : impl(new Impl(x))
    {
    }

    int NonCopyableFoo2::get() const
    {
        return impl->x;
    }
}
