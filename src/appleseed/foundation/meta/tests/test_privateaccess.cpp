
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/utility/privateaccess.h"
#include "foundation/utility/test.h"

using namespace foundation;

namespace
{
    class A
    {
      public:
        A() : m_x(77) {}

      private:
        int fun()
        {
            return 42;
        }

        int m_x;
    };
}

namespace foundation
{
    // A tag type for A::x.  Each distinct private member you need to
    // access should have its own tag.  Each tag should contain a
    // nested ::type that is the corresponding pointer-to-member type.
    struct A_x
    {
        typedef int(A::*type);
    };

    // Explicit instantiation; the only place where it is legal to pass
    // the address of a private member.  Generates the static ::instance
    // that in turn initializes Stowed<Tag>::value.
    template struct StowPrivate<A_x, &A::m_x>;

    // Repeat for member function pointer.
    struct A_fun
    {
        typedef int(A::*type)();
    };

    template struct StowPrivate<A_fun, &A::fun>;
}


TEST_SUITE(Foundation_Utility_PrivateAccess)
{
    TEST_CASE(AccessPrivateDataMember)
    {
        A a;
        EXPECT_EQ(a.*Stowed<A_x>::value, 77);
    }

    TEST_CASE(AccessPrivateMemberFunction)
    {
        A a;
        EXPECT_EQ((a.*Stowed<A_fun>::value)(), 42);
    }
}
