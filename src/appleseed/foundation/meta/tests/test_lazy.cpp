
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/lazy.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <memory>

using namespace foundation;
using namespace std;

namespace
{
    struct Object
    {
        const int m_value;

        explicit Object(const int value)
          : m_value(value)
        {
        }
    };

    typedef ILazyFactory<Object> ObjectFactory;

    struct SimpleObjectFactory : public ObjectFactory
    {
        const int m_value;

        explicit SimpleObjectFactory(const int value)
          : m_value(value)
        {
        }

        virtual auto_ptr<Object> create()
        {
            return auto_ptr<Object>(new Object(m_value));
        }
    };
}

TEST_SUITE(Foundation_Utility_Lazy_Access)
{
    TEST_CASE(Get_GivenAccessBoundToNonNullObject_ReturnsNonNullPointer)
    {
        auto_ptr<ObjectFactory> factory(new SimpleObjectFactory(42));
        Lazy<Object> object(factory);

        Access<Object> access(&object);

        EXPECT_NEQ(0, access.get());
    }

    TEST_CASE(OperatorArrow_GivenAccessBoundToNonNullObject_GivesAccessToObject)
    {
        auto_ptr<ObjectFactory> factory(new SimpleObjectFactory(42));
        Lazy<Object> object(factory);

        Access<Object> access(&object);

        EXPECT_EQ(42, access->m_value);
    }

    TEST_CASE(OperatorStar_GivenAccessBoundToNonNullObject_GivesAccessToObject)
    {
        auto_ptr<ObjectFactory> factory(new SimpleObjectFactory(42));
        Lazy<Object> object(factory);

        Access<Object> access(&object);

        EXPECT_EQ(42, (*access).m_value);
    }

    struct NullObjectFactory : public ObjectFactory
    {
        virtual auto_ptr<Object> create()
        {
            return auto_ptr<Object>(0);
        }
    };

    TEST_CASE(Get_GivenAccessBoundToNullObject_ReturnsNullPointer)
    {
        auto_ptr<ObjectFactory> factory(new NullObjectFactory());
        Lazy<Object> object(factory);

        Access<Object> access(&object);

        EXPECT_EQ(0, access.get());
    }
}
