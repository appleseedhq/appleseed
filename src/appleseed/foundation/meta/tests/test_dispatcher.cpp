
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
#include "foundation/utility/dispatcher.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <memory>
#include <string>

FOUNDATION_TEST_SUITE(Foundation_Utility_Dispatcher)
{
    using namespace foundation;
    using namespace std;

    struct Fruit
    {
        string m_name;
        string m_color;
        Fruit(const string& name, const string& color)
          : m_name(name)
          , m_color(color)
        {
        }
    };

    typedef Fruit* (*CreateFunctionPtr)(const string& color);

    struct AppleFactory
    {
        static Fruit* create(const string& color)
        {
            return new Fruit("apple", color);
        }
    };

    struct MangoFactory
    {
        static Fruit* create(const string& color)
        {
            return new Fruit("mango", color);
        }
    };

    FOUNDATION_TEST_CASE(CanDeclareFunctions)
    {
        Dispatcher<CreateFunctionPtr> dispatcher;
        FOUNDATION_EXPECT_TRUE(dispatcher.declare("apple", &AppleFactory::create));
        FOUNDATION_EXPECT_TRUE(dispatcher.declare("mango", &MangoFactory::create));
    }

    FOUNDATION_TEST_CASE(CannotDeclareDuplicateFunction)
    {
        Dispatcher<CreateFunctionPtr> dispatcher;
        dispatcher.declare("mango", &MangoFactory::create);
        FOUNDATION_EXPECT_FALSE(dispatcher.declare("mango", &MangoFactory::create));
    }

    FOUNDATION_TEST_CASE(CanLookUpDeclaredFunction)
    {
        Dispatcher<CreateFunctionPtr> dispatcher;
        dispatcher.declare("mango", &MangoFactory::create);
        FOUNDATION_EXPECT_NEQ(0, dispatcher.lookup("mango"));
    }

    FOUNDATION_TEST_CASE(CanLookUpUndeclaredFunction)
    {
        Dispatcher<CreateFunctionPtr> dispatcher;
        dispatcher.declare("mango", &MangoFactory::create);
        FOUNDATION_EXPECT_EQ(0, dispatcher.lookup("apple"));
    }

    FOUNDATION_TEST_CASE(LookedUpDeclaredFunctionWorks)
    {
        Dispatcher<CreateFunctionPtr> dispatcher;
        dispatcher.declare("mango", &MangoFactory::create);
        const CreateFunctionPtr create_mango = dispatcher.lookup("mango");
        auto_ptr<Fruit> mango(create_mango("yellow"));
        FOUNDATION_EXPECT_NEQ(0, mango.get());
    }
}
