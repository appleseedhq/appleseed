
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
#include "foundation/utility/test.h"
#include "foundation/utility/typetraits.h"

using namespace foundation;

TEST_SUITE(Foundation_Core_TypeTraits)
{
    struct Base {};

    struct NotDerived {};

    struct Derived : public Base {};

    struct IndirectlyDerived : public Derived {};

    TEST_CASE(IsBase_GivenClassDerivingFromBase_ReturnsTrue)
    {
        const bool result = IsBase<Base, Derived>::value;

        EXPECT_TRUE(result);
    }

    TEST_CASE(IsBase_GivenClassNotDerivingFromBase_ReturnsFalse)
    {
        const bool result = IsBase<Base, NotDerived>::value;

        EXPECT_FALSE(result);
    }

    TEST_CASE(IsBase_GivenSameBaseAndDerivedClasses_ReturnsTrue)
    {
        const bool result = IsBase<Base, Base>::value;

        EXPECT_TRUE(result);
    }

    TEST_CASE(IsBase_GivenClassIndirectlyDerivingFromBase_ReturnsTrue)
    {
        const bool result = IsBase<Base, IndirectlyDerived>::value;

        EXPECT_TRUE(result);
    }

    TEST_CASE(IsNotBase_GivenClassDerivingFromBase_ReturnsFalse)
    {
        const bool result = IsNotBase<Base, Derived>::value;

        EXPECT_FALSE(result);
    }

    TEST_CASE(IsNotBase_GivenClassNotDerivingFromBase_ReturnsTrue)
    {
        const bool result = IsNotBase<Base, NotDerived>::value;

        EXPECT_TRUE(result);
    }

    TEST_CASE(IsNotBase_GivenSameBaseAndDerivedClasses_ReturnsFalse)
    {
        const bool result = IsNotBase<Base, Base>::value;

        EXPECT_FALSE(result);
    }

    TEST_CASE(IsNotBase_GivenClassIndirectlyDerivingFromBase_ReturnsFalse)
    {
        const bool result = IsNotBase<Base, IndirectlyDerived>::value;

        EXPECT_FALSE(result);
    }
}
