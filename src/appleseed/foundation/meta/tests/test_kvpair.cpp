
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
#include "foundation/utility/kvpair.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Utility_KVPair)
{
    enum Colors { Red, Green, Blue };

    TEST_CASE(FoundationLookupKVPairArray_GivenKeyOfExistingEntry_ReturnsPointerToEntry)
    {
        static const KeyValuePair<Colors, const char*> ColorNames[] =
        {
            { Red, "Red" },
            { Green, "Green" },
            { Blue, "Blue" }
        };

        const KeyValuePair<Colors, const char*>* kvpair =
            LOOKUP_KVPAIR_ARRAY(ColorNames, Green);

        ASSERT_NEQ(0, kvpair);

        EXPECT_EQ(std::string("Green"), kvpair->m_value);
    }

    TEST_CASE(FoundationLookupKVPairArray_GivenKeyOfNonExistingEntry_ReturnsNullPointer)
    {
        static const KeyValuePair<Colors, const char*> ColorNames[] =
        {
            { Green, "Green" },
            { Blue, "Blue" }
        };

        const KeyValuePair<Colors, const char*>* kvpair =
            LOOKUP_KVPAIR_ARRAY(ColorNames, Red);

        EXPECT_EQ(0, kvpair);
    }
}
