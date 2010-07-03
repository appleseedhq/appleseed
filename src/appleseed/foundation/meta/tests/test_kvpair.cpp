
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
#include "foundation/utility/kvpair.h"
#include "foundation/utility/test.h"

FOUNDATION_TEST_SUITE(Foundation_Utility_KVPair)
{
    using namespace foundation;

    FOUNDATION_TEST_CASE(FoundationLookupKVPairArray_GivenKeyOfExistingEntry_ReturnsPointerToEntry)
    {
        enum Colors { Red, Green, Blue };

        static const KeyValuePair<Colors, const char*> ColorNames[] =
        {
            { Red, "Red" },
            { Green, "Green" },
            { Blue, "Blue" }
        };

        const KeyValuePair<Colors, const char*>* kvpair =
            FOUNDATION_LOOKUP_KVPAIR_ARRAY(ColorNames, Green);

        FOUNDATION_ASSERT_NEQ(0, kvpair);

        FOUNDATION_EXPECT_EQ("Green", kvpair->m_value);
    }

    FOUNDATION_TEST_CASE(FoundationLookupKVPairArray_GivenKeyOfNonExistingEntry_ReturnsNullPointer)
    {
        enum Colors { Red, Green, Blue };

        static const KeyValuePair<Colors, const char*> ColorNames[] =
        {
            { Green, "Green" },
            { Blue, "Blue" }
        };

        const KeyValuePair<Colors, const char*>* kvpair =
            FOUNDATION_LOOKUP_KVPAIR_ARRAY(ColorNames, Red);

        FOUNDATION_EXPECT_EQ(0, kvpair);
    }
}
