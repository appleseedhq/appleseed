
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

// appleseed.studio headers.
#include "mainwindow/project/tools.h"

// appleseed.renderer headers.
#include "renderer/api/entity.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace appleseed::studio;
using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Studio_MainWindow_Project_Tools)
{
    typedef TypedEntityVector<DummyEntity> DummyEntityVector;

    TEST_CASE(GetNameSuggestion_GivenZeroEntity_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;

        const string result = get_name_suggestion("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }

    TEST_CASE(GetNameSuggestion_GivenTwoEntitiesWithMatchingPrefixes_ReturnsNameWithNextSuffix)
    {
        DummyEntityVector entities;
        entities.insert(DummyEntityFactory::create("assembly3"));
        entities.insert(DummyEntityFactory::create("assembly1"));

        const string result = get_name_suggestion("assembly", entities);

        EXPECT_EQ("assembly4", result);
    }

    TEST_CASE(GetNameSuggestion_GivenEntityWithNegativeSuffix_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;
        entities.insert(DummyEntityFactory::create("assembly-5"));

        const string result = get_name_suggestion("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }

    TEST_CASE(GetNameSuggestion_GivenOneEntityWithNonMatchingPrefix_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;
        entities.insert(DummyEntityFactory::create("object"));

        const string result = get_name_suggestion("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }

    TEST_CASE(GetNameSuggestion_GivenOneEntityWithNonNumericSuffix_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;
        entities.insert(DummyEntityFactory::create("assembly_instance"));

        const string result = get_name_suggestion("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }
}
