
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

// appleseed.renderer headers.
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Scene_Containers)
{
    typedef TypedEntityVector<DummyEntity> DummyEntityVector;

    TEST_CASE(MakeUniqueName_GivenZeroEntity_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;

        const std::string result = make_unique_name("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }

    TEST_CASE(MakeUniqueName_GivenTwoEntitiesWithMatchingPrefixes_ReturnsNameWithNextSuffix)
    {
        DummyEntityVector entities;
        entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly3")));
        entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly1")));

        const std::string result = make_unique_name("assembly", entities);

        EXPECT_EQ("assembly4", result);
    }

    TEST_CASE(MakeUniqueName_GivenEntityWithNegativeSuffix_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;
        entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly-5")));

        const std::string result = make_unique_name("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }

    TEST_CASE(MakeUniqueName_GivenOneEntityWithNonMatchingPrefix_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;
        entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("object")));

        const std::string result = make_unique_name("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }

    TEST_CASE(MakeUniqueName_GivenOneEntityWithNonNumericSuffix_ReturnsNameWithFirstSuffix)
    {
        DummyEntityVector entities;
        entities.insert(auto_release_ptr<DummyEntity>(new DummyEntity("assembly_instance")));

        const std::string result = make_unique_name("assembly", entities);

        EXPECT_EQ("assembly1", result);
    }
}
