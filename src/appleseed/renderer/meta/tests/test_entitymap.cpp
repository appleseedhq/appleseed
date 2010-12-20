
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

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entitymap.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

TEST_SUITE(Renderer_Modeling_Entity_EntityMap)
{
    using namespace foundation;
    using namespace renderer;

    TEST_CASE(Swap_GivenEntityMapWithOneItemAndAnotherEmptyEntityMap_MovesItemToOtherContainer)
    {
        EntityMap m1;
        m1.insert(DummyEntityFactory::create("dummy"));

        EntityMap m2;
        m2.swap(m1);

        EXPECT_TRUE(m1.empty());
        EXPECT_FALSE(m2.empty());
    }
}
