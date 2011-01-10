
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
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Modeling_Entity_EntityVector)
{
    TEST_CASE(Swap_GivenEntityVectorWithOneItemAndAnotherEmptyEntityVector_MovesItemToOtherContainer)
    {
        EntityVector v1;
        v1.insert(DummyEntityFactory::create("dummy"));

        EntityVector v2;
        v2.swap(v1);

        EXPECT_TRUE(v1.empty());
        EXPECT_FALSE(v2.empty());
    }

    TEST_CASE(Remove_GivenOneItem_RemovesItem)
    {
        EntityVector v;
        v.insert(DummyEntityFactory::create("dummy"));

        Entity* entity = v.get(v.get_index("dummy"));
        v.remove(entity);

        EXPECT_TRUE(v.empty());
    }

    struct DummyEntityReleaseCheck
      : public Entity
    {
        bool m_release_was_called;

        explicit DummyEntityReleaseCheck(const char* name)
          : Entity(0)
          , m_release_was_called(false)
        {
            set_name(name);
        }

        virtual void release()
        {
            m_release_was_called = true;
        }
    };

    TEST_CASE(Remove_GivenOneItem_ReleasesItem)
    {
        auto_ptr<DummyEntityReleaseCheck> source_entity(new DummyEntityReleaseCheck("dummy"));

        EntityVector v;
        v.insert(auto_release_ptr<DummyEntityReleaseCheck>(source_entity.get()));

        Entity* entity = v.get(v.get_index("dummy"));
        v.remove(entity);

        EXPECT_TRUE(source_entity->m_release_was_called);
    }

    TEST_CASE(Remove_RemovingFirstInsertedItemOfTwo_LeavesOtherItemIntact)
    {
        EntityVector v;
        v.insert(DummyEntityFactory::create("dummy1"));
        v.insert(DummyEntityFactory::create("dummy2"));

        Entity* entity1 = v.get(v.get_index("dummy1"));
        Entity* entity2 = v.get(v.get_index("dummy2"));

        v.remove(entity1);

        ASSERT_EQ(1, v.size());
        EXPECT_EQ(entity2, v.get(v.get_index("dummy2")));
    }

    TEST_CASE(Remove_RemovingLastInsertedItemOfTwo_LeavesOtherItemIntact)
    {
        EntityVector v;
        v.insert(DummyEntityFactory::create("dummy1"));
        v.insert(DummyEntityFactory::create("dummy2"));

        Entity* entity1 = v.get(v.get_index("dummy1"));
        Entity* entity2 = v.get(v.get_index("dummy2"));

        v.remove(entity2);

        ASSERT_EQ(1, v.size());
        EXPECT_EQ(entity1, v.get(v.get_index("dummy1")));
    }
}
