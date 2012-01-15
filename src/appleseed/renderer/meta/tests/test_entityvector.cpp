
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
        v1.insert(auto_release_ptr<Entity>(DummyEntityFactory::create("dummy")));

        EntityVector v2;
        v2.swap(v1);

        EXPECT_TRUE(v1.empty());
        EXPECT_FALSE(v2.empty());
    }

    TEST_CASE(Remove_GivenOneItem_RemovesItem)
    {
        auto_release_ptr<Entity> dummy(DummyEntityFactory::create("dummy"));
        Entity* dummy_ptr = dummy.get();

        EntityVector v;
        v.insert(dummy);

        v.remove(dummy_ptr);

        EXPECT_TRUE(v.empty());
    }

    struct DummyEntityReleaseCheck
      : public Entity
    {
        bool& m_release_was_called;

        DummyEntityReleaseCheck(
            const char* name,
            bool&       release_was_called)
          : Entity(0)
          , m_release_was_called(release_was_called)
        {
            set_name(name);
        }

        virtual void release()
        {
            m_release_was_called = true;
            delete this;
        }
    };

    TEST_CASE(Remove_GivenOneItem_ReleasesItem)
    {
        bool release_was_called = false;
        auto_release_ptr<Entity> dummy(
            new DummyEntityReleaseCheck("dummy", release_was_called));
        Entity* dummy_ptr = dummy.get();

        EntityVector v;
        v.insert(dummy);

        v.remove(dummy_ptr);

        EXPECT_TRUE(release_was_called);
    }

    TEST_CASE(Remove_RemovingFirstInsertedItemOfTwo_LeavesOtherItemIntact)
    {
        auto_release_ptr<Entity> dummy1(DummyEntityFactory::create("dummy1"));
        auto_release_ptr<Entity> dummy2(DummyEntityFactory::create("dummy2"));
        Entity* dummy1_ptr = dummy1.get();
        Entity* dummy2_ptr = dummy2.get();

        EntityVector v;
        v.insert(dummy1);
        v.insert(dummy2);

        v.remove(dummy1_ptr);

        ASSERT_EQ(1, v.size());
        EXPECT_EQ(dummy2_ptr, v.get_by_name("dummy2"));
    }

    TEST_CASE(Remove_RemovingLastInsertedItemOfTwo_LeavesOtherItemIntact)
    {
        auto_release_ptr<Entity> dummy1(DummyEntityFactory::create("dummy1"));
        auto_release_ptr<Entity> dummy2(DummyEntityFactory::create("dummy2"));
        Entity* dummy1_ptr = dummy1.get();
        Entity* dummy2_ptr = dummy2.get();

        EntityVector v;
        v.insert(dummy1);
        v.insert(dummy2);

        v.remove(dummy2_ptr);

        ASSERT_EQ(1, v.size());
        EXPECT_EQ(dummy1_ptr, v.get_by_name("dummy1"));
    }

    TEST_CASE(Remove_RemovesEntityFromIndexes)
    {
        auto_release_ptr<Entity> dummy(DummyEntityFactory::create("dummy"));
        const UniqueID dummy_id = dummy->get_uid();
        Entity* dummy_ptr = dummy.get();

        EntityVector v;
        v.insert(dummy);

        v.remove(dummy_ptr);

        EXPECT_EQ(~size_t(0), v.get_index(dummy_id));
        EXPECT_EQ(~size_t(0), v.get_index("dummy"));
    }

    TEST_CASE(GetIndex_GivenID_ReturnsIndex)
    {
        auto_release_ptr<Entity> dummy1(DummyEntityFactory::create("dummy1"));
        auto_release_ptr<Entity> dummy2(DummyEntityFactory::create("dummy2"));
        const UniqueID dummy2_id = dummy2->get_uid();

        EntityVector v;
        v.insert(dummy1);
        v.insert(dummy2);

        EXPECT_EQ(1, v.get_index(dummy2_id));
    }

    TEST_CASE(GetIndex_GivenName_ReturnsIndex)
    {
        EntityVector v;
        v.insert(auto_release_ptr<Entity>(DummyEntityFactory::create("dummy1")));
        v.insert(auto_release_ptr<Entity>(DummyEntityFactory::create("dummy2")));

        EXPECT_EQ(1, v.get_index("dummy2"));
    }

    TEST_CASE(GetByIndex_GivenIndex_ReturnsEntity)
    {
        auto_release_ptr<Entity> dummy1(DummyEntityFactory::create("dummy1"));
        auto_release_ptr<Entity> dummy2(DummyEntityFactory::create("dummy2"));
        const Entity* dummy2_ptr = dummy2.get();

        EntityVector v;
        v.insert(dummy1);
        v.insert(dummy2);

        EXPECT_EQ(dummy2_ptr, v.get_by_index(1));
    }

    TEST_CASE(GetByUID_GivenID_ReturnsEntity)
    {
        auto_release_ptr<Entity> dummy1(DummyEntityFactory::create("dummy1"));
        auto_release_ptr<Entity> dummy2(DummyEntityFactory::create("dummy2"));
        const UniqueID dummy2_id = dummy2->get_uid();
        const Entity* dummy2_ptr = dummy2.get();

        EntityVector v;
        v.insert(dummy1);
        v.insert(dummy2);

        EXPECT_EQ(dummy2_ptr, v.get_by_uid(dummy2_id));
    }

    TEST_CASE(GetByName_GivenName_ReturnsEntity)
    {
        auto_release_ptr<Entity> dummy1(DummyEntityFactory::create("dummy1"));
        auto_release_ptr<Entity> dummy2(DummyEntityFactory::create("dummy2"));
        const Entity* dummy2_ptr = dummy2.get();

        EntityVector v;
        v.insert(dummy1);
        v.insert(dummy2);

        EXPECT_EQ(dummy2_ptr, v.get_by_name("dummy2"));
    }
}
