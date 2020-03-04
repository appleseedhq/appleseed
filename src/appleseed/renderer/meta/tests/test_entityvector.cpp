
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
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"
#include "foundation/utility/uid.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Entity_EntityVector)
{
    TEST_CASE(Insert_SetsParentPointerOnEntity)
    {
        auto_release_ptr<Entity> entity(new DummyEntity("entity"));
        const Entity* entity_ptr = entity.get();

        Entity* parent = (Entity*)0x123;
        EntityVector v(parent);
        v.insert(entity);

        EXPECT_EQ(parent, entity_ptr->get_parent());
    }

    TEST_CASE(Swap_GivenEntityVectorWithOneItemAndAnotherEmptyEntityVector_MovesItemToOtherContainer)
    {
        EntityVector v1;
        v1.insert(auto_release_ptr<Entity>(new DummyEntity("entity")));

        EntityVector v2;
        v2.swap(v1);

        EXPECT_TRUE(v1.empty());
        EXPECT_FALSE(v2.empty());
    }

    TEST_CASE(Swap_FixesParentPointerOnEntities)
    {
        Entity* parent1 = (Entity*)0x123;
        EntityVector v1(parent1);
        v1.insert(auto_release_ptr<Entity>(new DummyEntity("entity1")));

        Entity* parent2 = (Entity*)0x456;
        EntityVector v2(parent2);
        v2.insert(auto_release_ptr<Entity>(new DummyEntity("entity2")));

        v2.swap(v1);

        EXPECT_EQ(parent1, v1.get_by_name("entity2")->get_parent());
        EXPECT_EQ(parent2, v2.get_by_name("entity1")->get_parent());
    }

    TEST_CASE(Remove_GivenOneItem_RemovesItem)
    {
        auto_release_ptr<Entity> entity(new DummyEntity("entity"));
        Entity* entity_ptr = entity.get();

        EntityVector v;
        v.insert(entity);

        v.remove(entity_ptr);

        EXPECT_TRUE(v.empty());
    }

    TEST_CASE(Remove_GivenOneItem_ReleasesItem)
    {
        bool release_was_called = false;
        auto_release_ptr<Entity> entity(new DummyEntityReleaseCheck("entity", release_was_called));
        Entity* entity_ptr = entity.get();

        EntityVector v;
        v.insert(entity);

        v.remove(entity_ptr);

        EXPECT_TRUE(release_was_called);
    }

    TEST_CASE(Remove_RemovingFirstInsertedItemOfTwo_LeavesOtherItemIntact)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        Entity* entity1_ptr = entity1.get();
        Entity* entity2_ptr = entity2.get();

        EntityVector v;
        v.insert(entity1);
        v.insert(entity2);

        v.remove(entity1_ptr);

        ASSERT_EQ(1, v.size());
        EXPECT_EQ(entity2_ptr, v.get_by_name("entity2"));
    }

    TEST_CASE(Remove_RemovingLastInsertedItemOfTwo_LeavesOtherItemIntact)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        Entity* entity1_ptr = entity1.get();
        Entity* entity2_ptr = entity2.get();

        EntityVector v;
        v.insert(entity1);
        v.insert(entity2);

        v.remove(entity2_ptr);

        ASSERT_EQ(1, v.size());
        EXPECT_EQ(entity1_ptr, v.get_by_name("entity1"));
    }

    TEST_CASE(Remove_RemovesEntityFromIndexes)
    {
        auto_release_ptr<Entity> entity(new DummyEntity("entity"));
        const UniqueID entity_id = entity->get_uid();
        Entity* entity_ptr = entity.get();

        EntityVector v;
        v.insert(entity);

        v.remove(entity_ptr);

        EXPECT_EQ(~size_t(0), v.get_index(entity_id));
        EXPECT_EQ(~size_t(0), v.get_index("entity"));
    }

    TEST_CASE(GetIndex_GivenID_ReturnsIndex)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        const UniqueID entity2_id = entity2->get_uid();

        EntityVector v;
        v.insert(entity1);
        v.insert(entity2);

        EXPECT_EQ(1, v.get_index(entity2_id));
    }

    TEST_CASE(GetIndex_GivenName_ReturnsIndex)
    {
        EntityVector v;
        v.insert(auto_release_ptr<Entity>(new DummyEntity("entity1")));
        v.insert(auto_release_ptr<Entity>(new DummyEntity("entity2")));

        EXPECT_EQ(1, v.get_index("entity2"));
    }

    TEST_CASE(GetByIndex_GivenIndex_ReturnsEntity)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        const Entity* entity2_ptr = entity2.get();

        EntityVector v;
        v.insert(entity1);
        v.insert(entity2);

        EXPECT_EQ(entity2_ptr, v.get_by_index(1));
    }

    TEST_CASE(GetByUID_GivenID_ReturnsEntity)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        const UniqueID entity2_id = entity2->get_uid();
        const Entity* entity2_ptr = entity2.get();

        EntityVector v;
        v.insert(entity1);
        v.insert(entity2);

        EXPECT_EQ(entity2_ptr, v.get_by_uid(entity2_id));
    }

    TEST_CASE(GetByName_GivenName_ReturnsEntity)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        const Entity* entity2_ptr = entity2.get();

        EntityVector v;
        v.insert(entity1);
        v.insert(entity2);

        EXPECT_EQ(entity2_ptr, v.get_by_name("entity2"));
    }
}
