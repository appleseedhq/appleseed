
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
#include "renderer/modeling/entity/entitymap.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/test.h"
#include "foundation/utility/uid.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Entity_EntityMap)
{
    TEST_CASE(Insert_SetsParentPointerOnEntity)
    {
        auto_release_ptr<Entity> entity(new DummyEntity("entity"));
        const Entity* entity_ptr = entity.get();

        Entity* parent = (Entity*)0x123;
        EntityMap m(parent);
        m.insert(entity);

        EXPECT_EQ(parent, entity_ptr->get_parent());
    }

    TEST_CASE(Swap_GivenEntityMapWithOneItemAndAnotherEmptyEntityMap_MovesItemToOtherContainer)
    {
        EntityMap m1;
        m1.insert(auto_release_ptr<Entity>(new DummyEntity("entity")));

        EntityMap m2;
        m2.swap(m1);

        EXPECT_TRUE(m1.empty());
        EXPECT_FALSE(m2.empty());
    }

    TEST_CASE(Swap_FixesParentPointerOnEntities)
    {
        Entity* parent1 = (Entity*)0x123;
        EntityMap m1(parent1);
        m1.insert(auto_release_ptr<Entity>(new DummyEntity("entity1")));

        Entity* parent2 = (Entity*)0x456;
        EntityMap m2(parent2);
        m2.insert(auto_release_ptr<Entity>(new DummyEntity("entity2")));

        m2.swap(m1);

        EXPECT_EQ(parent1, m1.get_by_name("entity2")->get_parent());
        EXPECT_EQ(parent2, m2.get_by_name("entity1")->get_parent());
    }

    TEST_CASE(Remove_GivenUID_RemovesEntity)
    {
        auto_release_ptr<Entity> entity(new DummyEntity("entity"));
        const UniqueID entity_id = entity->get_uid();

        EntityMap m;
        m.insert(entity);

        m.remove(entity_id);

        EXPECT_TRUE(m.empty());
    }

    TEST_CASE(Remove_GivenUID_ReleasesEntity)
    {
        bool release_was_called = false;
        auto_release_ptr<Entity> entity(new DummyEntityReleaseCheck("entity", release_was_called));
        const UniqueID entity_id = entity->get_uid();

        EntityMap m;
        m.insert(entity);

        m.remove(entity_id);

        EXPECT_TRUE(release_was_called);
    }

    TEST_CASE(GetByUID_GivenUID_ReturnsEntity)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        const UniqueID entity2_id = entity2->get_uid();
        const Entity* entity2_ptr = entity2.get();

        EntityMap m;
        m.insert(entity1);
        m.insert(entity2);

        EXPECT_EQ(entity2_ptr, m.get_by_uid(entity2_id));
    }

    TEST_CASE(GetByName_GivenName_ReturnsEntity)
    {
        auto_release_ptr<Entity> entity1(new DummyEntity("entity1"));
        auto_release_ptr<Entity> entity2(new DummyEntity("entity2"));
        const Entity* entity2_ptr = entity2.get();

        EntityMap m;
        m.insert(entity1);
        m.insert(entity2);

        EXPECT_EQ(entity2_ptr, m.get_by_name("entity2"));
    }
}
