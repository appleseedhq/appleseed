
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

// appleseed.foundation headers.
#include "foundation/utility/test.h"

FOUNDATION_TEST_SUITE(Renderer_Modeling_Entity_EntityVector)
{
    using namespace foundation;
    using namespace renderer;

    struct DummyEntity
      : public Entity
    {
        virtual void release()
        {
            delete this;
        }

        virtual const char* get_name() const
        {
            return "DummyEntity";
        }
    };

    FOUNDATION_TEST_CASE(Swap_GivenEntityVectorWithOneItemAndAnotherEmptyEntityVector_MovesItemToOtherContainer)
    {
        EntityVector v1;
        v1.insert(auto_release_ptr<Entity>(new DummyEntity()));

        EntityVector v2;
        v2.swap(v1);

        FOUNDATION_EXPECT_TRUE(v1.empty());
        FOUNDATION_EXPECT_FALSE(v2.empty());
    }
}
