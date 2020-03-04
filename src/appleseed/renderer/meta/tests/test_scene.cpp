
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Scene_Scene)
{
    TEST_CASE(ComputeBbox_GivenEmptyScene_ReturnsEmptyBoundingBox)
    {
        auto_release_ptr<Scene> scene(SceneFactory::create());

        const GAABB3 bbox = scene->compute_bbox();

        ASSERT_TRUE(bbox.is_valid());
        EXPECT_EQ(GVector3(0.0), bbox.min);
        EXPECT_EQ(GVector3(0.0), bbox.max);
    }

    TEST_CASE(ComputeBbox_GivenSceneWithOneAssemblyInstance_ReturnsBoundingBox)
    {
        // Create a scene.
        auto_release_ptr<Scene> scene(SceneFactory::create());

        // Create an assembly.
        auto_release_ptr<Assembly> assembly(
            AssemblyFactory().create("assembly", ParamArray()));

        // Create an object.
        assembly->objects().insert(
            auto_release_ptr<Object>(
                new BoundingBoxObject(
                    "object",
                    GAABB3(GVector3(-1.0), GVector3(+1.0)))));

        // Create an instance of the object.
        assembly->object_instances().insert(
            ObjectInstanceFactory::create(
                "object_inst",
                ParamArray(),
                "object",
                Transformd::identity(),
                StringDictionary()));

        // Create an instance of the assembly.
        auto_release_ptr<AssemblyInstance> assembly_instance(
            AssemblyInstanceFactory::create(
                "assembly_inst",
                ParamArray(),
                "assembly"));
        assembly_instance->transform_sequence().set_transform(
            0.0f,
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(1.0)) *
                Matrix4d::make_scaling(Vector3d(10.0))));

        // Insert the assembly and the assembly instance into the scene.
        scene->assemblies().insert(assembly);
        scene->assembly_instances().insert(assembly_instance);

        const GAABB3 bbox = scene->compute_bbox();

        EXPECT_FEQ(GVector3( -9.0), bbox.min);
        EXPECT_FEQ(GVector3(+11.0), bbox.max);
    }
}
