
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
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Scene_Assembly)
{
    struct TestScene
    {
        auto_release_ptr<Scene> m_scene;

        TestScene()
          : m_scene(SceneFactory::create())
        {
            // Inner assembly.

            auto_release_ptr<Assembly> inner_assembly(
                AssemblyFactory().create("inner_assembly", ParamArray()));

            inner_assembly->objects().insert(
                auto_release_ptr<Object>(
                    new BoundingBoxObject(
                        "object",
                        GAABB3(GVector3(-1.0), GVector3(1.0)))));

            inner_assembly->object_instances().insert(
                ObjectInstanceFactory::create(
                    "object_instance",
                    ParamArray(),
                    "object",
                    Transformd::identity(),
                    StringDictionary()));

            // Inner assembly instance.

            auto_release_ptr<AssemblyInstance> inner_assembly_instance(
                AssemblyInstanceFactory::create(
                    "inner_assembly_instance",
                    ParamArray(),
                    "inner_assembly"));

            inner_assembly_instance->transform_sequence().set_transform(
                0.0f,
                Transformd::from_local_to_parent(
                    Matrix4d::make_scaling(Vector3d(10.0))));

            // Outer assembly.

            auto_release_ptr<Assembly> outer_assembly(
                AssemblyFactory().create(
                    "outer_assembly",
                    ParamArray()));

            outer_assembly->assemblies().insert(inner_assembly);
            outer_assembly->assembly_instances().insert(inner_assembly_instance);

            m_scene->assemblies().insert(outer_assembly);
        }
    };

    TEST_CASE_F(ComputeLocalBBox_TakesChildAssemblyInstanceIntoAccount, TestScene)
    {
        const Assembly* outer_assembly = m_scene->assemblies().get_by_name("outer_assembly");
        const GAABB3 local_bbox = outer_assembly->compute_local_bbox();

        EXPECT_EQ(GAABB3(GVector3(-10.0), GVector3(10.0)), local_bbox);
    }

    TEST_CASE_F(ComputeNonHierarchicalLocalBBox_DoesNotTakeChildAssemblyInstanceIntoAccount, TestScene)
    {
        const Assembly* outer_assembly = m_scene->assemblies().get_by_name("outer_assembly");
        const GAABB3 local_bbox = outer_assembly->compute_non_hierarchical_local_bbox();

        EXPECT_EQ(GAABB3::invalid(), local_bbox);
    }
}
