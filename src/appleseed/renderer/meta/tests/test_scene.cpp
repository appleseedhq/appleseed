
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Modeling_Scene_Scene)
{
    TEST_CASE(ComputeRadius_GivenEmptyScene_ReturnsZero)
    {
        Scene scene;

        const double radius = scene.compute_radius();

        EXPECT_EQ(0.0, radius);
    }

    TEST_CASE(ComputeRadius_GivenSceneWithOneAssemblyInstance_ReturnsRadius)
    {
        Scene scene;

        // Create an assembly.
        auto_release_ptr<Assembly> assembly(
            AssemblyFactory::create("assembly", ParamArray()));

        // Create an object.
        MeshObject* object = MeshObjectFactory::create("object", ParamArray()).release();
        object->push_vertex(GVector3(-1.0f, -1.0f, -1.0f));
        object->push_vertex(GVector3( 1.0f,  1.0f,  1.0f));
        object->push_vertex(GVector3( 1.0f, -1.0f,  1.0f));
        object->push_triangle(Triangle(0, 1, 2, 0, 0, 0, 0));
        const size_t object_index =
            assembly->objects().insert(auto_release_ptr<Object>(object));

        // Create an instance of the object.
        assembly->object_instances().insert(
            ObjectInstanceFactory::create(
                "object_inst",
                *object,
                object_index,
                Transformd(Matrix4d::identity()),
                MaterialIndexArray()));

        // Create an instance of the assembly.
        scene.assembly_instances().insert(
            AssemblyInstanceFactory::create(
                "assembly_inst",
                *assembly,
                Transformd(Matrix4d::scaling(Vector3d(10.0)))));

        scene.assemblies().insert(assembly);

        const double radius = scene.compute_radius();

        EXPECT_FEQ(10.0 * sqrt(3.0), radius);
    }
}
