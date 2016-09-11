
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Kernel_Intersection_Intersector)
{
    struct TestScene
    {
        auto_release_ptr<Scene> m_scene;

        TestScene()
          : m_scene(SceneFactory::create())
        {
            auto_release_ptr<Assembly> assembly(
                AssemblyFactory().create("assembly", ParamArray()));

            assembly->objects().insert(
                auto_release_ptr<Object>(
                    new BoundingBoxObject(
                        "object",
                        GAABB3(GVector3(-1.0), GVector3(1.0)))));

            assembly->object_instances().insert(
                ObjectInstanceFactory::create(
                    "object_instance",
                    ParamArray(),
                    "object",
                    Transformd::identity(),
                    StringDictionary()));

            m_scene->assembly_instances().insert(
                auto_release_ptr<AssemblyInstance>(
                    AssemblyInstanceFactory::create(
                        "assembly_instance",
                        ParamArray(),
                        "assembly")));

            m_scene->assemblies().insert(assembly);
        }
    };

    struct Fixture
      : public BindInputs<TestScene>
    {
        TraceContext    m_trace_context;
        TextureStore    m_texture_store;
        TextureCache    m_texture_cache;
        Intersector     m_intersector;

        Fixture()
          : m_trace_context(m_scene.ref())
          , m_texture_store(m_scene.ref())
          , m_texture_cache(m_texture_store)
          , m_intersector(m_trace_context, m_texture_cache)
        {
        }
    };

    TEST_CASE_F(Trace_GivenAssemblyContainingEmptyBoundingBoxAndRayWithTMaxInsideAssembly_ReturnsFalse, Fixture)
    {
        const ShadingRay ray(
            Vector3d(0.0, 0.0, 2.0),
            Vector3d(0.0, 0.0, -1.0),
            0.0,                                // tmin
            2.0,                                // tmax
            ShadingRay::Time(),
            VisibilityFlags::CameraRay,
            0);                                 // depth

        ShadingPoint shading_point;
        const bool hit = m_intersector.trace(ray, shading_point);

        EXPECT_FALSE(hit);
    }

    TEST_CASE_F(TraceProbe_GivenAssemblyContainingEmptyBoundingBoxAndRayWithTMaxInsideAssembly_ReturnsFalse, Fixture)
    {
        const ShadingRay ray(
            Vector3d(0.0, 0.0, 2.0),
            Vector3d(0.0, 0.0, -1.0),
            0.0,                                // tmin
            2.0,                                // tmax
            ShadingRay::Time(),
            VisibilityFlags::CameraRay,
            0);                                 // depth

        const bool hit = m_intersector.trace_probe(ray);

        EXPECT_FALSE(hit);
    }
}
