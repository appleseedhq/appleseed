
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
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/lazy.h"
#include "foundation/utility/test.h"

TEST_SUITE(Renderer_Kernel_Intersection_Intersector)
{
    using namespace foundation;
    using namespace renderer;

    class BoundingBoxObject
      : public Object
    {
      public:
        BoundingBoxObject()
          : Object(ParamArray())
          , m_bbox(GAABB3::VectorType(-1.0), GAABB3::VectorType(1.0))
          , m_lazy_region_kit(&m_region_kit)
        {
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return "BoundingBoxObject";
        }

        virtual const GAABB3& get_local_bbox() const
        {
            return m_bbox;
        }

        virtual Lazy<RegionKit>& get_region_kit()
        {
            return m_lazy_region_kit;
        }

      private:
        const GAABB3    m_bbox;
        RegionKit       m_region_kit;
        Lazy<RegionKit> m_lazy_region_kit;
    };

    class TestScene
      : public Scene
    {
      public:
        TestScene()
        {
            auto_release_ptr<Assembly> assembly(
                AssemblyFactory::create("assembly", ParamArray()));

            BoundingBoxObject* object = new BoundingBoxObject();

            const size_t object_index =
                assembly->objects().insert(auto_release_ptr<Object>(object));

            assembly->object_instances().insert(
                ObjectInstanceFactory::create(
                    "object_instance",
                    *object,
                    object_index,
                    Transformd(Matrix4d::identity()),
                    MaterialIndexArray()));

            assembly_instances().insert(
                auto_release_ptr<AssemblyInstance>(
                    AssemblyInstanceFactory::create(
                        "assembly_instance",
                        *assembly,
                        Transformd(Matrix4d::identity()))));

            assemblies().insert(assembly);
        }
    };

    TEST_CASE(Trace_GivenAssemblyContainingEmptyBoundingBoxAndRayWithTMaxInsideAssembly_ReturnsFalse)
    {
        const TestScene scene;
        const TraceContext trace_context(scene);
        const Intersector intersector(trace_context);

        const ShadingRay ray(
            Vector3d(0.0, 0.0, 2.0),
            Vector3d(0.0, 0.0, -1.0),
            0.0,
            2.0,
            0.0f,
            ~0);

        ShadingPoint shading_point;
        const bool hit = intersector.trace(ray, shading_point);

        EXPECT_FALSE(hit);
    }

    TEST_CASE(TraceProbe_GivenAssemblyContainingEmptyBoundingBoxAndRayWithTMaxInsideAssembly_ReturnsFalse)
    {
        const TestScene scene;
        const TraceContext trace_context(scene);
        const Intersector intersector(trace_context);

        const ShadingRay ray(
            Vector3d(0.0, 0.0, 2.0),
            Vector3d(0.0, 0.0, -1.0),
            0.0,
            2.0,
            0.0f,
            ~0);

        const bool hit = intersector.trace_probe(ray);

        EXPECT_FALSE(hit);
    }
}
