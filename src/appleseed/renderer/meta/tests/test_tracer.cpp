
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/constantsurfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/matrix.h"
#include "foundation/math/rng.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Kernel_Lighting_Tracer)
{
    struct SceneBase
    {
        Scene   m_scene;
        size_t  m_opaque_material_index;
        size_t  m_transparent_material_index;

        SceneBase()
        {
            create_assembly();
            create_assembly_instance();
            create_plane_object();
            create_opaque_material();
            create_transparent_material();
        }

        void create_assembly()
        {
            m_scene.assemblies().insert(
                AssemblyFactory::create("assembly", ParamArray()));
        }

        void create_assembly_instance()
        {
            const Assembly* assembly = m_scene.assemblies().get("assembly");

            m_scene.assembly_instances().insert(
                AssemblyInstanceFactory::create(
                    "assembly_inst",
                    *assembly,
                    Transformd(Matrix4d::identity())));
        }

        void create_plane_object()
        {
            auto_release_ptr<MeshObject> mesh_object =
                MeshObjectFactory::create("plane", ParamArray());

            mesh_object->push_vertex(GVector3(0.0f, -0.5f, -0.5f));
            mesh_object->push_vertex(GVector3(0.0f, +0.5f, -0.5f));
            mesh_object->push_vertex(GVector3(0.0f, +0.5f, +0.5f));
            mesh_object->push_vertex(GVector3(0.0f, -0.5f, +0.5f));
            
            mesh_object->push_vertex_normal(GVector3(1.0f, 0.0f, 0.0f));

            mesh_object->push_triangle(Triangle(0, 1, 2, 0, 0, 0, 0));
            mesh_object->push_triangle(Triangle(2, 3, 0, 0, 0, 0, 0));

            auto_release_ptr<Object> object(mesh_object.release());
            m_scene.assemblies().get("assembly")->objects().insert(object);
        }

        void create_plane_object_instance(const char* name, const Vector3d& position, const size_t material_index)
        {
            const Assembly* assembly = m_scene.assemblies().get("assembly");
            const size_t object_index = assembly->objects().get_index("plane");
            const Object* object = assembly->objects().get(object_index);
            
            MaterialIndexArray material_indices;
            material_indices.push_back(material_index);

            assembly->object_instances().insert(
                ObjectInstanceFactory::create(
                    name,
                    *object,
                    object_index,
                    Transformd(Matrix4d::translation(position)),
                    material_indices));
        }

        void create_color(const char* name, const Color4f& color)
        {
            ParamArray params;
            params.insert("color_space", "linear_rgb");

            const ColorValueArray color_values(3, &color[0]);
            const ColorValueArray alpha_values(1, &color[3]);

            m_scene.assemblies().get("assembly")->colors().insert(
                ColorEntityFactory::create(name, params, color_values, alpha_values));
        }

        void create_constant_surface_shader(const char* surface_shader_name, const char* color_name)
        {
            ParamArray params;
            params.insert("color", color_name);

            ConstantSurfaceShaderFactory factory;
            auto_release_ptr<SurfaceShader> surface_shader(factory.create(surface_shader_name, params));
            m_scene.assemblies().get("assembly")->surface_shaders().insert(surface_shader);
        }

        size_t create_material(const char* material_name, const char* surface_shader_name)
        {
            ParamArray params;
            params.insert("surface_shader", surface_shader_name);

            auto_release_ptr<Material> material(MaterialFactory::create(material_name, params));
            return m_scene.assemblies().get("assembly")->materials().insert(material);
        }

        void create_opaque_material()
        {
            create_color("opaque_color", Color4f(1.0f, 1.0f, 1.0f, 1.0f));
            create_constant_surface_shader("opaque_surface_shader", "opaque_color");
            m_opaque_material_index = create_material("opaque_material", "opaque_surface_shader");
        }

        void create_transparent_material()
        {
            create_color("transparent_color", Color4f(1.0f, 1.0f, 1.0f, 0.5f));
            create_constant_surface_shader("transparent_surface_shader", "transparent_color");
            m_transparent_material_index = create_material("transparent_material", "transparent_surface_shader");
        }
    };

    template <typename Base>
    struct Fixture
      : public Base
    {
        TraceContext        m_trace_context;
        Intersector         m_intersector;
        TextureCache        m_texture_cache;
        MersenneTwister     m_rng;
        SamplingContext     m_sampling_context;
        Tracer              m_tracer;

        Fixture()
          : m_trace_context(m_scene)
          , m_intersector(m_trace_context)
          , m_texture_cache(m_scene, 1024 * 16)
          , m_sampling_context(m_rng, 0, 0, 0)
          , m_tracer(m_intersector, m_texture_cache, m_sampling_context)
        {
            InputBinder input_binder;
            input_binder.bind(m_scene);
        }
    };

    struct EmptyScene
      : public SceneBase
    {
    };

    TEST_CASE_F(Trace_GivenOriginAndDirection_NoOccluder, Fixture<EmptyScene>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace(
                Vector3d(0.0),
                Vector3d(1.0, 0.0, 0.0),
                transmission);

        EXPECT_FALSE(shading_point.hit());
        EXPECT_EQ(1.0, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenOriginAndTarget_NoOccluder, Fixture<EmptyScene>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                transmission);

        EXPECT_FALSE(shading_point.hit());
        EXPECT_EQ(1.0, transmission);
    }

    struct SceneWithSingleOpaqueOccluder
      : public SceneBase
    {
        SceneWithSingleOpaqueOccluder()
        {
            create_plane_object_instance("plane_inst", Vector3d(2.0, 0.0, 0.0), m_opaque_material_index);
        }
    };

    TEST_CASE_F(Trace_GivenOriginAndDirection_SingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluder>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(1.0, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenOriginAndTarget_SingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluder>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(1.0, transmission);
    }

    struct SceneWithSingleTransparentOccluder
      : public SceneBase
    {
        SceneWithSingleTransparentOccluder()
        {
            create_plane_object_instance("plane_inst", Vector3d(2.0, 0.0, 0.0), m_transparent_material_index);
        }
    };

    TEST_CASE_F(Trace_GivenOriginAndDirection_SingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluder>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                transmission);

        ASSERT_FALSE(shading_point.hit());
        EXPECT_FEQ(0.5, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenOriginAndTarget_SingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluder>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                transmission);

        ASSERT_FALSE(shading_point.hit());
        EXPECT_FEQ(0.5, transmission);
    }

    struct SceneWithTransparentThenOpaqueOccluders
      : public SceneBase
    {
        SceneWithTransparentThenOpaqueOccluders()
        {
            create_plane_object_instance("plane_inst1", Vector3d(2.0, 0.0, 0.0), m_transparent_material_index);
            create_plane_object_instance("plane_inst2", Vector3d(4.0, 0.0, 0.0), m_opaque_material_index);
        }
    };

    TEST_CASE_F(Trace_GivenOriginAndDirection_TransparentThenOpaqueOccluders, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(4.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_FEQ(0.5, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenOriginAndTarget_TransparentThenOpaqueOccluders_TargetPastOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(4.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_FEQ(0.5, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenOriginAndTarget_TransparentThenOpaqueOccluders_TargetOnOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        double transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(4.0, 0.0, 0.0),
                transmission);

        ASSERT_FALSE(shading_point.hit());
        EXPECT_FEQ(0.5, transmission);
    }
}
