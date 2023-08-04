
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/camera/pinholecamera.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/material/genericmaterial.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/modeling/surfaceshader/constantsurfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/iostreamop.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/texture.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <memory>

// Forward declarations.
namespace renderer  { class TextureStore; }

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Kernel_Lighting_Tracer)
{
    struct SceneBase
      : public TestSceneBase
    {
        Assembly*           m_assembly;
        AssemblyInstance*   m_assembly_instance;

        SceneBase()
        {
            m_scene.cameras().insert(
                PinholeCameraFactory().create(
                    "camera",
                    ParamArray()
                        .insert("film_width", "0.025")
                        .insert("film_height", "0.025")
                        .insert("focal_length", "0.035")));

            m_project.set_frame(
                FrameFactory::create(
                    "frame",
                    ParamArray()
                        .insert("resolution", "512 512")
                        .insert("camera", "camera")));

            m_scene.assemblies().insert(
                AssemblyFactory().create("assembly", ParamArray()));
            m_assembly = m_scene.assemblies().get_by_name("assembly");

            m_scene.assembly_instances().insert(
                AssemblyInstanceFactory::create(
                    "assembly_inst",
                    ParamArray(),
                    "assembly"));
            m_assembly_instance = m_scene.assembly_instances().get_by_name("assembly_inst");

            create_color("white", Color4f(1.0f));
            create_constant_surface_shader("constant_white_surface_shader", "white");
            create_material("opaque_material", "constant_white_surface_shader", 1.0f);
            create_material("transparent_material", "constant_white_surface_shader", 0.5f);

            create_plane_object();
        }

        void create_color(const char* name, const Color4f& color)
        {
            ParamArray params;
            params.insert("color_space", "linear_rgb");

            const ColorValueArray color_values(3, &color[0]);
            const ColorValueArray alpha_values(1, &color[3]);

            m_assembly->colors().insert(
                ColorEntityFactory::create(name, params, color_values, alpha_values));
        }

        void create_constant_surface_shader(const char* surface_shader_name, const char* color_name)
        {
            ParamArray params;
            params.insert("color", color_name);

            ConstantSurfaceShaderFactory factory;
            auto_release_ptr<SurfaceShader> surface_shader(factory.create(surface_shader_name, params));
            m_assembly->surface_shaders().insert(surface_shader);
        }

        void create_material(const char* material_name, const char* surface_shader_name, const float alpha)
        {
            ParamArray params;
            params.insert("surface_shader", surface_shader_name);
            params.insert("alpha_map", alpha);

            m_assembly->materials().insert(
                GenericMaterialFactory().create(material_name, params));
        }

        void create_plane_object()
        {
            auto_release_ptr<MeshObject> mesh_object(
                MeshObjectFactory().create("plane", ParamArray()));

            mesh_object->push_vertex(GVector3(0.0f, -0.5f, -0.5f));
            mesh_object->push_vertex(GVector3(0.0f, +0.5f, -0.5f));
            mesh_object->push_vertex(GVector3(0.0f, +0.5f, +0.5f));
            mesh_object->push_vertex(GVector3(0.0f, -0.5f, +0.5f));

            mesh_object->push_vertex_normal(GVector3(-1.0f, 0.0f, 0.0f));

            mesh_object->push_triangle(Triangle(0, 1, 2, 0, 0, 0, 0));
            mesh_object->push_triangle(Triangle(2, 3, 0, 0, 0, 0, 0));

            mesh_object->push_material_slot("material");

            auto_release_ptr<Object> object(mesh_object.release());
            m_assembly->objects().insert(object);
        }

        void create_plane_object_instance(
            const char*             name,
            const Vector3d&         position,
            const char*             material_name,
            const ParamArray&       params = ParamArray())
        {
            StringDictionary material_mappings;
            material_mappings.insert("material", material_name);

            m_assembly->object_instances().insert(
                ObjectInstanceFactory::create(
                    name,
                    params,
                    "plane",
                    Transformd::from_local_to_parent(
                        Matrix4d::make_translation(position)),
                    material_mappings,
                    material_mappings));
        }
    };

    template <typename ParamFixtureBaseClass, bool ParamUseEmbree>
    struct FixtureParams
    {
        typedef ParamFixtureBaseClass FixtureBaseClass;

        static bool UseEmbree()
        {
            return ParamUseEmbree;
        }
    };

    template <typename FixtureParams>
    struct Fixture
      : public StaticTestSceneContext<typename FixtureParams::FixtureBaseClass>
    {
        TraceContext                            m_trace_context;
        TextureStore&                           m_texture_store;
        TextureCache                            m_texture_cache;
        Intersector                             m_intersector;
        std::shared_ptr<OIIOTextureSystem>      m_texture_system;
        RendererServices                        m_renderer_services;
        std::shared_ptr<OSLShadingSystem>       m_shading_system;
        Arena                                   m_arena;
        OSLShaderGroupExec                      m_shading_group_exec;
        ShadingContext                          m_shading_context;
        Tracer                                  m_tracer;

        Fixture()
          : m_trace_context(FixtureParams::FixtureBaseClass::m_project)
          , m_texture_store(FixtureParams::FixtureBaseClass::m_project.get_texture_store())
          , m_texture_cache(m_texture_store)
          , m_intersector(m_trace_context, m_texture_cache)
          , m_texture_system(
                OIIOTextureSystemFactory::create(),
                [](OIIOTextureSystem* object) { object->release(); })
          , m_renderer_services(
                FixtureParams::FixtureBaseClass::m_project,
                reinterpret_cast<OIIO::TextureSystem&>(*m_texture_system))
          , m_shading_system(
                OSLShadingSystemFactory::create(&m_renderer_services, m_texture_system.get()),
                [](OSLShadingSystem* object) { object->release(); })
          , m_shading_group_exec(*m_shading_system, m_arena)
          , m_tracer(FixtureParams::FixtureBaseClass::m_scene, m_intersector, m_shading_group_exec)
          , m_shading_context(
                m_intersector,
                m_tracer,
                m_texture_cache,
                *m_texture_system,
                m_shading_group_exec,
                m_arena,
                0)  // thread index
        {
#ifdef APPLESEED_WITH_EMBREE
            m_trace_context.set_use_embree(FixtureParams::UseEmbree());
#endif
            m_trace_context.update();
        }
    };

    struct EmptyScene
      : public SceneBase
    {
    };

    typedef FixtureParams<EmptyScene, false> EmptySceneParams;

    TEST_CASE_F(TraceFull_GivenNoOccluder, Fixture<EmptySceneParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        EXPECT_FALSE(shading_point.hit_surface());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceSimple_GivenNoOccluder, Fixture<EmptySceneParams>)
    {
        ShadingRay ray(
            Vector3d(0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        Spectrum transmission;
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_GivenNoOccluder, Fixture<EmptySceneParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        EXPECT_FALSE(shading_point.hit_surface());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_GivenNoOccluder, Fixture<EmptySceneParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

#ifdef APPLESEED_WITH_EMBREE

    typedef FixtureParams<EmptyScene, true> EmptySceneWithEmbreeParams;

    TEST_CASE_F(TraceFull_Embree_GivenNoOccluder, Fixture<EmptySceneWithEmbreeParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        EXPECT_FALSE(shading_point.hit_surface());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceSimple_Embree_GivenNoOccluder, Fixture<EmptySceneWithEmbreeParams>)
    {
        ShadingRay ray(
            Vector3d(0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        Spectrum transmission;
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_Embree_GivenNoOccluder, Fixture<EmptySceneWithEmbreeParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        EXPECT_FALSE(shading_point.hit_surface());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_Embree_GivenNoOccluder, Fixture<EmptySceneWithEmbreeParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }
#endif

    struct SceneWithSingleOpaqueOccluder
      : public SceneBase
    {
        SceneWithSingleOpaqueOccluder()
        {
            create_plane_object_instance("plane_inst", Vector3d(2.0, 0.0, 0.0), "opaque_material");
        }
    };

    typedef FixtureParams<SceneWithSingleOpaqueOccluder, false> SceneWithSingleOpaqueOccluderParams;

    TEST_CASE_F(TraceFull_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceSimple_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_EQ(Spectrum(0.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_EQ(Spectrum(0.0f), transmission);
    }

#ifdef APPLESEED_WITH_EMBREE

    typedef FixtureParams<SceneWithSingleOpaqueOccluder, false> SceneWithSingleOpaqueOccluderWithEmbreeParams;

    TEST_CASE_F(TraceFull_Embree_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceSimple_Embree_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_EQ(Spectrum(0.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_Embree_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_Embree_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_EQ(Spectrum(0.0f), transmission);
    }

#endif

    struct SceneWithSingleTransparentOccluder
      : public SceneBase
    {
        SceneWithSingleTransparentOccluder()
        {
            create_plane_object_instance("plane_inst", Vector3d(2.0, 0.0, 0.0), "transparent_material");
        }
    };

    typedef FixtureParams<SceneWithSingleTransparentOccluder, false> SceneWithSingleTransparentOccluderParams;

    TEST_CASE_F(TraceFull_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        ASSERT_FALSE(shading_point.hit_surface());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceSimple_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_FALSE(shading_point.hit_surface());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

#ifdef APPLESEED_WITH_EMBREE

    typedef FixtureParams<SceneWithSingleTransparentOccluder, true> SceneWithSingleTransparentOccluderWithEmbreeParams;

    TEST_CASE_F(TraceFull_Embree_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        ASSERT_FALSE(shading_point.hit_surface());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceSimple_Embree_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_Embree_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_FALSE(shading_point.hit_surface());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_Embree_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluderWithEmbreeParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

#endif

    struct SceneWithTransparentThenOpaqueOccluders
      : public SceneBase
    {
        SceneWithTransparentThenOpaqueOccluders()
        {
            create_plane_object_instance("plane_inst1", Vector3d(2.0, 0.0, 0.0), "transparent_material");
            create_plane_object_instance("plane_inst2", Vector3d(4.0, 0.0, 0.0), "opaque_material");
        }
    };

    typedef FixtureParams<SceneWithTransparentThenOpaqueOccluders, false> SceneWithTransparentThenOpaqueOccludersParams;

    TEST_CASE_F(TraceFull_GivenTransparentThenOpaqueOccluders, Fixture<SceneWithTransparentThenOpaqueOccludersParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3d(4.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceSimple_GivenTransparentThenOpaqueOccluders, Fixture<SceneWithTransparentThenOpaqueOccludersParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_FEQ(Spectrum(0.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_GivenTransparentThenOpaqueOccluders_GivenTargetPastOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3d(4.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_GivenTransparentThenOpaqueOccluders_GivenTargetPastOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_FEQ(Spectrum(0.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_GivenTransparentThenOpaqueOccluders_GivenTargetOnOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(4.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_FALSE(shading_point.hit_surface());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_GivenTransparentThenOpaqueOccluders_GivenTargetOnOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(4.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

#ifdef APPLESEED_WITH_EMBREE

    typedef FixtureParams<SceneWithTransparentThenOpaqueOccluders, true> SceneWithTransparentThenOpaqueOccludersWithEmbreeParams;

    TEST_CASE_F(TraceFull_Embree_GivenTransparentThenOpaqueOccluders, Fixture<SceneWithTransparentThenOpaqueOccludersWithEmbreeParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        const ShadingPoint& shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3f(4.0f, 0.0f, 0.0f), Vector3f(shading_point.get_point()));
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceSimple_Embree_GivenTransparentThenOpaqueOccluders, Fixture<SceneWithTransparentThenOpaqueOccludersWithEmbreeParams>)
    {
        Spectrum transmission;
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        m_tracer.trace_simple(
            m_shading_context,
            ray,
            transmission);

        EXPECT_FEQ(Spectrum(0.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_Embree_GivenTransparentThenOpaqueOccluders_GivenTargetPastOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersWithEmbreeParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit_surface());
        EXPECT_FEQ(Vector3f(4.0f, 0.0f, 0.0f), Vector3f(shading_point.get_point()));
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_Embree_GivenTransparentThenOpaqueOccluders_GivenTargetPastOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersWithEmbreeParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(5.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_FEQ(Spectrum(0.0f), transmission);
    }

    TEST_CASE_F(TraceBetweenFull_Embree_GivenTransparentThenOpaqueOccluders_GivenTargetOnOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersWithEmbreeParams>)
    {
        Spectrum transmission;
        const ShadingPoint& shading_point =
            m_tracer.trace_between_full(
                m_shading_context,
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(4.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_FALSE(shading_point.hit_surface());
        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

    TEST_CASE_F(TraceBetweenSimple_Embree_GivenTransparentThenOpaqueOccluders_GivenTargetOnOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccludersWithEmbreeParams>)
    {
        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(4.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0,
            transmission);

        EXPECT_FEQ(Spectrum(0.5f), transmission);
    }

#endif

    struct SceneWithTwoOpaqueOccluders
      : public SceneBase
    {
        SceneWithTwoOpaqueOccluders()
        {
            create_plane_object_instance("plane_inst1", Vector3d(2.0, 0.0, 0.0), "opaque_material");
            create_plane_object_instance("plane_inst2", Vector3d(4.0, 0.0, 0.0), "opaque_material");
        }
    };

    typedef FixtureParams<SceneWithTwoOpaqueOccluders, false> SceneWithTwoOpaqueOccludersParams;

    TEST_CASE_F(TraceBetweenSimple_ComputeVisibilityBetweenTwoOpaqueOccluders_ReturnsOne, Fixture<SceneWithTwoOpaqueOccludersParams>)
    {
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        Spectrum parent_transmission;
        const ShadingPoint& parent_shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                parent_transmission);

        ASSERT_TRUE(parent_shading_point.hit_surface());
        ASSERT_FEQ(2.0, parent_shading_point.get_distance());

        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            parent_shading_point,
            Vector3d(4.0, 0.0, 0.0),
            VisibilityFlags::ShadowRay,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

#ifdef APPLESEED_WITH_EMBREE

    typedef FixtureParams<SceneWithTwoOpaqueOccluders, true> SceneWithTwoOpaqueOccludersWithEmbreeParams;

    TEST_CASE_F(TraceBetweenSimple_Embree_ComputeVisibilityBetweenTwoOpaqueOccluders_ReturnsOne, Fixture<SceneWithTwoOpaqueOccludersWithEmbreeParams>)
    {
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        Spectrum parent_transmission;
        const ShadingPoint& parent_shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                parent_transmission);

        ASSERT_TRUE(parent_shading_point.hit_surface());
        ASSERT_FEQ(2.0f, static_cast<float>(parent_shading_point.get_distance()));

        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            parent_shading_point,
            Vector3d(4.0, 0.0, 0.0),
            VisibilityFlags::ShadowRay,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

#endif

    struct SceneWithTwoOpaqueOccludersAndScaledAssemblyInstance
      : public SceneWithTwoOpaqueOccluders
    {
        SceneWithTwoOpaqueOccludersAndScaledAssemblyInstance()
        {
            m_assembly_instance->transform_sequence().set_transform(
                0.0f,
                Transformd::from_local_to_parent(
                    Matrix4d::make_scaling(Vector3d(0.5))));
        }
    };

    typedef FixtureParams<SceneWithTwoOpaqueOccludersAndScaledAssemblyInstance, false> SceneWithTwoOpaqueOccludersAndScaledAssemblyInstanceParams;

    TEST_CASE_F(TraceBetweenSimple_ComputeVisibilityBetweenTwoOpaqueOccludersAndScaledAssemblyInstance_ReturnsOne, Fixture<SceneWithTwoOpaqueOccludersAndScaledAssemblyInstanceParams>)
    {
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        Spectrum parent_transmission;
        const ShadingPoint& parent_shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                parent_transmission);

        ASSERT_TRUE(parent_shading_point.hit_surface());
        ASSERT_FEQ(1.0, parent_shading_point.get_distance());

        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            parent_shading_point,
            Vector3d(2.0, 0.0, 0.0),
            VisibilityFlags::ShadowRay,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

#ifdef APPLESEED_WITH_EMBREE

    typedef FixtureParams<SceneWithTwoOpaqueOccludersAndScaledAssemblyInstance, true> SceneWithTwoOpaqueOccludersAndScaledAssemblyInstanceWithEmbreeParams;

    TEST_CASE_F(TraceBetweenSimple_Embree_ComputeVisibilityBetweenTwoOpaqueOccludersAndScaledAssemblyInstance_ReturnsOne, Fixture<SceneWithTwoOpaqueOccludersAndScaledAssemblyInstanceWithEmbreeParams>)
    {
        ShadingRay ray(
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0),
            ShadingRay::Time(),
            VisibilityFlags::ShadowRay,
            0);
        Spectrum parent_transmission;
        const ShadingPoint& parent_shading_point =
            m_tracer.trace_full(
                m_shading_context,
                ray,
                parent_transmission);

        ASSERT_TRUE(parent_shading_point.hit_surface());
        ASSERT_FEQ(1.0f, static_cast<float>(parent_shading_point.get_distance()));

        Spectrum transmission;
        m_tracer.trace_between_simple(
            m_shading_context,
            parent_shading_point,
            Vector3d(2.0, 0.0, 0.0),
            VisibilityFlags::ShadowRay,
            transmission);

        EXPECT_EQ(Spectrum(1.0f), transmission);
    }

#endif

}
