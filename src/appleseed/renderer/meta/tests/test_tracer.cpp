
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
#include "renderer/kernel/lighting/tracer.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#endif
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
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
#include "renderer/utility/paramarray.h"
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// OpenImageIO headers.
#ifdef APPLESEED_WITH_OIIO
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
END_OIIO_INCLUDES
#endif

// Boost headers.
#include "boost/bind.hpp"
#include "boost/shared_ptr.hpp"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace boost;
using namespace std;

TEST_SUITE(Renderer_Kernel_Lighting_Tracer)
{
    struct SceneBase
    {
        auto_release_ptr<Project>       m_project;
        Scene*                          m_scene;
        Assembly*                       m_assembly;
        AssemblyInstance*               m_assembly_instance;

        SceneBase()
          : m_project(ProjectFactory::create("project"))
        {
            m_project->set_scene(SceneFactory::create());
            m_scene = m_project->get_scene();
            m_scene->cameras().insert(
                PinholeCameraFactory().create(
                    "camera",
                    ParamArray()
                        .insert("film_width", "0.025")
                        .insert("film_height", "0.025")
                        .insert("focal_length", "0.035")));

            m_project->set_frame(
                FrameFactory::create(
                    "frame",
                    ParamArray()
                        .insert("resolution", "512 512")
                        .insert("camera", "camera")));

            m_scene->assemblies().insert(
                AssemblyFactory().create("assembly", ParamArray()));
            m_assembly = m_scene->assemblies().get_by_name("assembly");

            m_scene->assembly_instances().insert(
                AssemblyInstanceFactory::create(
                    "assembly_inst",
                    ParamArray(),
                    "assembly"));
            m_assembly_instance = m_scene->assembly_instances().get_by_name("assembly_inst");

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
            auto_release_ptr<MeshObject> mesh_object =
                MeshObjectFactory::create("plane", ParamArray());

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

    template <typename Base>
    struct Fixture
      : public BindInputs<Base>
    {
        TraceContext                            m_trace_context;
        TextureStore                            m_texture_store;
        TextureCache                            m_texture_cache;
        Intersector                             m_intersector;
#ifdef APPLESEED_WITH_OIIO
        boost::shared_ptr<OIIO::TextureSystem>  m_texture_system;
#endif
#ifdef APPLESEED_WITH_OSL
        boost::shared_ptr<RendererServices>     m_renderer_services;
        boost::shared_ptr<OSL::ShadingSystem>   m_shading_system;
        boost::shared_ptr<OSLShaderGroupExec>   m_shading_group_exec;
#endif
        OnFrameBeginRecorder                    m_recorder;

        Fixture()
          : m_trace_context(*Base::m_scene)
          , m_texture_store(*Base::m_scene)
          , m_texture_cache(m_texture_store)
          , m_intersector(m_trace_context, m_texture_cache)
        {
#ifdef APPLESEED_WITH_OIIO
            m_texture_system.reset(
                OIIO::TextureSystem::create(),
                boost::bind(&OIIO::TextureSystem::destroy, _1));
#endif
#ifdef APPLESEED_WITH_OSL
            m_renderer_services.reset(
                new RendererServices(*Base::m_project, *m_texture_system));
            m_shading_system.reset(
                new OSL::ShadingSystem(
                    m_renderer_services.get(),
                    m_texture_system.get()));
            m_shading_group_exec.reset(new OSLShaderGroupExec(*m_shading_system));
#endif

            Base::m_scene->on_frame_begin(Base::m_project.ref(), 0, m_recorder);
        }

        ~Fixture()
        {
            m_recorder.on_frame_end(Base::m_project.ref());
        }
    };

    struct EmptyScene
      : public SceneBase
    {
    };

    TEST_CASE_F(Trace_GivenNoOccluder, Fixture<EmptyScene>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace(
                Vector3d(0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        EXPECT_FALSE(shading_point.hit());
        EXPECT_EQ(1.0f, transmission);
    }

    TEST_CASE_F(Trace_QuickVariant_GivenNoOccluder, Fixture<EmptyScene>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace(
                Vector3d(0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_EQ(1.0f, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenNoOccluder, Fixture<EmptyScene>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        EXPECT_FALSE(shading_point.hit());
        EXPECT_EQ(1.0f, transmission);
    }

    TEST_CASE_F(TraceBetween_QuickVariant_GivenNoOccluder, Fixture<EmptyScene>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_EQ(1.0f, transmission);
    }

    struct SceneWithSingleOpaqueOccluder
      : public SceneBase
    {
        SceneWithSingleOpaqueOccluder()
        {
            create_plane_object_instance("plane_inst", Vector3d(2.0, 0.0, 0.0), "opaque_material");
        }
    };

    TEST_CASE_F(Trace_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(1.0f, transmission);
    }

    TEST_CASE_F(Trace_QuickVariant_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_EQ(0.0f, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(2.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_EQ(1.0f, transmission);
    }

    TEST_CASE_F(TraceBetween_QuickVariant_GivenSingleOpaqueOccluder, Fixture<SceneWithSingleOpaqueOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_EQ(0.0f, transmission);
    }

    struct SceneWithSingleTransparentOccluder
      : public SceneBase
    {
        SceneWithSingleTransparentOccluder()
        {
            create_plane_object_instance("plane_inst", Vector3d(2.0, 0.0, 0.0), "transparent_material");
        }
    };

    TEST_CASE_F(Trace_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_FALSE(shading_point.hit());
        EXPECT_FEQ(0.5f, transmission);
    }

    TEST_CASE_F(Trace_QuickVariant_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_FEQ(0.5f, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_FALSE(shading_point.hit());
        EXPECT_FEQ(0.5f, transmission);
    }

    TEST_CASE_F(TraceBetween_QuickVariant_GivenSingleTransparentOccluder, Fixture<SceneWithSingleTransparentOccluder>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_FEQ(0.5f, transmission);
    }

    struct SceneWithTransparentThenOpaqueOccluders
      : public SceneBase
    {
        SceneWithTransparentThenOpaqueOccluders()
        {
            create_plane_object_instance("plane_inst1", Vector3d(2.0, 0.0, 0.0), "transparent_material");
            create_plane_object_instance("plane_inst2", Vector3d(4.0, 0.0, 0.0), "opaque_material");
        }
    };

    TEST_CASE_F(Trace_GivenTransparentThenOpaqueOccluders, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(4.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_FEQ(0.5f, transmission);
    }

    TEST_CASE_F(Trace_QuickVariant_GivenTransparentThenOpaqueOccluders, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_FEQ(0.0f, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenTransparentThenOpaqueOccluders_GivenTargetPastOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_TRUE(shading_point.hit());
        EXPECT_FEQ(Vector3d(4.0, 0.0, 0.0), shading_point.get_point());
        EXPECT_FEQ(0.5f, transmission);
    }

    TEST_CASE_F(TraceBetween_QuickVariant_GivenTransparentThenOpaqueOccluders_GivenTargetPastOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(5.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_FEQ(0.0f, transmission);
    }

    TEST_CASE_F(TraceBetween_GivenTransparentThenOpaqueOccluders_GivenTargetOnOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float transmission;
        const ShadingPoint& shading_point =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(4.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                transmission);

        ASSERT_FALSE(shading_point.hit());
        EXPECT_FEQ(0.5f, transmission);
    }

    TEST_CASE_F(TraceBetween_QuickVariant_GivenTransparentThenOpaqueOccluders_GivenTargetOnOpaqueOccluder, Fixture<SceneWithTransparentThenOpaqueOccluders>)
    {
        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        const float transmission =
            tracer.trace_between(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(4.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0);

        EXPECT_FEQ(0.5f, transmission);
    }

    struct SceneWithTwoOpaqueOccluders
      : public SceneBase
    {
        SceneWithTwoOpaqueOccluders()
        {
            create_plane_object_instance("plane_inst1", Vector3d(2.0, 0.0, 0.0), "opaque_material");
            create_plane_object_instance("plane_inst2", Vector3d(4.0, 0.0, 0.0), "opaque_material");
        }
    };

    TEST_CASE_F(TraceBetween_ComputeVisibilityBetweenTwoOpaqueOccluders_ReturnsOne, Fixture<SceneWithTwoOpaqueOccluders>)
    {
        Tracer parent_tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float parent_transmission;
        const ShadingPoint& parent_shading_point =
            parent_tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                parent_transmission);

        ASSERT_TRUE(parent_shading_point.hit());
        ASSERT_FEQ(2.0, parent_shading_point.get_distance());

        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );
        const float transmission =
            tracer.trace_between(
                parent_shading_point,
                Vector3d(4.0, 0.0, 0.0),
                VisibilityFlags::ShadowRay);

        EXPECT_EQ(1.0f, transmission);
    }

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

    TEST_CASE_F(TraceBetween_ComputeVisibilityBetweenTwoOpaqueOccludersAndScaledAssemblyInstance_ReturnsOne, Fixture<SceneWithTwoOpaqueOccludersAndScaledAssemblyInstance>)
    {
        Tracer parent_tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );

        float parent_transmission;
        const ShadingPoint& parent_shading_point =
            parent_tracer.trace(
                Vector3d(0.0, 0.0, 0.0),
                Vector3d(1.0, 0.0, 0.0),
                ShadingRay::Time(),
                VisibilityFlags::ShadowRay,
                0,
                parent_transmission);

        ASSERT_TRUE(parent_shading_point.hit());
        ASSERT_FEQ(1.0, parent_shading_point.get_distance());

        Tracer tracer(
            *m_scene,
            m_intersector,
            m_texture_cache
#ifdef APPLESEED_WITH_OSL
            , *m_shading_group_exec
#endif
            );
        const float transmission =
            tracer.trace_between(
                parent_shading_point,
                Vector3d(2.0, 0.0, 0.0),
                VisibilityFlags::ShadowRay);

        EXPECT_EQ(1.0f, transmission);
    }
}
