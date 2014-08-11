
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/api/bsdf.h"
#include "renderer/api/camera.h"
#include "renderer/api/color.h"
#include "renderer/api/environment.h"
#include "renderer/api/environmentedf.h"
#include "renderer/api/frame.h"
#include "renderer/api/light.h"
#include "renderer/api/log.h"
#include "renderer/api/material.h"
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/surfaceshader.h"
#include "renderer/api/texture.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/math/matrix.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/log/consolelogtarget.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

auto_release_ptr<Project> build_project()
{
    GenericImageFileReader reader;
    auto_ptr<Image> image(reader.read("data/heightfield.png"));
    const size_t image_width = image->properties().m_canvas_width;
    const size_t image_height = image->properties().m_canvas_height;

    auto_release_ptr<Project> project(ProjectFactory::create("heightfield"));
    project->search_paths().push_back("data");
    project->add_default_configurations();

    auto_release_ptr<Scene> scene(SceneFactory::create());

    auto_release_ptr<Assembly> assembly(
        AssemblyFactory::create(
            "assembly",
            ParamArray()));

    //------------------------------------------------------------------------
    // Material
    //------------------------------------------------------------------------

    assembly->textures().insert(
        DiskTexture2dFactory().create(
            "cubes_texture",
            ParamArray()
                .insert("filename", "heightfield.png")
                .insert("color_space", "srgb"),
            project->search_paths()));

    assembly->texture_instances().insert(
        TextureInstanceFactory::create(
            "cubes_texture_instance",
            ParamArray()
                .insert("filtering_mode", "nearest")
                .insert("addressing_mode", "wrap"),
            "cubes_texture",
            Transformd::identity()));

    assembly->bsdfs().insert(
        DisneyBRDFFactory().create(
            "cubes_brdf",
            ParamArray()
                .insert("anisotropic", 0.0)
                .insert("base_color", "cubes_texture_instance")
                .insert("clearcoat", 1.0)
                .insert("clearcoat_gloss", 0.9)
                .insert("metallic", 0.0)
                .insert("roughness", 0.3)
                .insert("sheen", 0.0)
                .insert("sheen_tint", 0.0)
                .insert("specular", 0.9)
                .insert("specular_tint", 1.0)
                .insert("subsurface", 1.0)));

    assembly->surface_shaders().insert(
        PhysicalSurfaceShaderFactory().create(
            "physical_surface_shader",
            ParamArray()));

    assembly->materials().insert(
        GenericMaterialFactory().create(
            "cubes_material",
            ParamArray()
                .insert("surface_shader", "physical_surface_shader")
                .insert("bsdf", "cubes_brdf")));

    //------------------------------------------------------------------------
    // Geometry
    //------------------------------------------------------------------------

    static const GVector3 CubeVertices[8] =
    {
        GVector3(-0.5f, 0.0f, -0.5f),
        GVector3( 0.5f, 0.0f, -0.5f),
        GVector3(-0.5f, 1.0f, -0.5f),
        GVector3( 0.5f, 1.0f, -0.5f),
        GVector3(-0.5f, 0.0f,  0.5f),
        GVector3( 0.5f, 0.0f,  0.5f),
        GVector3(-0.5f, 1.0f,  0.5f),
        GVector3( 0.5f, 1.0f,  0.5f)
    };

    static const size_t CubeTriangles[12 * 3] =
    {
        0, 2, 3,
        3, 1, 0,
        5, 7, 6,
        6, 4, 5,
        0, 4, 6,
        6, 2, 0,
        3, 7, 5,
        5, 1, 3,
        1, 5, 4,
        4, 0, 1,
        2, 6, 7,
        7, 3, 2
    };

    static const GVector3 CubeNormals[6] =
    {
        GVector3( 0.0f,  0.0f, -1.0f),
        GVector3( 0.0f,  0.0f,  1.0f),
        GVector3(-1.0f,  0.0f,  0.0f),
        GVector3( 1.0f,  0.0f,  0.0f),
        GVector3( 0.0f, -1.0f,  0.0f),
        GVector3( 0.0f,  1.0f,  0.0f)
    };

    const double ValueMultiplier = 1.0;
    const double CubeSizeX = 0.002;
    const double CubeScaleY = 0.02;
    const double CubeSizeZ = 0.002;
    const double CubeMinSizeY = 0.0001;
    const double ScaleVariation = 0.4;
    const double OrientationVariation = 0.05;
    const double ShiftX = 0.1 * CubeSizeX;
    const double ShiftZ = 0.1 * CubeSizeZ;

    const double total_size_x = CubeSizeX * image_width;
    const double total_size_z = CubeSizeZ * image_height;

    auto_release_ptr<MeshObject> mesh(
        MeshObjectFactory::create("cubes", ParamArray()));

    // Reserve memory.
    const size_t cube_count = image_width * image_height;
    mesh->reserve_vertices(8 * cube_count);
    mesh->reserve_vertex_normals(6 * cube_count);
    mesh->reserve_triangles(12 * cube_count);

    MersenneTwister rng;

    for (size_t iy = 0; iy < image_height; ++iy)
    {
        for (size_t ix = 0; ix < image_width; ++ix)
        {
            const double fx = (ix + 0.5) / image_width;
            const double fz = (iy + 0.5) / image_height;

            // Read pixel color.
            Color3b color;
            image->get_pixel(ix, iy, color);

            // Compute pixel value.
            const Color3f reflectance = Color3f(color) * (1.0f / 255);
            const double value = pow(reflectance.r, 3.0) * ValueMultiplier;

            // Compute scaling.
            const double scale_multiplier = 1.0 + ScaleVariation * value;
            const double scale_x = CubeSizeX * scale_multiplier;
            const double scale_z = CubeSizeZ * scale_multiplier;
            const double scale_y = max(CubeScaleY * value, CubeMinSizeY);
            const Vector3d scaling(scale_x, scale_y, scale_z);

            // Compute rotation.
            const double rotation = OrientationVariation * rand2(rng, -Pi, Pi);

            // Compute translation.
            const double translate_x = (fx - 0.5) * total_size_x;
            const double translate_z = (fz - 0.5) * total_size_z;
            const double shift_x = ShiftX * rand1(rng, -0.5, 0.5);
            const double shift_z = ShiftZ * rand1(rng, -0.5, 0.5);
            const Vector3d translation(translate_x + shift_x, 0.0, translate_z + shift_z);

            // Compute cube transform.
            const Transformd transform =
                Transformd::from_local_to_parent(
                      Matrix4d::translation(translation)
                    * Matrix4d::rotation_y(rotation)
                    * Matrix4d::scaling(scaling));

            // Push vertices.
            size_t vertex_indices[8];
            for (size_t i = 0; i < 8; ++i)
            {
                vertex_indices[i] =
                    mesh->push_vertex(
                        transform.point_to_parent(CubeVertices[i]));
            }

            // Push normals.
            size_t normal_indices[6];
            for (size_t i = 0; i < 6; ++i)
            {
                normal_indices[i] =
                    mesh->push_vertex_normal(
                        normalize(transform.normal_to_parent(CubeNormals[i])));
            }

            // Push texture coordinates.
            const size_t tex_coords_index =
                mesh->push_tex_coords(
                    GVector2(static_cast<float>(fx), static_cast<float>(1.0 - fz)));

            // Push triangles.
            for (size_t i = 0; i < 12; ++i)
            {
                Triangle triangle;
                triangle.m_v0 = static_cast<uint32>(vertex_indices[CubeTriangles[i * 3 + 0]]);
                triangle.m_v1 = static_cast<uint32>(vertex_indices[CubeTriangles[i * 3 + 1]]);
                triangle.m_v2 = static_cast<uint32>(vertex_indices[CubeTriangles[i * 3 + 2]]);
                triangle.m_n0 = triangle.m_n1 = triangle.m_n2 = static_cast<uint32>(normal_indices[i / 2]);
                triangle.m_a0 = triangle.m_a1 = triangle.m_a2 = static_cast<uint32>(tex_coords_index);
                triangle.m_pa = 0;
                mesh->push_triangle(triangle);
            }
        }

        RENDERER_LOG_INFO("%s completed...", pretty_percent(iy + 1, image_height).c_str());
    }

    assembly->objects().insert(auto_release_ptr<Object>(mesh));

    assembly->object_instances().insert(
        ObjectInstanceFactory::create(
            "cubes_instance",
            ParamArray(),
            "cubes",
            Transformd::identity(),
            StringDictionary()
                .insert("default", "cubes_material")));

    //------------------------------------------------------------------------
    // Light
    //------------------------------------------------------------------------

    static const float LightRadiance[] = { 1.0f, 1.0f, 1.0f };
    assembly->colors().insert(
        ColorEntityFactory::create(
            "light_radiance",
            ParamArray()
                .insert("color_space", "srgb")
                .insert("multiplier", "6.0"),
            ColorValueArray(3, LightRadiance)));

    auto_release_ptr<Light> light(
        DirectionalLightFactory().create(
            "sun_light",
            ParamArray()
                .insert("radiance", "light_radiance")));
    light->set_transform(
        Transformd::from_local_to_parent(
            Matrix4d::rotation_x(deg_to_rad(-30.0))));
    assembly->lights().insert(light);

    //------------------------------------------------------------------------
    // Assembly instance
    //------------------------------------------------------------------------

    auto_release_ptr<AssemblyInstance> assembly_instance(
        AssemblyInstanceFactory::create(
            "assembly_inst",
            ParamArray(),
            "assembly"));
    assembly_instance
        ->transform_sequence()
            .set_transform(
                0.0,
                Transformd::identity());
    scene->assembly_instances().insert(assembly_instance);

    scene->assemblies().insert(assembly);

    //------------------------------------------------------------------------
    // Environment
    //------------------------------------------------------------------------

    scene->environment_edfs().insert(
        HosekEnvironmentEDFFactory().create(
            "sky_edf",
            ParamArray()
                .insert("sun_theta", 30.0)
                .insert("sun_phi", 0.0)
                .insert("turbidity", 4.0)
                .insert("turbidity_multiplier", 1.0)
                .insert("luminance_multiplier", 2.0)));

    scene->set_environment(
        EnvironmentFactory::create(
            "sky",
            ParamArray()
                .insert("environment_edf", "sky_edf")));

    //------------------------------------------------------------------------
    // Camera
    //------------------------------------------------------------------------

    auto_release_ptr<Camera> camera(
        ThinLensCameraFactory().create(
            "camera",
            ParamArray()
                .insert("film_dimensions", "0.025 0.014")
                .insert("horizontal_fov", "70.0")
                .insert("f_stop", "2.0")
                .remove_path("focal_distance")
                .insert("autofocus_target", "0.5 0.5")
                .insert("controller_target", "0.00222523 -0.004497 0.00937141")));

    const double CameraMatrix[16] =
    {
        0.898911352044089, -0.291992464000737, 0.326647795236774, 0.051430150409342,
        0.000000000000000, 0.745548999631107, 0.666450815251250, 0.095894497845129,
        -0.438130552650996, -0.599080203408387, 0.670182459273516, 0.110325032485670,
        0.000000000000000, 0.000000000000000, 0.000000000000000, 1.000000000000000
    };

    camera->transform_sequence().set_transform(
        0.0,
        Transformd::from_local_to_parent(Matrix4d(CameraMatrix)));

    scene->set_camera(camera);

    //------------------------------------------------------------------------
    // Frame
    //------------------------------------------------------------------------

    project->set_frame(
        FrameFactory::create(
            "beauty",
            ParamArray()
                .insert("camera", scene->get_camera()->get_name())
                .insert("resolution", "1280 720")
                .insert("color_space", "srgb")));

    project->set_scene(scene);

    return project;
}

int main()
{
    // Create a log target that outputs to stderr, and binds it to the renderer's global logger.
    auto_ptr<ILogTarget> log_target(create_console_log_target(stderr));
    global_logger().add_target(log_target.get());

    auto_release_ptr<Project> project(build_project());
    ProjectFileWriter::write(project.ref(), "output/heightfield.appleseed");

    return 0;
}
