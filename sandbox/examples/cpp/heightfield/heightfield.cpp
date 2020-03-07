
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/log/consolelogtarget.h"
#include "foundation/math/matrix.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <iomanip>
#include <memory>
#include <sstream>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

class ProjectBuilder
{
  public:
    auto_release_ptr<Project> build_project()
    {
        GenericImageFileReader reader;
        unique_ptr<Image> image(reader.read("data/heightfield.png"));
        const size_t image_width = image->properties().m_canvas_width;
        const size_t image_height = image->properties().m_canvas_height;

        auto_release_ptr<Project> project(ProjectFactory::create("heightfield"));
        project->add_default_configurations();
        project->search_paths().push_back_explicit_path("data");

        auto_release_ptr<Scene> scene(SceneFactory::create());
        auto_release_ptr<Assembly> assembly(AssemblyFactory().create("assembly"));

        //------------------------------------------------------------------------
        // Geometry
        //------------------------------------------------------------------------

        initialize_assembly(
            *project,
            *assembly,
            image_width,
            image_height);

        const double ValueMultiplier = 1.0;
        const double CubeSizeX = 0.002;
        const double CubeScaleY = 0.02;
        const double CubeSizeZ = 0.002;
        const double CubeMinSizeY = 0.0001;
        const double ScaleVariation = 0.6;
        const double OrientationVariation = 0.2;
        const double ShiftX = 0.1 * CubeSizeX;
        const double ShiftZ = 0.1 * CubeSizeZ;

        const double total_size_x = CubeSizeX * image_width;
        const double total_size_z = CubeSizeZ * image_height;

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
                const double rotation = OrientationVariation * rand2(rng, -Pi<double>(), Pi<double>());

                // Compute translation.
                const double translate_x = (fx - 0.5) * total_size_x;
                const double translate_z = (fz - 0.5) * total_size_z;
                const double shift_x = ShiftX * rand1(rng, -0.5, 0.5);
                const double shift_z = ShiftZ * rand1(rng, -0.5, 0.5);
                const Vector3d translation(translate_x + shift_x, 0.0, translate_z + shift_z);

                // Compute cube transform.
                const Transformd transform =
                    Transformd::from_local_to_parent(
                          Matrix4d::make_translation(translation)
                        * Matrix4d::make_rotation_y(rotation)
                        * Matrix4d::make_scaling(scaling));

                // Add cube to assembly.
                add_cube(*assembly, ix, iy, fx, fz, color, transform);
            }

            RENDERER_LOG_INFO("%s completed...", pretty_percent(iy + 1, image_height).c_str());
        }

        finalize_assembly(*assembly);

        //------------------------------------------------------------------------
        // Light
        //------------------------------------------------------------------------

        static const float LightIrradiance[] = { 1.0f, 1.0f, 1.0f };
        assembly->colors().insert(
            ColorEntityFactory::create(
                "light_irradiance",
                ParamArray()
                    .insert("color_space", "srgb")
                    .insert("multiplier", "6.0"),
                ColorValueArray(3, LightIrradiance)));

        auto_release_ptr<Light> light(
            DirectionalLightFactory().create(
                "sun_light",
                ParamArray()
                    .insert("irradiance", "light_irradiance")));
        light->set_transform(
            Transformd::from_local_to_parent(
                Matrix4d::make_rotation_x(deg_to_rad(-30.0))));
        assembly->lights().insert(light);

        //------------------------------------------------------------------------
        // Assembly instance
        //------------------------------------------------------------------------

        auto_release_ptr<AssemblyInstance> assembly_instance(
            AssemblyInstanceFactory::create(
                "assembly_inst",
                ParamArray(),
                "assembly"));
        assembly_instance->transform_sequence().set_transform(0.0, Transformd::identity());
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

        static const double CameraMatrix[16] =
        {
            0.898911352044089, -0.291992464000737, 0.326647795236774, 0.051430150409342,
            0.000000000000000, 0.745548999631107, 0.666450815251250, 0.095894497845129,
            -0.438130552650996, -0.599080203408387, 0.670182459273516, 0.110325032485670,
            0.000000000000000, 0.000000000000000, 0.000000000000000, 1.000000000000000
        };

        camera->transform_sequence().set_transform(
            0.0,
            Transformd::from_local_to_parent(
                Matrix4d::from_array(CameraMatrix)));

        scene->cameras().insert(camera);

        //------------------------------------------------------------------------
        // Frame
        //------------------------------------------------------------------------

        project->set_frame(
            FrameFactory::create(
                "beauty",
                ParamArray()
                    .insert("camera", "camera")
                    .insert("resolution", "1280 720")
                    .insert("color_space", "srgb")
                    .insert("camera", "camera")));

        project->set_scene(scene);

        return project;
    }

  protected:
    virtual void initialize_assembly(
        Project&            project,
        Assembly&           assembly,
        const size_t        image_width,
        const size_t        image_height) = 0;

    virtual void add_cube(
        Assembly&           assembly,
        const size_t        ix,
        const size_t        iy,
        const double        fx,
        const double        fz,
        const Color3b&      color,
        const Transformd&   transform) = 0;

    virtual void finalize_assembly(
        Assembly&           assembly) = 0;
};

class SingleBakedMeshProjectBuilder
  : public ProjectBuilder
{
  private:
    auto_release_ptr<MeshObject>    m_cube;
    auto_release_ptr<MeshObject>    m_mesh;

    void initialize_assembly(
        Project&            project,
        Assembly&           assembly,
        const size_t        image_width,
        const size_t        image_height) override
    {
        assembly.textures().insert(
            DiskTexture2dFactory().create(
                "cubes_texture",
                ParamArray()
                    .insert("filename", "heightfield.png")
                    .insert("color_space", "srgb"),
                project.search_paths()));

        assembly.texture_instances().insert(
            TextureInstanceFactory::create(
                "cubes_texture_instance",
                ParamArray()
                    .insert("filtering_mode", "nearest")
                    .insert("addressing_mode", "wrap"),
                "cubes_texture"));

        assembly.bsdfs().insert(
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

        assembly.surface_shaders().insert(
            PhysicalSurfaceShaderFactory().create(
                "physical_surface_shader",
                ParamArray()));

        assembly.materials().insert(
            GenericMaterialFactory().create(
                "cubes_material",
                ParamArray()
                    .insert("surface_shader", "physical_surface_shader")
                    .insert("bsdf", "cubes_brdf")));

        // Load the cube mesh object from disk.
        MeshObjectArray objects;
        MeshObjectReader::read(
            project.search_paths(),
            "cube",
            ParamArray()
                .insert("filename", "cube.obj"),
            objects);
        assert(objects.size() == 1);
        m_cube.reset(objects[0]);

        // Create the baked mesh object.
        m_mesh = MeshObjectFactory().create("cubes", ParamArray());

        // Reserve memory into the baked mesh object.
        const size_t cube_count = image_width * image_height;
        m_mesh->reserve_vertices(m_cube->get_vertex_count() * cube_count);
        m_mesh->reserve_vertex_normals(m_cube->get_vertex_normal_count() * cube_count);
        m_mesh->reserve_tex_coords(cube_count);
        m_mesh->reserve_triangles(m_cube->get_triangle_count() * cube_count);
    }

    void add_cube(
        Assembly&           assembly,
        const size_t        ix,
        const size_t        iy,
        const double        fx,
        const double        fz,
        const Color3b&      color,
        const Transformd&   transform) override
    {
        // Push vertices.
        const size_t base_vertex_index = m_mesh->get_vertex_count();
        for (size_t i = 0; i < m_cube->get_vertex_count(); ++i)
        {
            m_mesh->push_vertex(
                transform.point_to_parent(m_cube->get_vertex(i)));
        }

        // Push normals.
        const size_t base_vertex_normal_index = m_mesh->get_vertex_normal_count();
        for (size_t i = 0; i < m_cube->get_vertex_normal_count(); ++i)
        {
            m_mesh->push_vertex_normal(
                normalize(
                    transform.normal_to_parent(m_cube->get_vertex_normal(i))));
        }

        // Push texture coordinates.
        const size_t tex_coords_index =
            m_mesh->push_tex_coords(
                GVector2(static_cast<float>(fx), static_cast<float>(1.0 - fz)));

        // Push triangles.
        for (size_t i = 0; i < m_cube->get_triangle_count(); ++i)
        {
            Triangle triangle = m_cube->get_triangle(i);
            triangle.m_v0 += static_cast<uint32>(base_vertex_index);
            triangle.m_v1 += static_cast<uint32>(base_vertex_index);
            triangle.m_v2 += static_cast<uint32>(base_vertex_index);
            triangle.m_n0 += static_cast<uint32>(base_vertex_normal_index);
            triangle.m_n1 += static_cast<uint32>(base_vertex_normal_index);
            triangle.m_n2 += static_cast<uint32>(base_vertex_normal_index);
            triangle.m_a0 = triangle.m_a1 = triangle.m_a2 = static_cast<uint32>(tex_coords_index);
            m_mesh->push_triangle(triangle);
        }
    }

    void finalize_assembly(Assembly& assembly) override
    {
        // Insert the baked mesh object into the assembly.
        assembly.objects().insert(auto_release_ptr<Object>(m_mesh));

        // Instantiate the baked mesh object.
        assembly.object_instances().insert(
            ObjectInstanceFactory::create(
                "cubes_instance",
                ParamArray(),
                "cubes",
                Transformd::identity(),
                StringDictionary()
                    .insert("default", "cubes_material")));
    }
};

class InstancesProjectBuilder
  : public ProjectBuilder
{
  private:
    void initialize_assembly(
        Project&            project,
        Assembly&           assembly,
        const size_t        image_width,
        const size_t        image_height) override
    {
        assembly.surface_shaders().insert(
            PhysicalSurfaceShaderFactory().create(
                "physical_surface_shader",
                ParamArray()));

        // Load the cube mesh object from disk.
        MeshObjectArray objects;
        MeshObjectReader::read(
            project.search_paths(),
            "cube",
            ParamArray()
                .insert("filename", "smoothcube.obj"),
            objects);

        // Insert the cube mesh into the assembly.
        assert(objects.size() == 1);
        assembly.objects().insert(auto_release_ptr<Object>(objects[0]));
    }

    void add_cube(
        Assembly&           assembly,
        const size_t        ix,
        const size_t        iy,
        const double        fx,
        const double        fz,
        const Color3b&      color,
        const Transformd&   transform) override
    {
        const string color_suffix = color_to_string(color);
        const string color_name = "color_" + color_suffix;
        const string material_name = "material_" + color_suffix;

        if (assembly.colors().get_by_name(color_name.c_str()) == 0)
        {
            const Color3f reflectance = Color3f(color) * (1.0f / 255);
            assembly.colors().insert(
                ColorEntityFactory::create(
                    color_name.c_str(),
                    ParamArray()
                        .insert("color_space", "srgb"),
                    ColorValueArray(3, &reflectance[0])));

            const string brdf_name = "brdf_" + color_suffix;
            assembly.bsdfs().insert(
                DisneyBRDFFactory().create(
                    brdf_name.c_str(),
                    ParamArray()
                        .insert("anisotropic", 0.0)
                        .insert("base_color", color_name)
                        .insert("clearcoat", 1.0)
                        .insert("clearcoat_gloss", 0.9)
                        .insert("metallic", 0.0)
                        .insert("roughness", 0.3)
                        .insert("sheen", 0.0)
                        .insert("sheen_tint", 0.0)
                        .insert("specular", 0.9)
                        .insert("specular_tint", 1.0)
                        .insert("subsurface", 1.0)));

            assembly.materials().insert(
                GenericMaterialFactory().create(
                    material_name.c_str(),
                    ParamArray()
                        .insert("surface_shader", "physical_surface_shader")
                        .insert("bsdf", brdf_name)));
        }

        // Create an assembly for this cube.
        const string coordinates_suffix = coordinates_to_string(ix, iy);
        const string cube_assembly_name = "assembly_" + coordinates_suffix;
        auto_release_ptr<Assembly> cube_assembly(
            AssemblyFactory().create(cube_assembly_name.c_str()));

        // Instantiate the cube mesh object (from the parent assembly) into this assembly.
        cube_assembly->object_instances().insert(
            ObjectInstanceFactory::create(
                "cube.0_inst",
                ParamArray(),
                "cube.0",
                Transformd::identity(),
                StringDictionary()
                    .insert("default", material_name)));

        // Create an instance of the cube assembly.
        const string cube_assembly_inst_name = cube_assembly_name + "_inst";
        auto_release_ptr<AssemblyInstance> cube_assembly_inst(
            AssemblyInstanceFactory::create(
                cube_assembly_inst_name.c_str(),
                ParamArray(),
                cube_assembly_name.c_str()));
        cube_assembly_inst->transform_sequence().set_transform(0.0, transform);

        // Insert both the cube assembly and its instance into the parent assembly.
        assembly.assemblies().insert(cube_assembly);
        assembly.assembly_instances().insert(cube_assembly_inst);
    }

    void finalize_assembly(Assembly& assembly) override
    {
    }

    static string color_to_string(const Color3b& c)
    {
        stringstream sstr;
        sstr << '#' << hex << setfill('0')
             << setw(2) << int(c.r)
             << setw(2) << int(c.g)
             << setw(2) << int(c.b);
        return sstr.str();
    }

    static string coordinates_to_string(const size_t x, const size_t y)
    {
        stringstream sstr;
        sstr << setfill('0')
             << setw(4) << x << "_"
             << setw(4) << y;
        return sstr.str();
    }
};

int main()
{
    // Create a log target that outputs to stderr, and binds it to the renderer's global logger.
    unique_ptr<ILogTarget> log_target(create_console_log_target(stderr));
    global_logger().add_target(log_target.get());

    //SingleBakedMeshProjectBuilder project_builder;
    InstancesProjectBuilder project_builder;

    auto_release_ptr<Project> project(project_builder.build_project());
    ProjectFileWriter::write(project.ref(), "output/heightfield.appleseed");

    return 0;
}
