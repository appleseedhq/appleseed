
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/api/log.h"
#include "renderer/api/object.h"
#include "renderer/api/scene.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cstring>
#include <memory>

// Sphereflake headers.
#include"sphereflakedata.h"

namespace asf = foundation;
namespace asr = renderer;

class SphereFlakeAssembly
  : public asr::PluginAssembly
{
  public:
    SphereFlakeAssembly(const char* name, const asr::ParamArray& params)
      : asr::PluginAssembly(name, params)
    {
    }

    void release() override
    {
        delete this;
    }

    bool expand_contents(
        const asr::Project&     project,
        const asr::Assembly*    parent,
        asf::IAbortSwitch*      abort_switch = 0) override
    {
        build_sphere_master_object();

        const double* p = spheres;
        for (size_t i = 0; i < NumSpheres; ++i)
        {
            const asf::Vector3d center(p[0], p[1], p[2]);
            const double radius = p[3];
            output_sphere_instance(center, radius);
            p += 4;
        }

        return true;
    }

  private:
    void build_sphere_master_object()
    {
        asf::auto_release_ptr<Assembly> sphere_assembly = asr::AssemblyFactory().create(
            "sphere_assembly",
            asr::ParamArray());

        get_parameters().insert("primitive", "sphere");
        asf::auto_release_ptr<asr::MeshObject> sphere_mesh = asr::create_primitive_mesh(
            "sphere_mesh",
            get_parameters());

        asf::StringDictionary materials;
        materials.insert("default", get_parameters().get("material"));

        asf::auto_release_ptr<asr::ObjectInstance> sphere_mesh_instance = asr::ObjectInstanceFactory().create(
            "sphere_mesh_instance",
            asr::ParamArray(),
            "sphere_mesh",
            asf::Transformd::make_identity(),
            materials,
            materials);

        sphere_assembly->objects().insert(asf::auto_release_ptr<asr::Object>(sphere_mesh.release()));
        sphere_assembly->object_instances().insert(sphere_mesh_instance);
        assemblies().insert(sphere_assembly);
    }

    void output_sphere_instance(const asf::Vector3d& center, const double radius)
    {
        const size_t index = assembly_instances().size() + 1;
        asf::auto_release_ptr<asr::AssemblyInstance> sphere_instance(asr::AssemblyInstanceFactory().create(
            asf::format("sphere_assembly_instance_{0}", index).c_str(),
            asr::ParamArray(),
            "sphere_assembly"));

        asf::Matrix4d matrix =
            asf::Matrix4d::make_translation(center) *
            asf::Matrix4d::make_scaling(asf::Vector3d(radius));

        sphere_instance->transform_sequence().set_transform(0.0f, asf::Transformd(matrix));
        assembly_instances().insert(sphere_instance);
    }
};


class SphereFlakeAssemblyFactory
  : public asr::PluginAssemblyFactory
{
  public:
    // Delete this instance.
    void release() override
    {
        delete this;
    }

    // Create a new assembly.
    asf::auto_release_ptr<asr::Assembly> create(
        const char*             name,
        const asr::ParamArray&  params = asr::ParamArray()) const override
    {
        return asf::auto_release_ptr<asr::Assembly>(
            new SphereFlakeAssembly(name, params));
    }
};

extern "C"
{

// Procedural assembly plugin entry point.
asr::IAssemblyFactory* create_assembly_factory()
{
    return new SphereFlakeAssemblyFactory();
}

}
