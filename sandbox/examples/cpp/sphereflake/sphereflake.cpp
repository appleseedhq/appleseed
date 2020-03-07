
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/job/iabortswitch.h"

// appleseed.main headers.
#include "main/dllvisibility.h"

// Sphereflake headers.
#include "sphereflakedata.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace asf = foundation;
namespace asr = renderer;

namespace
{
    //
    // New assembly model.
    //

    const char* Model = "sphereflake_assembly";

    class SphereFlakeAssembly
      : public asr::ProceduralAssembly
    {
      public:
        // Constructor.
        SphereFlakeAssembly(
            const char*             name,
            const asr::ParamArray&  params)
          : asr::ProceduralAssembly(name, params)
        {
        }

        // Delete this instance.
        void release() override
        {
            delete this;
        }

        // Return a string identifying the model of this entity.
        const char* get_model() const override
        {
            return Model;
        }

      private:
        // Expand the contents of the assembly.
        bool do_expand_contents(
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

        void build_sphere_master_object()
        {
            auto sphere_assembly =
                asr::AssemblyFactory().create(
                    "sphere_assembly",
                    asr::ParamArray());

            get_parameters().insert("primitive", "sphere");
            asf::auto_release_ptr<asr::Object> sphere_mesh(
                asr::create_primitive_mesh(
                    "sphere_mesh",
                    get_parameters()));

            asf::StringDictionary materials;
            materials.insert("default", get_parameters().get("material"));

            auto sphere_mesh_instance =
                asr::ObjectInstanceFactory().create(
                    "sphere_mesh_instance",
                    asr::ParamArray(),
                    "sphere_mesh",
                    asf::Transformd::make_identity(),
                    materials,
                    materials);

            sphere_assembly->objects().insert(sphere_mesh);
            sphere_assembly->object_instances().insert(sphere_mesh_instance);

            assemblies().insert(sphere_assembly);
        }

        void output_sphere_instance(const asf::Vector3d& center, const double radius)
        {
            const size_t index = assembly_instances().size() + 1;

            auto sphere_instance(
                asr::AssemblyInstanceFactory().create(
                    asf::format("sphere_assembly_instance_{0}", index).c_str(),
                    asr::ParamArray(),
                    "sphere_assembly"));

            const asf::Matrix4d matrix =
                asf::Matrix4d::make_translation(center) *
                asf::Matrix4d::make_scaling(asf::Vector3d(radius));

            sphere_instance->transform_sequence().set_transform(0.0f, asf::Transformd(matrix));
            assembly_instances().insert(sphere_instance);
        }
    };


    //
    // Factory for the new assembly model.
    //

    class SphereFlakeAssemblyFactory
      : public asr::IAssemblyFactory
    {
      public:
        // Delete this instance.
        void release() override
        {
            delete this;
        }

        // Return a string identifying this assembly model.
        const char* get_model() const override
        {
            return Model;
        }

        // Return metadata for this assembly model.
        asf::Dictionary get_model_metadata() const override
        {
            return asf::Dictionary();
        }

        // Return metadata for the inputs of this assembly model.
        asf::DictionaryArray get_input_metadata() const override
        {
            asf::DictionaryArray metadata;
            return metadata;
        }

        // Create a new assembly.
        asf::auto_release_ptr<asr::Assembly> create(
            const char*             name,
            const asr::ParamArray&  params) const override
        {
            return asf::auto_release_ptr<asr::Assembly>(
                new SphereFlakeAssembly(name, params));
        }
    };
}


//
// Plugin entry point.
//

extern "C"
{
    APPLESEED_DLL_EXPORT asr::IAssemblyFactory* appleseed_create_assembly_factory()
    {
        return new SphereFlakeAssemblyFactory();
    }
}
