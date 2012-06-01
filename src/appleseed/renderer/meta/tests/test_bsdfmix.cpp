
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfmix.h"
#include "renderer/modeling/bsdf/lambertianbrdf.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_BSDF_BSDFMix)
{
    template <typename T>
    T get_value(const InputEvaluator& input_evaluator, const size_t offset)
    {
        const uint8* base = static_cast<const uint8*>(input_evaluator.data());
        return *reinterpret_cast<const T*>(base + offset);
    }

    TEST_CASE(EvaluateInputs_RecursivelyEvaluateChildBSDFInputs)
    {
        auto_release_ptr<Project> project = ProjectFactory::create("project");

        project->set_scene(SceneFactory::create());

        Scene& scene = *project->get_scene();

        scene.assemblies().insert(
            AssemblyFactory::create("assembly", ParamArray()));

        Assembly& assembly = *scene.assemblies().get_by_name("assembly");

        BSDFMixFactory bsdfmix_factory;
        LambertianBRDFFactory lambertianbrdf_factory;

        assembly.bsdfs().insert(
            bsdfmix_factory.create(
                "parent_bsdf",
                ParamArray()
                    .insert("bsdf0", "child0_bsdf")
                    .insert("weight0", "0.6")
                    .insert("bsdf1", "child1_bsdf")
                    .insert("weight1", "0.4")));

        assembly.bsdfs().insert(
            bsdfmix_factory.create(
                "child0_bsdf",
                ParamArray()
                    .insert("bsdf0", "child0_child0_bsdf")
                    .insert("weight0", "0.2")
                    .insert("bsdf1", "child0_child1_bsdf")
                    .insert("weight1", "0.8")));

        assembly.bsdfs().insert(
            lambertianbrdf_factory.create(
                "child0_child0_bsdf",
                ParamArray()
                    .insert("reflectance", "0.5")));

        assembly.bsdfs().insert(
            lambertianbrdf_factory.create(
                "child0_child1_bsdf",
                ParamArray()
                    .insert("reflectance", "0.1")));

        assembly.bsdfs().insert(
            lambertianbrdf_factory.create(
                "child1_bsdf",
                ParamArray()
                    .insert("reflectance", "1.0")));

        InputBinder input_binder;
        input_binder.bind(scene);

        scene.on_frame_begin(project.ref());

        TextureStore texture_store(scene);
        TextureCache texture_cache(texture_store);
        InputEvaluator input_evaluator(texture_cache);

        BSDF& parent_bsdf = *assembly.bsdfs().get_by_name("parent_bsdf");
        parent_bsdf.evaluate_inputs(input_evaluator, Vector2d(0.0));

        // parent_bsdf mixing weights.
        EXPECT_EQ(0.6, get_value<double>(input_evaluator, 0));
        EXPECT_EQ(0.4, get_value<double>(input_evaluator, 8));

        // child0_bsdf mixing weights.
        EXPECT_EQ(0.2, get_value<double>(input_evaluator, 16));
        EXPECT_EQ(0.8, get_value<double>(input_evaluator, 24));

        // child0_child0_bsdf reflectance.
        EXPECT_EQ(Spectrum(0.5f), get_value<Spectrum>(input_evaluator, 32));

        // child0_child1_bsdf reflectance.
        EXPECT_EQ(Spectrum(0.1f), get_value<Spectrum>(input_evaluator, 176));

        // child1_bsdf reflectance.
        EXPECT_EQ(Spectrum(1.0f), get_value<Spectrum>(input_evaluator, 320));

        scene.on_frame_end(project.ref());
    }
}
