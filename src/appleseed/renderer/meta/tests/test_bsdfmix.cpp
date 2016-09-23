
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
#include "renderer/kernel/lighting/tracer.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#endif
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingpointbuilder.h"
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
#include "renderer/utility/iostreamop.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/test.h"

// OSL headers.
#ifdef APPLESEED_WITH_OSL
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
END_OSL_INCLUDES
#endif

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
#include <cassert>
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
        TextureStore texture_store(scene);

#ifdef APPLESEED_WITH_OIIO
        boost::shared_ptr<OIIO::TextureSystem> texture_system(
            OIIO::TextureSystem::create(),
            boost::bind(&OIIO::TextureSystem::destroy, _1));
#endif
#ifdef APPLESEED_WITH_OSL
        RendererServices renderer_services(*project, *texture_system);

        boost::shared_ptr<OSL::ShadingSystem> shading_system(
            new OSL::ShadingSystem(&renderer_services, texture_system.get()));
#endif

        scene.assemblies().insert(
            AssemblyFactory().create("assembly", ParamArray()));

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
        assert(input_binder.get_error_count() == 0);

        scene.on_frame_begin(project.ref());

        TextureCache texture_cache(texture_store);
        InputEvaluator input_evaluator(texture_cache);

        Intersector intersector(
            project->get_trace_context(),
            texture_cache);

#ifdef APPLESEED_WITH_OSL
        OSLShaderGroupExec sg_exec(*shading_system);
#endif
        Tracer tracer(
            *project->get_scene(),
            intersector,
            texture_cache
#ifdef APPLESEED_WITH_OSL
            , sg_exec
#endif
            );

        ShadingContext shading_context(
            intersector,
            tracer,
            texture_cache
#ifdef APPLESEED_WITH_OIIO
            , *texture_system
#endif
#ifdef APPLESEED_WITH_OSL
            , sg_exec
#endif
            , 0);

        ShadingPoint shading_point;
        ShadingPointBuilder builder(shading_point);
        builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
        builder.set_uvs(Vector2f(0.0));

        BSDF& parent_bsdf = *assembly.bsdfs().get_by_name("parent_bsdf");
        parent_bsdf.evaluate_inputs(
            shading_context,
            input_evaluator,
            shading_point);

        size_t offset = 0;

        // parent_bsdf mixing weights.
        EXPECT_EQ(0.6, get_value<double>(input_evaluator, offset)); offset += sizeof(double);
        EXPECT_EQ(0.4, get_value<double>(input_evaluator, offset)); offset += sizeof(double);

        // child0_bsdf mixing weights.
        EXPECT_EQ(0.2, get_value<double>(input_evaluator, offset)); offset += sizeof(double);
        EXPECT_EQ(0.8, get_value<double>(input_evaluator, offset)); offset += sizeof(double);

        // child0_child0_bsdf reflectance.
        EXPECT_EQ(Spectrum(0.5f), get_value<Spectrum>(input_evaluator, offset));
        offset = align(offset + sizeof(Spectrum) + sizeof(double), 16);

        // child0_child1_bsdf reflectance.
        EXPECT_EQ(Spectrum(0.1f), get_value<Spectrum>(input_evaluator, offset));
        offset = align(offset + sizeof(Spectrum) + sizeof(double), 16);

        // child1_bsdf reflectance.
        EXPECT_EQ(Spectrum(1.0f), get_value<Spectrum>(input_evaluator, offset));

        scene.on_frame_end(project.ref());
    }
}
