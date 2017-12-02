
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

// Interface header.
#include "diffuseaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/color/colorspace.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Diffuse AOV accumulator.
    //

    class DiffuseAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit DiffuseAOVAccumulator(const size_t index)
          : m_index(index)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            ShadingResult&              shading_result) override
        {
            shading_result.m_aovs[m_index].rgb() =
                shading_components.m_diffuse.to_rgb(g_std_lighting_conditions) +
                shading_components.m_indirect_diffuse.to_rgb(g_std_lighting_conditions);

            shading_result.m_aovs[m_index].a = shading_result.m_main.a;
        }

      private:
        const size_t m_index;
    };


    //
    // DirectDiffuse AOV accumulator.
    //

    class DirectDiffuseAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit DirectDiffuseAOVAccumulator(const size_t index)
          : m_index(index)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            ShadingResult&              shading_result) override
        {
            shading_result.m_aovs[m_index].rgb() =
                shading_components.m_diffuse.to_rgb(g_std_lighting_conditions);

            shading_result.m_aovs[m_index].a = shading_result.m_main.a;
        }

      private:
        const size_t m_index;
    };


    //
    // IndirectDiffuse AOV accumulator.
    //

    class IndirectDiffuseAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit IndirectDiffuseAOVAccumulator(const size_t index)
          : m_index(index)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            ShadingResult&              shading_result) override
        {
            shading_result.m_aovs[m_index].rgb() =
                shading_components.m_indirect_diffuse.to_rgb(g_std_lighting_conditions);

            shading_result.m_aovs[m_index].a = shading_result.m_main.a;
        }

      private:
        const size_t m_index;
    };


    //
    // Diffuse AOV.
    //

    const char* DiffuseModel = "diffuse_aov";

    class DiffuseAOV
      : public ColorAOV
    {
      public:
        explicit DiffuseAOV(const ParamArray& params)
          : ColorAOV("diffuse", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return DiffuseModel;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new DiffuseAOVAccumulator(index));
        }
    };


    //
    // DirectDiffuse AOV.
    //

    const char* DirectDiffuseModel = "direct_diffuse_aov";

    class DirectDiffuseAOV
      : public ColorAOV
    {
      public:
        explicit DirectDiffuseAOV(const ParamArray& params)
          : ColorAOV("direct_diffuse", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return DirectDiffuseModel;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new DirectDiffuseAOVAccumulator(index));
        }
    };


    //
    // IndirectDiffuse AOV.
    //

    const char* IndirectDiffuseModel = "indirect_diffuse_aov";

    class IndirectDiffuseAOV
      : public ColorAOV
    {
      public:
        explicit IndirectDiffuseAOV(const ParamArray& params)
          : ColorAOV("indirect_diffuse", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return IndirectDiffuseModel;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new IndirectDiffuseAOVAccumulator(index));
        }
    };
}


//
// DiffuseAOVFactory class implementation.
//

void DiffuseAOVFactory::release()
{
    delete this;
}

const char* DiffuseAOVFactory::get_model() const
{
    return DiffuseModel;
}

Dictionary DiffuseAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Diffuse")
            .insert("default_model", "false");
}

DictionaryArray DiffuseAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> DiffuseAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new DiffuseAOV(params));
}


//
// DirectDiffuseAOVFactory class implementation.
//

void DirectDiffuseAOVFactory::release()
{
    delete this;
}

const char* DirectDiffuseAOVFactory::get_model() const
{
    return DirectDiffuseModel;
}

Dictionary DirectDiffuseAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Direct Diffuse")
            .insert("default_model", "false");
}

DictionaryArray DirectDiffuseAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> DirectDiffuseAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new DirectDiffuseAOV(params));
}


//
// IndirectDiffuseAOVFactory class implementation.
//

void IndirectDiffuseAOVFactory::release()
{
    delete this;
}

const char* IndirectDiffuseAOVFactory::get_model() const
{
    return IndirectDiffuseModel;
}

Dictionary IndirectDiffuseAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Indirect Diffuse")
            .insert("default_model", "false");
}

DictionaryArray IndirectDiffuseAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> IndirectDiffuseAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new IndirectDiffuseAOV(params));
}

}   // namespace renderer
