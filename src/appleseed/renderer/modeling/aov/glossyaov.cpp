
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

// Interface header.
#include "glossyaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/color/colorspace.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Glossy AOV accumulator.
    //

    class GlossyAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit GlossyAOVAccumulator(const size_t index)
          : m_index(index)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            shading_result.m_aovs[m_index].rgb() =
                shading_components.m_glossy.to_rgb(g_std_lighting_conditions) +
                shading_components.m_indirect_glossy.to_rgb(g_std_lighting_conditions);

            shading_result.m_aovs[m_index].a = shading_result.m_main.a;
        }

      private:
        const size_t m_index;
    };


    //
    // Direct Glossy AOV accumulator.
    //

    class DirectGlossyAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit DirectGlossyAOVAccumulator(const size_t index)
          : m_index(index)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            shading_result.m_aovs[m_index].rgb() =
                shading_components.m_glossy.to_rgb(g_std_lighting_conditions);

            shading_result.m_aovs[m_index].a = shading_result.m_main.a;
        }

      private:
        const size_t m_index;
    };


    //
    // Indirect Glossy AOV accumulator.
    //

    class IndirectGlossyAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit IndirectGlossyAOVAccumulator(const size_t index)
          : m_index(index)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            shading_result.m_aovs[m_index].rgb() =
                shading_components.m_indirect_glossy.to_rgb(g_std_lighting_conditions);

            shading_result.m_aovs[m_index].a = shading_result.m_main.a;
        }

      private:
        const size_t m_index;
    };


    //
    // Glossy AOV.
    //

    const char* GlossyAOVModel = "glossy_aov";

    class GlossyAOV
      : public ColorAOV
    {
      public:
        explicit GlossyAOV(const ParamArray& params)
          : ColorAOV("glossy", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return GlossyAOVModel;
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new GlossyAOVAccumulator(m_image_index));
        }
    };


    //
    // Direct Glossy AOV.
    //

    const char* DirectGlossyAOVModel = "direct_glossy_aov";

    class DirectGlossyAOV
      : public ColorAOV
    {
      public:
        explicit DirectGlossyAOV(const ParamArray& params)
          : ColorAOV("direct_glossy", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return DirectGlossyAOVModel;
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new DirectGlossyAOVAccumulator(m_image_index));
        }
    };


    //
    // Indirect Glossy AOV.
    //

    const char* IndirectGlossyAOVModel = "indirect_glossy_aov";

    class IndirectGlossyAOV
      : public ColorAOV
    {
      public:
        explicit IndirectGlossyAOV(const ParamArray& params)
          : ColorAOV("indirect_glossy", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return IndirectGlossyAOVModel;
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new IndirectGlossyAOVAccumulator(m_image_index));
        }
    };
}


//
// GlossyAOVFactory class implementation.
//

void GlossyAOVFactory::release()
{
    delete this;
}

const char* GlossyAOVFactory::get_model() const
{
    return GlossyAOVModel;
}

Dictionary GlossyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Glossy");
}

DictionaryArray GlossyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> GlossyAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new GlossyAOV(params));
}


//
// DirectGlossyAOVFactory class implementation.
//

void DirectGlossyAOVFactory::release()
{
    delete this;
}

const char* DirectGlossyAOVFactory::get_model() const
{
    return DirectGlossyAOVModel;
}

Dictionary DirectGlossyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Direct Glossy");
}

DictionaryArray DirectGlossyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> DirectGlossyAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new DirectGlossyAOV(params));
}


//
// IndirectGlossyAOVFactory class implementation.
//

void IndirectGlossyAOVFactory::release()
{
    delete this;
}

const char* IndirectGlossyAOVFactory::get_model() const
{
    return IndirectGlossyAOVModel;
}

Dictionary IndirectGlossyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Indirect Glossy");
}

DictionaryArray IndirectGlossyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> IndirectGlossyAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new IndirectGlossyAOV(params));
}

}   // namespace renderer
