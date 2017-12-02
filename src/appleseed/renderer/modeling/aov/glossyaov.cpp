
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
#include "glossyaov.h"

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
    // DirectGlossy AOV accumulator.
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
    // IndirectGlossy AOV accumulator.
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

    const char* GlossyModel = "glossy_aov";

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
            return GlossyModel;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new GlossyAOVAccumulator(index));
        }
    };


    //
    // Direct Glossy AOV.
    //

    const char* DirectGlossyModel = "direct_glossy_aov";

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
            return DirectGlossyModel;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new DirectGlossyAOVAccumulator(index));
        }
    };


    //
    // Indirect Glossy AOV.
    //

    const char* IndirectGlossyModel = "indirect_glossy_aov";

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
            return IndirectGlossyModel;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new IndirectGlossyAOVAccumulator(index));
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
    return GlossyModel;
}

Dictionary GlossyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Glossy")
            .insert("default_model", "false");
}

DictionaryArray GlossyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> GlossyAOVFactory::create(
    const ParamArray&   params) const
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
    return DirectGlossyModel;
}

Dictionary DirectGlossyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Direct Glossy")
            .insert("default_model", "false");
}

DictionaryArray DirectGlossyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> DirectGlossyAOVFactory::create(
    const ParamArray&   params) const
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
    return IndirectGlossyModel;
}

Dictionary IndirectGlossyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Indirect Glossy")
            .insert("default_model", "false");
}

DictionaryArray IndirectGlossyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> IndirectGlossyAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new IndirectGlossyAOV(params));
}

}   // namespace renderer
