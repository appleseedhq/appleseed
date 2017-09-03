
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
          : AOVAccumulator(index)
        {
        }

        virtual void reset() override
        {
            m_color.set(0.0f);
        }

        virtual void write(
            const ShadingComponents&    shading_components,
            const float                 multiplier) override
        {
            m_color = shading_components.m_glossy.to_rgb(g_std_lighting_conditions);
            m_color += shading_components.m_indirect_glossy.to_rgb(g_std_lighting_conditions);
            m_color *= multiplier;
        }

        virtual void flush(ShadingResult& result) override
        {
            result.m_aovs[m_index].rgb() = m_color;
            result.m_aovs[m_index].a = result.m_main.a;
        }

      private:
        Color3f m_color;
    };


    //
    // Glossy AOV.
    //

    const char* Model = "glossy_aov";

    class GlossyAOV
      : public AOV
    {
      public:
        GlossyAOV(const char* name, const ParamArray& params)
          : AOV(name, params)
        {
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual size_t get_channel_count() const override
        {
            return 3;
        }

        virtual const char** get_channel_names() const override
        {
            static const char* ChannelNames[] = {"R", "G", "B"};
            return ChannelNames;
        }

        virtual bool has_color_data() const override
        {
            return true;
        }

        virtual auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new GlossyAOVAccumulator(index));
        }
    };
}


//
// GlossyAOVFactory class implementation.
//

const char* GlossyAOVFactory::get_model() const
{
    return Model;
}

Dictionary GlossyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Glossy")
            .insert("default_model", "false");
}

DictionaryArray GlossyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> GlossyAOVFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<AOV>(
            new GlossyAOV(name, params));
}

auto_release_ptr<AOV> GlossyAOVFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<AOV>(
            new GlossyAOV(name, params));
}

}   // namespace renderer
