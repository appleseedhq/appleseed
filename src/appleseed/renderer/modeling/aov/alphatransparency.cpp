
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
#include "alphatransparency.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/aovcomponents.h"
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
    // AlphaTransparency AOV accumulator.
    //

    class AlphaTransparencyAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit AlphaTransparencyAOVAccumulator(const size_t index)
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
            shading_result.m_aovs[m_index].set(luminance(aov_components.m_refraction_value.to_rgb(g_std_lighting_conditions)));
            shading_result.m_aovs[m_index].a = shading_result.m_main.a;
        }

      private:
        const size_t m_index;
    };


    //
    // AlphaTransparency AOV.
    //

    const char* AlphaTransparency = "alphatransparency_aov";

    class AlphaTransparencyAOV
      : public ColorAOV
    {
      public:
        explicit AlphaTransparencyAOV(const ParamArray& params)
          : ColorAOV("alphatransparency", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return AlphaTransparency;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new AlphaTransparencyAOVAccumulator(m_image_index));
        }
    };
}


//
// AlphaTransparencyAOVFactory class implementation.
//

void AlphaTransparencyAOVFactory::release()
{
    delete this;
}

const char* AlphaTransparencyAOVFactory::get_model() const
{
    return AlphaTransparency;
}

Dictionary AlphaTransparencyAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Alpha Transparency")
            .insert("default_model", "false");
}

DictionaryArray AlphaTransparencyAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> AlphaTransparencyAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new AlphaTransparencyAOV(params));
}

}   // namespace renderer
