
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
#include "normalcoloraov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
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


namespace
{
    // Compute a color from a given unit-length 3D vector.
    template <typename T>
    inline Color3f unit_vector3_to_color(const Vector<T, 3>& vec)
    {
        assert(is_normalized(vec));

#ifdef SHADE_VECTORS_USING_3DSMAX_CONVENTIONS
        return Color3f(
            saturate(( static_cast<float>(vec[0]) + 1.0f) * 0.5f),
            saturate((-static_cast<float>(vec[2]) + 1.0f) * 0.5f),
            saturate(( static_cast<float>(vec[1]) + 1.0f) * 0.5f));
#else
        return Color3f(
            saturate((static_cast<float>(vec[0]) + 1.0f) * 0.5f),
            saturate((static_cast<float>(vec[1]) + 1.0f) * 0.5f),
            saturate((static_cast<float>(vec[2]) + 1.0f) * 0.5f));
#endif
    }
}

namespace renderer
{

namespace
{
    //
    // Normal Color AOV accumulator.
    //

    class NormalColorAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit NormalColorAOVAccumulator(const size_t index)
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
            if (shading_point.hit_surface())
            {
                shading_result.m_aovs[m_index].rgb() = unit_vector3_to_color(shading_point.get_geometric_normal());
                shading_result.m_aovs[m_index].a = shading_result.m_main.a;
            }
            else
            {
                shading_result.m_aovs[m_index].rgb() = Color3f(0.5f);
                shading_result.m_aovs[m_index].a = shading_result.m_main.a;
            }
            
        }

      private:
        const size_t m_index;
    };


    //
    // Normal Color AOV.
    //

    const char* NormalColorAOVModel = "normal_color_aov";

    class NormalColorAOV
      : public ColorAOV
    {
      public:
        explicit NormalColorAOV(const ParamArray& params)
          : ColorAOV("normal_color", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return NormalColorAOVModel;
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new NormalColorAOVAccumulator(m_image_index));
        }
    };
}


//
// NormalColorAOVFactory class implementation.
//

void NormalColorAOVFactory::release()
{
    delete this;
}

const char* NormalColorAOVFactory::get_model() const
{
    return NormalColorAOVModel;
}

Dictionary NormalColorAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Normal Color");
}

DictionaryArray NormalColorAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> NormalColorAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new NormalColorAOV(params));
}

}   // namespace renderer
