
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
#include "uvaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"

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
    // UV AOV accumulator.
    //

    class UVAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit UVAOVAccumulator(const size_t index)
          : m_index(index)
        {
        }

        virtual void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            ShadingResult&              shading_result) override
        {
            if (shading_point.hit())
            {
                const Vector2f uvs(shading_point.get_uv(0));
                shading_result.m_aovs[m_index] = Color4f(uvs[0], uvs[1], 0.0f, 1.0f);
            }
        }

      private:
        const size_t m_index;
    };


    //
    // UV AOV.
    //

    const char* Model = "uv_aov";

    class UVAOV
      : public AOV
    {
      public:
        explicit UVAOV(const ParamArray& params)
          : AOV("uv", params)
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
            return 2;
        }

        virtual const char** get_channel_names() const override
        {
            static const char* ChannelNames[] = {"U", "V"};
            return ChannelNames;
        }

        virtual bool has_color_data() const override
        {
            return false;
        }

        virtual auto_release_ptr<AOVAccumulator> create_accumulator(
            const size_t index) const override
        {
            return auto_release_ptr<AOVAccumulator>(new UVAOVAccumulator(index));
        }
    };
}


//
// UVAOVFactory class implementation.
//

const char* UVAOVFactory::get_model() const
{
    return Model;
}

Dictionary UVAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "UV")
            .insert("default_model", "false");
}

DictionaryArray UVAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> UVAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new UVAOV(params));
}

auto_release_ptr<AOV> UVAOVFactory::static_create(
    const ParamArray&   params)
{
    return auto_release_ptr<AOV>(new UVAOV(params));
}

}   // namespace renderer
