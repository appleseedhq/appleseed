
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
#include "uvaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
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
    // UV AOV accumulator.
    //

    class UVAOVAccumulator
      : public UnfilteredAOVAccumulator
    {
      public:
        explicit UVAOVAccumulator(Image& image)
          : UnfilteredAOVAccumulator(image)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            const Vector2i& pi = pixel_context.get_pixel_coords();

            // Ignore samples outside the tile.
            if (!m_cropped_tile_bbox.contains(pi))
                return;

            float* out =
                reinterpret_cast<float*>(
                    m_tile->pixel(
                        pi.x - m_tile_origin_x,
                        pi.y - m_tile_origin_y));

            if (shading_point.hit_surface())
            {
                const Vector2f& uv = shading_point.get_uv(0);
                out[0] = uv[0];
                out[1] = uv[1];
                out[2] = 0.0f;
            }
            else
            {
                out[0] = 0.0f;
                out[1] = 0.0f;
                out[2] = 0.0f;
            }
        }
    };


    //
    // UV AOV.
    //

    const char* UVAOVModel = "uv_aov";

    class UVAOV
      : public UnfilteredAOV
    {
      public:
        explicit UVAOV(const ParamArray& params)
          : UnfilteredAOV("uv", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return UVAOVModel;
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new UVAOVAccumulator(get_image()));
        }
    };
}


//
// UVAOVFactory class implementation.
//

void UVAOVFactory::release()
{
    delete this;
}

const char* UVAOVFactory::get_model() const
{
    return UVAOVModel;
}

Dictionary UVAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "UV");
}

DictionaryArray UVAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> UVAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new UVAOV(params));
}

}   // namespace renderer
