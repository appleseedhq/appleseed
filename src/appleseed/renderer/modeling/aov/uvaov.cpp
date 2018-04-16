
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
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
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
      : public UnfilteredAOVAccumulator
    {
      public:
        UVAOVAccumulator(Image& image, Image& filter_image)
          : UnfilteredAOVAccumulator(image, filter_image)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            ShadingResult&              shading_result) override
        {
            const Vector2i& pi = pixel_context.get_pixel_coords();

            // Ignore samples outside the tile.
            if (outside_tile(pi))
                return;

            float* p = reinterpret_cast<float*>(
                get_tile().pixel(pi.x - m_tile_origin_x, pi.y - m_tile_origin_y));

            float* f = reinterpret_cast<float*>(
                get_filter_tile().pixel(pi.x - m_tile_origin_x, pi.y - m_tile_origin_y));

            const float min_sample_square_distance = *f;
            const float sample_square_distance =
                square_distance_to_pixel_center(pixel_context.get_sample_position());

            if (sample_square_distance < min_sample_square_distance)
            {
                if (shading_point.hit_surface())
                {
                    const Vector2f& uv = shading_point.get_uv(0);
                    p[0] = uv[0];
                    p[1] = uv[1];
                    p[2] = 0.0f;
                    *f = sample_square_distance;
                }
                else
                {
                    p[0] = 0.0f;
                    p[1] = 0.0f;
                    p[2] = 0.0f;
                    *f = sample_square_distance;
                }
            }
        }
    };


    //
    // UV AOV.
    //

    const char* Model = "uv_aov";

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
            return Model;
        }

        size_t get_channel_count() const override
        {
            return 3;
        }

        const char** get_channel_names() const override
        {
            static const char* ChannelNames[] = {"R", "G", "B"};
            return ChannelNames;
        }

        void clear_image() override
        {
            m_image->clear(Color3f(0.0f, 0.0f, 0.0f));
            m_filter_image->clear(Color<float, 1>(numeric_limits<float>::max()));
        }

        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new UVAOVAccumulator(get_image(), *m_filter_image));
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

}   // namespace renderer
