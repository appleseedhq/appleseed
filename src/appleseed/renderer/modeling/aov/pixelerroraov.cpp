
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Luke Wilimitis, The appleseedhq Organization
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
#include "pixelerroraov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/analysis.h"
#include "foundation/image/color.h"
#include "foundation/image/colormap.h"
#include "foundation/image/colormapdata.h"
#include "foundation/image/image.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Pixel Error AOV.
    //

    const char* PixelErrorAOVModel = "pixel_error_aov";

    class PixelErrorAOV
      : public UnfilteredAOV
    {
      public:
        explicit PixelErrorAOV(const ParamArray& params)
          : UnfilteredAOV("pixel_error", params)
        {
        }

        const char* get_model() const override
        {
            return PixelErrorAOVModel;
        }

        void post_process_image(const Frame& frame) override
        {
            if (!frame.has_valid_ref_image())
                return;

            ColorMap color_map;
            color_map.set_palette_from_array(InfernoColorMapLinearRGB, countof(InfernoColorMapLinearRGB) / 3);

            const AABB2u& crop_window = frame.get_crop_window();

            float max_error = 0.0f;

            for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
            {
                for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
                {
                    Color3f image_color;
                    frame.image().get_pixel(x, y, image_color);

                    Color3f ref_color;
                    frame.ref_image()->get_pixel(x, y, ref_color);

                    const float error = std::sqrt(square_distance(image_color, ref_color));
                    max_error = std::max(max_error, error);

                    m_image->set_pixel(x, y, &error, 1);
                }
            }

            if (max_error == 0.0f)
                return;

            color_map.remap_red_channel(*m_image, crop_window, 0.0f, max_error);
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(new AOVAccumulator());
        }
    };
}


//
// PixelErrorAOVFactory class implementation.
//

void PixelErrorAOVFactory::release()
{
    delete this;
}

const char* PixelErrorAOVFactory::get_model() const
{
    return PixelErrorAOVModel;
}

Dictionary PixelErrorAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", PixelErrorAOVModel)
            .insert("label", "Pixel Error");
}

DictionaryArray PixelErrorAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> PixelErrorAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new PixelErrorAOV(params));
}

}   // namespace renderer
