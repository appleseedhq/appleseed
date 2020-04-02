
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Kevin Masson, The appleseedhq Organization
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
#include "pixelvariationaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/colormap.h"
#include "foundation/image/colormapdata.h"
#include "foundation/image/image.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Pixel Variation AOV.
    //

    const char* PixelVariationAOVModel = "pixel_variation_aov";

    class PixelVariationAOV
      : public UnfilteredAOV
    {
      public:
        explicit PixelVariationAOV(const ParamArray& params)
          : UnfilteredAOV("pixel_variation", params)
        {
        }

        void post_process_image(const Frame& frame) override
        {
            const AABB2u& crop_window = frame.get_crop_window();

            ColorMap color_map;
            color_map.set_palette_from_array(InfernoColorMapLinearRGB, countof(InfernoColorMapLinearRGB) / 3);

            // Clamps the pixel variation in the red channel between 0 and 1.
            // No normalization happens here.
            color_map.remap_red_channel(*m_image, crop_window, 0.0f, 1.0f);
        }

        const char* get_model() const override
        {
            return PixelVariationAOVModel;
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new AOVAccumulator());
        }
    };
}


//
// PixelVariationAOVFactory class implementation.
//

void PixelVariationAOVFactory::release()
{
    delete this;
}

const char* PixelVariationAOVFactory::get_model() const
{
    return PixelVariationAOVModel;
}

Dictionary PixelVariationAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", PixelVariationAOVModel)
            .insert("label", "Pixel Variation");
}

DictionaryArray PixelVariationAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> PixelVariationAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new PixelVariationAOV(params));
}

}   // namespace renderer
