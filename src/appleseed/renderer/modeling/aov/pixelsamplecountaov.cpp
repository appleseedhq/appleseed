
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
#include "pixelsamplecountaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingresult.h"
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
#include <memory>
#include <string>

using namespace foundation;

namespace renderer
{

//
// PixelSampleCountAOV class implementation.
//

namespace
{
    const char* PixelSampleCountAOVModel = "pixel_sample_count_aov";
}

PixelSampleCountAOV::PixelSampleCountAOV(const ParamArray& params)
  : UnfilteredAOV("pixel_sample_count", params)
  , m_min_spp(0)
  , m_max_spp(0)
{
}

const char* PixelSampleCountAOV::get_model() const
{
    return PixelSampleCountAOVModel;
}

void PixelSampleCountAOV::post_process_image(const Frame& frame)
{
    ColorMap color_map;
    color_map.set_palette_from_array(InfernoColorMapLinearRGB, countof(InfernoColorMapLinearRGB) / 3);

    //
    // At this point, the AOV is filled with real sample/pixel count values.
    //
    // If the Uniform Pixel Renderer is used, the AOV should be empty and
    // the exported AOV will be of a solid color depending on the used
    // color map.
    //
    // Otherwise, if the Adaptive Tile Renderer is used, we use the user's
    // min and max sample/pixel counts to determine the final color. If the
    // user's max sample/pixel count is 0 (infinite) then we use the actual
    // max sample/pixel count found in the image.
    //

    const AABB2u& crop_window = frame.get_crop_window();

    float min_spp, max_spp;
    if (m_max_spp == 0)
        color_map.find_min_max_red_channel(*m_image, crop_window, min_spp, max_spp);
    else
        max_spp = static_cast<float>(m_max_spp);
    min_spp = static_cast<float>(m_min_spp);

    color_map.remap_red_channel(*m_image, crop_window, min_spp, max_spp);
}

void PixelSampleCountAOV::set_normalization_range(
    const size_t        min_spp,
    const size_t        max_spp)
{
    m_min_spp = min_spp;
    m_max_spp = max_spp;
}

auto_release_ptr<AOVAccumulator> PixelSampleCountAOV::create_accumulator() const
{
    return auto_release_ptr<AOVAccumulator>(new AOVAccumulator());
}


//
// PixelSampleCountAOVFactory class implementation.
//

void PixelSampleCountAOVFactory::release()
{
    delete this;
}

const char* PixelSampleCountAOVFactory::get_model() const
{
    return PixelSampleCountAOVModel;
}

Dictionary PixelSampleCountAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", PixelSampleCountAOVModel)
            .insert("label", "Pixel Sample Count");
}

DictionaryArray PixelSampleCountAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> PixelSampleCountAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new PixelSampleCountAOV(params));
}

}   // namespace renderer
