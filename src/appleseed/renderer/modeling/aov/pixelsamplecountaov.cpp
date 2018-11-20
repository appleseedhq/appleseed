
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
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

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

namespace
{
    // Return the maximum sample/pixel count from `image`'s pixels inside of `crop_window`.
    float get_max_spp_count(
        const Image*  image,
        const AABB2u& crop_window)
    {
        float max_spp = 0.0f;

        for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
        {
            for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
            {
                Color3f color;
                image->get_pixel(x, y, color);
                max_spp = max(color[0], max_spp);
            }
        }

        return max_spp;
    }

    void fill_aov(
        Image*          image,
        const AABB2u&   crop_window,
        const Color3f&  color)
    {
        for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
        {
            for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
            {
                image->set_pixel(x, y, color);
            }
        }
    }
}

void PixelSampleCountAOV::post_process_image(const Frame& frame)
{
    static const Color3f Blue(0.0f, 0.0f, 1.0f);
    static const Color3f Red(1.0f, 0.0f, 0.0f);

    const AABB2u& crop_window = frame.get_crop_window();

    //
    // At this point, the AOV is filled with real sample/pixel count values.
    //
    // We want to normalize it so that high sample/pixel counts are red and
    // low sample/pixel counts are blue.
    //
    // If the Uniform Pixel Renderer is used, the AOV should be empty and
    // the exported AOV will be completely red.
    //
    // Otherwise, if the Adaptive Tile Renderer is used, we use the user's
    // min and max sample/pixel counts to determine the final color. If the
    // user's max sample/pixel count is 0 (infinite) then we use the actual
    // max sample/pixel count found in the image.
    //

    const float min_spp = static_cast<float>(m_min_spp);
    const float max_spp =
        m_max_spp == 0
            ? get_max_spp_count(m_image, crop_window)
            : static_cast<float>(m_max_spp);

    if (max_spp == 0.0f)
    {
        fill_aov(m_image, crop_window, Red);
        return;
    }

    assert(max_spp > min_spp);

    // Normalize.
    for (size_t y = crop_window.min.y; y <= crop_window.max.y; ++y)
    {
        for (size_t x = crop_window.min.x; x <= crop_window.max.x; ++x)
        {
            Color3f color;
            m_image->get_pixel(x, y, color);

            const float c = saturate(fit(color[0], min_spp, max_spp, 0.0f, 1.0f));

            m_image->set_pixel(x, y, lerp(Blue, Red, c));
        }
    }
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

auto_release_ptr<AOV> PixelSampleCountAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new PixelSampleCountAOV(params));
}

}   // namespace renderer
