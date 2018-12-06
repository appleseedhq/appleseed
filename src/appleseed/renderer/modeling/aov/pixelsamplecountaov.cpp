
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
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/text/textrenderer.h"
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
// Color mapping
//

class ColorMap
{
    vector<Color3f>     m_palette;
    size_t              m_legend_bar_ticks;

    template <typename Func>
    void for_each_pixel(const Frame& frame, const Func& func) const
    {
        foundation::Image& image = frame.image();
        const foundation::CanvasProperties& frame_props = image.properties();
        for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
        {
            for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
            {
                foundation::Tile& tile = image.tile(tx, ty);

                for (size_t y = 0, th = tile.get_height(); y < th; ++y)
                {
                    for (size_t x = 0, tw = tile.get_width(); x < tw; ++x)
                    {
                        foundation::Color4f color;
                        tile.get_pixel(x, y, color);
                        func(color);
                        tile.set_pixel(x, y, color);
                    }
                }
            }
        }
    }

    void set_palette_from_array(const float* values, const size_t entry_count)
    {
        m_palette.resize(entry_count);

        for (size_t i = 0; i < entry_count; ++i)
            {
                m_palette[i] =
                    Color3f(
                        values[i * 3 + 0],
                        values[i * 3 + 1],
                        values[i * 3 + 2]);
            }
    }

    void set_palette_from_image_file(const string& filepath)
    {
        GenericImageFileReader reader;
        unique_ptr<Image> image(reader.read(filepath.c_str()));

        const size_t image_width = image->properties().m_canvas_width;
        m_palette.resize(image_width);

        for (size_t i = 0; i < image_width; ++i)
            image->get_pixel(i, 0, m_palette[i]);
    }

    Color3f evaluate_palette(float x) const
    {
        assert(m_palette.size() > 1);

        x *= m_palette.size() - 1;

        const size_t ix = min(truncate<size_t>(x), m_palette.size() - 2);
        const float w = x - ix;

        return lerp(m_palette[ix], m_palette[ix + 1], w);
    }

    void remap_colors(Frame& frame, const float min_luminance, const float max_luminance) const
    {
        if (min_luminance == max_luminance)
        {
            for_each_pixel(frame, [this](Color4f& color)
            {
                color.rgb() = evaluate_palette(0.0f);
            });
        }
        else
        {
            for_each_pixel(frame, [this, min_luminance, max_luminance](Color4f& color)
            {
                const float col_luminance = luminance(color.rgb());

                const float x =
                    saturate(
                        inverse_lerp(
                            min_luminance,
                            max_luminance,
                            col_luminance));

                color.rgb() = evaluate_palette(x);
            });
        }
    }

    void add_legend_bar(Frame& frame, const float min_luminance, const float max_luminance) const
    {
        // Legend bar settings.
        const float LegendBarWidthPercent = 5.0f;               // width in percents of the legend bar
        const float LegendBarMinWidth = 1.0f;                   // minimum width in pixels of the legend bar
        const float LegendBarMaxWidth = 30.0f;                  // maximum width in pixels of the legend bar
        const float LegendBarLeftMargin = 10.0f;                // margin in pixels on the left of the legend bar
        const size_t LegendBarVerticalMargin = 22;              // margin in pixels at top and bottom of the legend bar
        const size_t TickMarkLength = 6;                        // length in pixel of tick marks
        const Color4f TickMarkColor(1.0f, 1.0f, 1.0f, 1.0f);    // color of tick marks in linear RGB
        const auto LabelFont = TextRenderer::Font::UbuntuL;     // font for tick labels
        const float LabelFontHeight = 14.0f;                    // height in pixel of tick labels
        const Color4f LabelFontColor(1.0f, 1.0f, 1.0f, 1.0f);   // color of tick labels in linear RGB

        Image& image = frame.image();
        const CanvasProperties& props = image.properties();

        // Compute the width of the legend bar in pixels.
        const size_t legend_bar_width =
            truncate<size_t>(
                clamp(
                    LegendBarWidthPercent * props.m_canvas_width / 100.0f,
                    LegendBarMinWidth,
                    LegendBarMaxWidth));

        // Handle edge cases.
        if (props.m_canvas_width <= legend_bar_width ||
            props.m_canvas_height <= 2 * LegendBarVerticalMargin)
            return;

        // Compute bounds of the legend bar.
        const size_t x0 = props.m_canvas_width - legend_bar_width;
        const size_t x1 = props.m_canvas_width;
        const size_t y0 = LegendBarVerticalMargin;
        const size_t y1 = props.m_canvas_height - LegendBarVerticalMargin;
        assert(x0 < x1);
        assert(y0 < y1);

        // Draw legend bar.
        for (size_t y = y0; y < y1; ++y)
        {
            for (size_t x = x0; x < x1; ++x)
            {
                const float val =
                    y0 == y1 - 1
                        ? 0.0f
                        : fit<size_t, float>(y, y0, y1 - 1, 1.0f, 0.0f);

                image.set_pixel(
                    x, y,
                    Color4f(
                        evaluate_palette(val),
                        1.0f));
            }
        }

        // Handle more edge cases.
        if (x0 < TickMarkLength)
            return;

        // Draw ticks.
        for (size_t i = 0; i < m_legend_bar_ticks; ++i)
        {
            const size_t y = y0 + i * (y1 - y0 - 1) / (m_legend_bar_ticks - 1);
            assert(y >= y0 && y < y1);

            // Draw tick mark.
            for (size_t x = x0 - TickMarkLength; x < x0; ++x)
                image.set_pixel(x, y, TickMarkColor);

            const float lum =
                fit<size_t, float>(i, 0, m_legend_bar_ticks - 1, max_luminance, min_luminance);
            const string label = to_string(lum);

            const float label_width =
                TextRenderer::compute_string_width(LabelFont, LabelFontHeight, label.c_str());
            const float label_height =
                TextRenderer::compute_string_height(LabelFontHeight, label.c_str());

            // Draw tick label.
            TextRenderer::draw_string(
                image,
                LabelFont,
                LabelFontHeight,
                LabelFontColor,
                static_cast<float>(x0 - label_width - LegendBarLeftMargin),
                static_cast<float>(y) - label_height / 2.0f + 1.0f,
                label.c_str());
        }
    }    
};

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
