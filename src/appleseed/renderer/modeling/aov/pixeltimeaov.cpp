
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "pixeltimeaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/colormap.h"
#include "foundation/image/colormapdata.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <vector>

using namespace foundation;

namespace renderer
{

namespace
{

    //
    // Pixel Time AOV accumulator.
    //

    class PixelTimeAOVAccumulator
      : public UnfilteredAOVAccumulator
    {
      public:
        explicit PixelTimeAOVAccumulator(Image& image)
          : UnfilteredAOVAccumulator(image)
        {
        }

        void on_tile_begin(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            const size_t                max_spp) override
        {
            UnfilteredAOVAccumulator::on_tile_begin(frame, tile_x, tile_y, max_spp);
            m_samples.reserve(max_spp);
        }

        void on_sample_begin(const PixelContext& pixel_context) override
        {
            m_stopwatch.start();
        }

        void on_sample_end(const PixelContext& pixel_context) override
        {
            // Only collect samples inside the tile.
            if (m_cropped_tile_bbox.contains(pixel_context.get_pixel_coords()))
            {
                m_stopwatch.measure();
                m_samples.push_back(m_stopwatch.get_seconds());
            }
        }

        void on_pixel_begin(const Vector2i& pi) override
        {
            UnfilteredAOVAccumulator::on_pixel_begin(pi);

            m_samples.clear();
        }

        void on_pixel_end(const Vector2i& pi) override
        {
            if (m_cropped_tile_bbox.contains(pi) && !m_samples.empty())
            {
                // Compute the median of all the sample times we collected.
                const size_t mid = m_samples.size() / 2;

                nth_element(
                    m_samples.begin(),
                    m_samples.begin() + mid,
                    m_samples.end());

                const double median = m_samples[mid];

                float* out =
                    reinterpret_cast<float*>(
                        m_tile->pixel(
                            pi.x - m_tile_origin_x,
                            pi.y - m_tile_origin_y));

                *out += static_cast<float>(median) * m_samples.size();
            }

            UnfilteredAOVAccumulator::on_pixel_end(pi);
        }

      private:
        Stopwatch<DefaultProcessorTimer>    m_stopwatch;
        std::vector<double>                 m_samples;
    };


    //
    // Pixel Time AOV.
    //

    const char* PixelTimeAOVModel = "pixel_time_aov";

    class PixelTimeAOV
      : public UnfilteredAOV
    {
      public:
        explicit PixelTimeAOV(const ParamArray& params)
          : UnfilteredAOV("pixel_time", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return PixelTimeAOVModel;
        }

        size_t get_channel_count() const override
        {
            return 3;
        }

        const char** get_channel_names() const override
        {
            static const char* ChannelNames[] = { "R", "G", "B" };
            return ChannelNames;
        }

        void clear_image() override
        {
            m_image->clear(Color<float, 3>(0.0f));
        }

        void post_process_image(const Frame& frame) override
        {
            const AABB2u& crop_window = frame.get_crop_window();

            ColorMap color_map;
            color_map.set_palette_from_array(InfernoColorMapLinearRGB, countof(InfernoColorMapLinearRGB) / 3);

            float min_time, max_time;
            color_map.find_min_max_red_channel(*m_image, crop_window, min_time, max_time);
            color_map.remap_red_channel(*m_image, crop_window, min_time, max_time);
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(new PixelTimeAOVAccumulator(get_image()));
        }
    };
}


//
// PixelTimeAOVFactory class implementation.
//

void PixelTimeAOVFactory::release()
{
    delete this;
}

const char* PixelTimeAOVFactory::get_model() const
{
    return PixelTimeAOVModel;
}

Dictionary PixelTimeAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", PixelTimeAOVModel)
            .insert("label", "Pixel Time");
}

DictionaryArray PixelTimeAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> PixelTimeAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new PixelTimeAOV(params));
}

}   // namespace renderer
