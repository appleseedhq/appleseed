
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
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // PixelTime AOV accumulator.
    //

    class PixelTimeAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit PixelTimeAOVAccumulator(Image& image)
          : m_image(image)
        {
        }

        void on_tile_begin(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            const size_t                max_spp) override
        {
            // Fetch the destination tile.
            const CanvasProperties& props = frame.image().properties();
            m_tile = &m_image.tile(tile_x, tile_y);

            // Fetch the tile bounds (inclusive).
            m_tile_origin_x = static_cast<int>(tile_x * props.m_tile_width);
            m_tile_origin_y = static_cast<int>(tile_y * props.m_tile_height);
            m_tile_end_x = static_cast<int>(m_tile_origin_x + m_tile->get_width() - 1);
            m_tile_end_y = static_cast<int>(m_tile_origin_y + m_tile->get_height() - 1);

            m_samples.reserve(max_spp);
        }

        void on_sample_begin(const PixelContext& pixel_context) override
        {
            m_stopwatch.clear();
            m_stopwatch.start();
        }

        void on_sample_end(const PixelContext& pixel_context) override
        {
            m_stopwatch.measure();

            // Only collect samples inside the tile.
            if (!outside_tile(pixel_context.get_pixel_coords()))
                m_samples.push_back(m_stopwatch.get_seconds());
        }

        void on_pixel_begin(const Vector2i& pi) override
        {
            m_samples.clear();
        }

        void on_pixel_end(const Vector2i& pi) override
        {
            if (m_samples.empty())
                return;

            // Compute the median of all the sample times we collected.
            const size_t mid = m_samples.size() / 2;

            nth_element(
                m_samples.begin(),
                m_samples.begin() + mid,
                m_samples.end());

            const double median = m_samples[mid];

            float* p = reinterpret_cast<float*>(
                m_tile->pixel(pi.x - m_tile_origin_x, pi.y - m_tile_origin_y));

            *p += static_cast<float>(median) * m_samples.size();
        }

      private:
        Image&                              m_image;
        foundation::Tile*                   m_tile;

        int                                 m_tile_origin_x;
        int                                 m_tile_origin_y;
        int                                 m_tile_end_x;
        int                                 m_tile_end_y;

        Stopwatch<DefaultProcessorTimer>    m_stopwatch;
        std::vector<double>                 m_samples;

        bool outside_tile(const Vector2i& pi) const
        {
            return
                pi.x < m_tile_origin_x ||
                pi.y < m_tile_origin_y ||
                pi.x > m_tile_end_x ||
                pi.y > m_tile_end_y;
        }
    };


    //
    // PixelTime AOV.
    //

    const char* PixelTimeAOVModel = "pixel_time_aov";

    class PixelTimeAOV
      : public AOV
    {
      public:
        explicit PixelTimeAOV(const ParamArray& params)
          : AOV("pixel_time", params)
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
            return 1;
        }

        const char** get_channel_names() const override
        {
            static const char* ChannelNames[] = {"PixelTime"};
            return ChannelNames;
        }

        bool has_color_data() const override
        {
            return false;
        }

        void create_image(
            const size_t canvas_width,
            const size_t canvas_height,
            const size_t tile_width,
            const size_t tile_height,
            ImageStack&  aov_images) override
        {
            m_image =
                new Image(
                    canvas_width,
                    canvas_height,
                    tile_width,
                    tile_height,
                    get_channel_count(),
                    PixelFormatFloat);
        }

        void clear_image() override
        {
            m_image->clear(Color<float, 1>(0.0f));
        }

        void post_process_image() override
        {
            const CanvasProperties& src_props = m_image->properties();

            // Find the maximum value.
            float max_time = 0.0f;

            for (size_t j = 0; j < src_props.m_canvas_height; ++j)
            {
                for (size_t i = 0; i < src_props.m_canvas_width; ++i)
                {
                    Color<float, 1> val;
                    m_image->get_pixel(i, j, val);
                    max_time = max(val[0], max_time);
                }
            }

            if (max_time == 0.0f)
                return;

            const float rcp_max_time = 1.0f / max_time;

            // Normalize.
            for (size_t j = 0; j < src_props.m_canvas_height; ++j)
            {
                for (size_t i = 0; i < src_props.m_canvas_width; ++i)
                {
                    Color<float, 1> c;
                    m_image->get_pixel(i, j, c);

                    c[0] *= rcp_max_time;
                    m_image->set_pixel(i, j, c);
                }
            }
        }

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

auto_release_ptr<AOV> PixelTimeAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new PixelTimeAOV(params));
}

}   // namespace renderer
