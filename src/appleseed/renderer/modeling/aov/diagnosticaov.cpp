
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
#include "diagnosticaov.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Diagnostic AOV accumulator.
    //

    class DiagnosticAOVAccumulator
      : public AOVAccumulator
    {
      public:
        DiagnosticAOVAccumulator()
        {
        }
    };

    //
    // Invalid Sample AOV accumulator.
    //
    //

    const uint8 NoState = 0;
    const uint8 InvalidSample = 1;
    const uint8 CorrectSample = 2;

    class InvalidSampleAOVAccumulator
      : public AOVAccumulator
    {
      public:
        explicit InvalidSampleAOVAccumulator(
            Image& image)
          : m_image(image)
          , m_invalid_sample_count(0)
        {
        }

        void on_tile_begin(
                const Frame&                frame,
                const size_t                tile_x,
                const size_t                tile_y,
                const size_t                max_spp) override
        {
            // Create a tile that stores samples hint.
            const Tile& tile = frame.image().tile(tile_x, tile_y);

            m_invalid_sample_tile.reset(
                new Tile(tile.get_width(), tile.get_height(), 1, PixelFormatUInt8));
            m_invalid_sample_tile->clear(Color<uint8, 1>(0));

            const CanvasProperties& props = frame.image().properties();

            m_tile_origin_x = static_cast<int>(tile_x * props.m_tile_width);
            m_tile_origin_y = static_cast<int>(tile_y * props.m_tile_height);
            m_tile_end_x = static_cast<int>(m_tile_origin_x + m_invalid_sample_tile->get_width() - 1);
            m_tile_end_y = static_cast<int>(m_tile_origin_y + m_invalid_sample_tile->get_height() - 1);
        }

        void on_tile_end(
                const Frame&                frame,
                const size_t                tile_x,
                const size_t                tile_y) override
        {
            // Fill the tile according to samples state.
            const Tile& tile = frame.image().tile(tile_x, tile_y);
            Tile& aov_tile = m_image.tile(tile_x, tile_y);

            const size_t width = tile.get_width();
            const size_t height = tile.get_height();

            for (size_t y = 0; y < height; ++y)
            {
                for (size_t x = 0; x < width; ++x)
                {
                    Color<uint8, 1> sample_state;
                    m_invalid_sample_tile->get_pixel(x, y, sample_state);

                    Color4f color;

                    switch (sample_state[0])
                    {
                      case NoState:
                        color = Color4f(1.0f, 0.0f, 0.0f, 1.0f);
                        break;

                      case InvalidSample:
                        color = Color4f(1.0f, 0.0f, 1.0f, 1.0f);
                        break;

                      case CorrectSample:
                        tile.get_pixel(x, y, color);
                        color.rgb().set(0.2f * luminance(color.rgb()));     // 20% of luminance
                        color.a = 1.0f;
                        break;

                      assert_otherwise;
                    }

                    aov_tile.set_pixel(x, y, color);
                }
            }
        }

        void on_pixel_begin(const Vector2i& pi) override
        {
            m_invalid_sample_count = 0;
        }

        void on_pixel_end(const Vector2i& pi) override
        {
            // Store a hint corresping to the sample state in the tile.
            if (pi.x >= m_tile_origin_x &&
                pi.y >= m_tile_origin_y &&
                pi.x <= m_tile_end_x &&
                pi.y <= m_tile_end_y)
            {
                const Vector2i pt(pi.x - m_tile_origin_x, pi.y - m_tile_origin_y);
                m_invalid_sample_tile->set_pixel(pt.x, pt.y,
                    m_invalid_sample_count > 0 ? &InvalidSample : &CorrectSample);
            }
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            // Detect invalid samples.
            static const size_t MaxWarningsPerThread = 5;

            if (!shading_result.is_valid())
            {
                m_invalid_sample_count++;

                if (m_invalid_sample_count <= MaxWarningsPerThread)
                {
                    RENDERER_LOG_WARNING(
                        "%s sample%s at pixel (%s, %s) had nan, negative or infinite components",
                       pretty_uint(m_invalid_sample_count).c_str(),
                       m_invalid_sample_count > 1 ? "s" : "",
                       pretty_uint(pixel_context.get_pixel_coords().x).c_str(),
                       pretty_uint(pixel_context.get_pixel_coords().y).c_str());
                }
                else if (m_invalid_sample_count == MaxWarningsPerThread + 1)
                {
                    RENDERER_LOG_WARNING("more invalid samples found, omitting warning messages for brevity.");
                }
            }
        }

      private:
        Image&                                  m_image;
        size_t                                  m_invalid_sample_count;
        std::unique_ptr<foundation::Tile>       m_invalid_sample_tile;
        int                                     m_tile_origin_x;
        int                                     m_tile_origin_y;
        int                                     m_tile_end_x;
        int                                     m_tile_end_y;
    };

    //
    // Diagnostic AOV.
    //

    class DiagnosticAOV
      : public AOV
    {
      public:
        explicit DiagnosticAOV(const char* name, const ParamArray& params)
          : AOV(name, params)
        {
        }

        void release() override
        {
            delete this;
        }

        size_t get_channel_count() const override
        {
            return 4;
        }

        const char** get_channel_names() const override
        {
            static const char* ChannelNames[] = {"R", "G", "B", "A"};
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
            m_image->clear(Color4f(0.0f, 0.0f, 0.0f, 1.0f));
        }

        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new DiagnosticAOVAccumulator());
        }
    };

    //
    // Invalid Sample AOV.
    //

    const char* Invalid_Sample_Model = "invalid_sample_aov";

    class InvalidSampleAOV
      : public DiagnosticAOV
    {
      public:
        explicit InvalidSampleAOV(const ParamArray& params)
          : DiagnosticAOV("invalid_sample", params)
        {
        }

        const char* get_model() const override
        {
            return Invalid_Sample_Model;
        }

        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new InvalidSampleAOVAccumulator(get_image()));
        }
    };

    //
    // Pixel Sample AOV.
    //

    const char* Pixel_Sample_Model = "pixel_sample_aov";

    class PixelSampleAOV
      : public DiagnosticAOV
    {
      public:
        explicit PixelSampleAOV(const ParamArray& params)
          : DiagnosticAOV("pixel_sample", params)
        {
        }

        const char* get_model() const override
        {
            return Pixel_Sample_Model;
        }
    };

    //
    // Pixel Variation AOV.
    //

    const char* Pixel_Variation_Model = "pixel_variation_aov";

    class PixelVariationAOV
      : public DiagnosticAOV
    {
      public:
        explicit PixelVariationAOV(const ParamArray& params)
          : DiagnosticAOV("pixel_variation", params)
        {
        }

        const char* get_model() const override
        {
            return Pixel_Variation_Model;
        }
    };
}


//
// InvalidSampleAOVFactory class implementation.
//

void InvalidSampleAOVFactory::release()
{
    delete this;
}

const char* InvalidSampleAOVFactory::get_model() const
{
    return Invalid_Sample_Model;
}

Dictionary InvalidSampleAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Invalid_Sample_Model)
            .insert("label", "Invalid Sample");
}

DictionaryArray InvalidSampleAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> InvalidSampleAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new InvalidSampleAOV(params));
}


//
// PixelSampleAOVFactory class implementation.
//

void PixelSampleAOVFactory::release()
{
    delete this;
}

const char* PixelSampleAOVFactory::get_model() const
{
    return Pixel_Sample_Model;
}

Dictionary PixelSampleAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Pixel_Sample_Model)
            .insert("label", "Pixel Sample");
}

DictionaryArray PixelSampleAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> PixelSampleAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new PixelSampleAOV(params));
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
    return Pixel_Variation_Model;
}

Dictionary PixelVariationAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Pixel_Variation_Model)
            .insert("label", "Pixel Variation");
}

DictionaryArray PixelVariationAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> PixelVariationAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new PixelVariationAOV(params));
}

}   // namespace renderer
