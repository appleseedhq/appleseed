
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
        DiagnosticAOVAccumulator(
            Image& image,
            const string& diagnostic_name)
          : m_image(image)
          , m_diagnostic_name(diagnostic_name)
        {
        }

        void on_tile_end(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y)
        {
            for (size_t i = 0, e = frame.aov_images().size(); i < e; ++i)
            {
                const string aov_name = frame.aov_images().get_name(i);

                if (aov_name == m_diagnostic_name)
                {
                    const Image& image = frame.aov_images().get_image(i);
                    const Tile& diagnostic_tile = image.tile(tile_x, tile_y);
                    Tile& aov_tile = m_image.tile(tile_x, tile_y);

                    aov_tile.copy_from(diagnostic_tile);

                    return;
                }
            }
        }

      private:
        Image&              m_image;
        const string        m_diagnostic_name;
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
            string diagnostic_name = get_name();
            diagnostic_name += "_diagnostic";

            return auto_release_ptr<AOVAccumulator>(
                new DiagnosticAOVAccumulator(get_image(), diagnostic_name));
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
