
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "colormappostprocessingstage.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/postprocessingstage/colormapdata.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/messagecontext.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <exception>
#include <memory>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Color map post-processing stage.
    //

    const char* Model = "color_map_post_processing_stage";

    class ColorMapPostProcessingStage
      : public PostProcessingStage
    {
      public:
        ColorMapPostProcessingStage(
            const char*             name,
            const ParamArray&       params)
          : PostProcessingStage(name, params)
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

        void collect_asset_paths(StringArray& paths) const override
        {
            if (m_params.strings().exist("color_map_file_path"))
            {
                const char* filepath = m_params.get("color_map_file_path");
                if (!is_empty_string(filepath))
                    paths.push_back(filepath);
            }
        }

        void update_asset_paths(const StringDictionary& mappings) override
        {
            m_params.set("color_map_file_path", mappings.get(m_params.get("color_map_file_path")));
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            const OnFrameBeginMessageContext context("post-processing stage", this);

            const string color_map =
                m_params.get_optional<string>(
                    "color_map",
                    "inferno",
                    make_vector("inferno", "jet", "magma", "plasma", "viridis", "custom"),
                    context);

            if (color_map == "inferno")
                set_palette_from_array(InfernoColorMap, countof(InfernoColorMap) / 3);
            else if (color_map == "jet")
                set_palette_from_array(JetColorMap, countof(JetColorMap) / 3);
            else if (color_map == "magma")
                set_palette_from_array(MagmaColorMap, countof(MagmaColorMap) / 3);
            else if (color_map == "plasma")
                set_palette_from_array(PlasmaColorMap, countof(PlasmaColorMap) / 3);
            else if (color_map == "viridis")
                set_palette_from_array(ViridisColorMap, countof(ViridisColorMap) / 3);
            else
            {
                assert(color_map == "custom");

                const string color_map_filepath =
                    m_params.get_optional<string>("color_map_file_path", "", context);

                if (color_map_filepath.empty())
                {
                    RENDERER_LOG_ERROR("%sno file specified for custom color map.", context.get());
                    return false;
                }

                try
                {
                    set_palette_from_image_file(
                        to_string(
                            project.search_paths().qualify(color_map_filepath)));
                }
                catch (const exception& e)
                {
                    RENDERER_LOG_ERROR("%s%s", context.get(), e.what());
                    return false;
                }
            }

            m_auto_range = m_params.get_optional<bool>("auto_range", true);

            if (m_auto_range)
            {
                m_range_min = 0.0f;
                m_range_max = 0.0f;
            }
            else
            {
                m_range_min = m_params.get_required<float>("range_min", 0.0f);
                m_range_max = m_params.get_required<float>("range_max", 1.0f);
            }

            m_add_legend_bar = m_params.get_optional<bool>("add_legend_bar", true);

            return true;
        }

        void execute(Frame& frame) const override
        {
            remap_colors(frame);

            if (m_add_legend_bar)
                add_legend_bar(frame);
        }

      private:
        vector<Color3f>     m_palette;
        bool                m_auto_range;
        float               m_range_min;
        float               m_range_max;
        bool                m_add_legend_bar;

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

        void remap_colors(Frame& frame) const
        {
            float min_luminance, max_luminance;

            if (m_auto_range)
            {
                Color4f min_color, max_color;
                find_min_max(frame, min_color, max_color);

                min_luminance = luminance(min_color.rgb());
                max_luminance = luminance(max_color.rgb());

                RENDERER_LOG_DEBUG("min luminance: %f, max luminance: %f", min_luminance, max_luminance);
            }
            else
            {
                min_luminance = m_range_min;
                max_luminance = m_range_max;
            }

            if (min_luminance == max_luminance)
            {
                for_each_pixel(frame, [this, min_luminance, max_luminance](Color4f& color)
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

        void add_legend_bar(Frame& frame) const
        {
            Image& image = frame.image();
            const CanvasProperties& props = image.properties();

            // Can't add a vertical bar if the image is less than 2 pixels wide.
            if (props.m_canvas_width < 2)
                return;

            // Width of the legend bar in percents.
            const float LegendBarWidthPercent = 5.0f;   // width of the legend bar in percents
            const float LegendBarMinWidth = 1.0f;       // minimum width of the legend bar in pixels
            const float LegendBarMaxWidth = 30.0f;      // maximum width of the legend bar in pixels

            // Compute the width of the legend bar in pixels.
            const size_t legend_bar_width =
                truncate<size_t>(
                    clamp(
                        LegendBarWidthPercent * props.m_canvas_width / 100.0f,
                        LegendBarMinWidth,
                        LegendBarMaxWidth));

            // Draw the legend bar.
            for (size_t y = 0; y < props.m_canvas_height; ++y)
            {
                for (size_t x = props.m_canvas_width - legend_bar_width; x < props.m_canvas_width; ++x)
                {
                    image.set_pixel(
                        x, y,
                        Color4f(
                            evaluate_palette(
                                fit<size_t, float>(y, 0, props.m_canvas_height - 1, 1.0f, 0.0f)),
                            1.0f));
                }
            }
        }
    };
}


//
// ColorMapPostProcessingStageFactory class implementation.
//

void ColorMapPostProcessingStageFactory::release()
{
    delete this;
}

const char* ColorMapPostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary ColorMapPostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Color Map");
}

DictionaryArray ColorMapPostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "color_map")
            .insert("label", "Color Map")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Inferno", "inferno")
                    .insert("Jet", "jet")
                    .insert("Magma", "magma")
                    .insert("Plasma", "plasma")
                    .insert("Viridis", "viridis")
                    .insert("Custom", "custom"))
            .insert("use", "required")
            .insert("default", "inferno")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "color_map_file_path")
            .insert("label", "Colormap File Path")
            .insert("type", "file")
            .insert("file_picker_mode", "open")
            .insert("file_picker_type", "image")
            .insert("use", "optional")
            .insert("visible_if",
                Dictionary()
                    .insert("color_map", "custom")));

    metadata.push_back(
        Dictionary()
            .insert("name", "auto_range")
            .insert("label", "Auto Range")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "range_min")
            .insert("label", "Range Min")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("visible_if",
                Dictionary()
                    .insert("auto_range", "false")));

    metadata.push_back(
        Dictionary()
            .insert("name", "range_max")
            .insert("label", "Range Max")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("auto_range", "false")));

    metadata.push_back(
        Dictionary()
            .insert("name", "add_legend_bar")
            .insert("label", "Add Legend Bar")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true"));

    return metadata;
}

auto_release_ptr<PostProcessingStage> ColorMapPostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new ColorMapPostProcessingStage(name, params));
}

}   // namespace renderer
