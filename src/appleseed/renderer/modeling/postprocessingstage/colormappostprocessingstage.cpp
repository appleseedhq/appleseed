
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
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/messagecontext.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colormap.h"
#include "foundation/image/colormapdata.h"
#include "foundation/image/conversion.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/text/textrenderer.h"
#include "foundation/math/aabb.h"
#include "foundation/math/distance.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <exception>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;

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
            if (m_params.strings().exist("color_map_file_path"))
            {
                const char* filepath = m_params.get("color_map_file_path");
                if (!is_empty_string(filepath))
                    m_params.set("color_map_file_path", mappings.get(m_params.get("color_map_file_path")));
            }
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            const OnFrameBeginMessageContext context("post-processing stage", this);

            const std::string color_map =
                m_params.get_optional<std::string>(
                    "color_map",
                    "inferno",
                    make_vector("inferno", "jet", "magma", "plasma", "viridis", "turbo", "custom"),
                    context);

            if (color_map == "inferno")
                m_color_map.set_palette_from_array(InfernoColorMapLinearRGB, countof(InfernoColorMapLinearRGB) / 3);
            else if (color_map == "jet")
                m_color_map.set_palette_from_array(JetColorMapLinearRGB, countof(JetColorMapLinearRGB) / 3);
            else if (color_map == "magma")
                m_color_map.set_palette_from_array(MagmaColorMapLinearRGB, countof(MagmaColorMapLinearRGB) / 3);
            else if (color_map == "plasma")
                m_color_map.set_palette_from_array(PlasmaColorMapLinearRGB, countof(PlasmaColorMapLinearRGB) / 3);
            else if (color_map == "viridis")
                m_color_map.set_palette_from_array(ViridisColorMapLinearRGB, countof(ViridisColorMapLinearRGB) / 3);
            else if (color_map == "turbo")
                m_color_map.set_palette_from_array(TurboColorMapLinearRGB, countof(TurboColorMapLinearRGB) / 3);
            else
            {
                assert(color_map == "custom");

                const std::string color_map_filepath =
                    m_params.get_optional<std::string>("color_map_file_path", "", context);

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
                catch (const std::exception& e)
                {
                    RENDERER_LOG_ERROR("%s%s", context.get(), e.what());
                    return false;
                }
            }

            m_auto_range = m_params.get_optional<bool>("auto_range", true, context);

            if (m_auto_range)
            {
                m_range_min = 0.0f;
                m_range_max = 0.0f;
            }
            else
            {
                m_range_min = m_params.get_required<float>("range_min", 0.0f, context);
                m_range_max = m_params.get_required<float>("range_max", 1.0f, context);
            }

            m_add_legend_bar = m_params.get_optional<bool>("add_legend_bar", true, context);
            m_legend_bar_ticks = m_params.get_optional<size_t>("legend_bar_ticks", 8, context);

            m_render_isolines = m_params.get_optional<bool>("render_isolines", false, context);
            m_line_thickness = m_params.get_optional<float>("line_thickness", 1.0f, context);

            return true;
        }

        void execute(Frame& frame) const override
        {
            float min_luminance, max_luminance;

            if (m_auto_range)
            {
                m_color_map.find_min_max_relative_luminance(
                    frame.image(),
                    frame.get_crop_window(),
                    min_luminance,
                    max_luminance);
            }
            else
            {
                min_luminance = m_range_min;
                max_luminance = m_range_max;
            }

            RENDERER_LOG_INFO(
                "post-processing stage \"%s\":\n"
                "  min luminance                 %f\n"
                "  max luminance                 %f",
                get_path().c_str(),
                min_luminance,
                max_luminance);

            SegmentVector isoline_segments;

            if (m_render_isolines)
                collect_isoline_segments(isoline_segments, frame, min_luminance, max_luminance);

            m_color_map.remap_relative_luminance(
                frame.image(),
                frame.get_crop_window(),
                min_luminance,
                max_luminance);

            if (m_render_isolines)
                render_isoline_segments(frame, isoline_segments);

            if (m_add_legend_bar)
                add_legend_bar(frame, min_luminance, max_luminance);
        }

      private:
        struct Segment
        {
            Vector2f    m_a;
            Vector2f    m_b;

            Segment()
            {
            }

            Segment(const Segment& rhs)
              : m_a(rhs.m_a)
              , m_b(rhs.m_b)
            {
            }

            Segment(
                const Vector2f& a,
                const Vector2f& b)
              : m_a(a)
              , m_b(b)
            {
            }
        };

        typedef std::vector<Segment> SegmentVector;

        ColorMap            m_color_map;
        bool                m_auto_range;
        float               m_range_min;
        float               m_range_max;
        bool                m_add_legend_bar;
        size_t              m_legend_bar_ticks;
        bool                m_render_isolines;
        float               m_line_thickness;

        void set_palette_from_image_file(const std::string& file_path)
        {
            GenericImageFileReader reader;
            std::unique_ptr<Image> image(reader.read(file_path.c_str()));

            if (!is_linear_image_file_format(file_path))
                convert_srgb_to_linear_rgb(*image);

            m_color_map.set_palette_from_image_file(*image.get());
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
                const float val =
                    y0 == y1 - 1
                        ? 0.0f
                        : fit<size_t, float>(y, y0, y1 - 1, 1.0f, 0.0f);

                const Color4f color(m_color_map.evaluate_palette(val), 1.0f);

                for (size_t x = x0; x < x1; ++x)
                    image.set_pixel(x, y, color);
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
                const std::string label = to_string(lum);

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

        //
        // Isolines rendering.
        //

        void collect_isoline_segments(
            SegmentVector&              segments,
            const Frame&                frame,
            const float                 min_luminance,
            const float                 max_luminance) const
        {
            const Image& image = frame.image();
            const CanvasProperties& props = image.properties();

            if (props.m_canvas_width <= 1 || props.m_canvas_height <= 1)
                return;

            Stopwatch<DefaultWallclockTimer> sw(0);
            sw.start();

            for (size_t level = 0; level < m_legend_bar_ticks; ++level)
            {
                const float isovalue =
                    fit<size_t, float>(level, 0, m_legend_bar_ticks - 1, min_luminance, max_luminance);

                for (size_t y = 0, end_y = props.m_canvas_height - 1; y < end_y; ++y)
                {
                    for (size_t x = 0, end_x = props.m_canvas_width - 1; x < end_x; ++x)
                        find_isoline_segment(image, props, x, y, isovalue, segments);
                }
            }

            sw.measure();
            RENDERER_LOG_DEBUG("post-processing stage \"%s\": isolines detection (" FMT_SIZE_T " segment%s) executed in %s.",
                get_path().c_str(),
                segments.size(),
                segments.size() > 1 ? "s" : "",
                pretty_time(sw.get_seconds()).c_str());
        }

        void render_isoline_segments(
            Frame&                      frame,
            const SegmentVector&        segments) const
        {
            Image& image = frame.image();
            const CanvasProperties& props = image.properties();

            Stopwatch<DefaultWallclockTimer> sw(0);
            sw.start();

            for (const Segment& seg : segments)
                rasterize_isoline_segment(image, props, seg);

            sw.measure();
            RENDERER_LOG_DEBUG("post-processing stage \"%s\": isolines rendering (" FMT_SIZE_T " segment%s) executed in %s.",
                get_path().c_str(),
                segments.size(),
                segments.size() > 1 ? "s" : "",
                pretty_time(sw.get_seconds()).c_str());
        }

        static void find_isoline_segment(
            const Image&                image,
            const CanvasProperties&     props,
            const size_t                x,
            const size_t                y,
            const float                 isovalue,
            SegmentVector&              segments)
        {
            //
            // Reference:
            //
            //   https://en.wikipedia.org/wiki/Marching_squares
            //

            assert(x < props.m_canvas_width - 1);
            assert(y < props.m_canvas_height - 1);

            // Retrieve value at each pixel of 2x2 block with (x, y) as top-left corner.
            const float f00 = get_pixel_value(image, props, x + 0, y + 0);
            const float f10 = get_pixel_value(image, props, x + 1, y + 0);
            const float f01 = get_pixel_value(image, props, x + 0, y + 1);
            const float f11 = get_pixel_value(image, props, x + 1, y + 1);

            // Corners are numbered clockwise starting in top-left corner.
            const size_t b00 = f00 > isovalue ? 1UL << 3 : 0;
            const size_t b10 = f10 > isovalue ? 1UL << 2 : 0;
            const size_t b11 = f11 > isovalue ? 1UL << 1 : 0;
            const size_t b01 = f01 > isovalue ? 1UL << 0 : 0;
            const size_t mask = b00 + b10 + b11 + b01;

            // Compute middle of top edge.
            const auto mt = [x, y, f00, f10, isovalue]()
            {
                return Vector2f(
                    static_cast<float>(x) + inverse_lerp(f00, f10, isovalue),
                    static_cast<float>(y) + 0.0f);
            };

            // Compute middle of right edge.
            const auto mr = [x, y, f10, f11, isovalue]()
            {
                return Vector2f(
                    static_cast<float>(x) + 1.0f,
                    static_cast<float>(y) + inverse_lerp(f10, f11, isovalue));
            };

            // Compute middle of bottom edge.
            const auto mb = [x, y, f01, f11, isovalue]()
            {
                return Vector2f(
                    static_cast<float>(x) + inverse_lerp(f01, f11, isovalue),
                    static_cast<float>(y) + 1.0f);
            };

            // Compute middle of left edge.
            const auto ml = [x, y, f00, f01, isovalue]()
            {
                return Vector2f(
                    static_cast<float>(x) + 0.0f,
                    static_cast<float>(y) + inverse_lerp(f00, f01, isovalue));
            };

            switch (mask)
            {
              case 0x0 /* 0b0000 */: break;
              case 0x1 /* 0b0001 */: segments.emplace_back(ml(), mb()); break;
              case 0x2 /* 0b0010 */: segments.emplace_back(mb(), mr()); break;
              case 0x3 /* 0b0011 */: segments.emplace_back(ml(), mr()); break;
              case 0x4 /* 0b0100 */: segments.emplace_back(mr(), mt()); break;
              case 0x5 /* 0b0101 */:
                if (f00 + f10 + f01 + f11 < 4.0f * isovalue)
                {
                    segments.emplace_back(mr(), mt());
                    segments.emplace_back(ml(), mb());
                }
                else
                {
                    segments.emplace_back(ml(), mt());
                    segments.emplace_back(mr(), mb());
                }
                break;
              case 0x6 /* 0b0110 */: segments.emplace_back(mb(), mt()); break;
              case 0x7 /* 0b0111 */: segments.emplace_back(ml(), mt()); break;
              case 0x8 /* 0b1000 */: segments.emplace_back(mt(), ml()); break;
              case 0x9 /* 0b1001 */: segments.emplace_back(mt(), mb()); break;
              case 0xA /* 0b1010 */:
                if (f00 + f10 + f01 + f11 < 4.0f * isovalue)
                {
                    segments.emplace_back(mt(), ml());
                    segments.emplace_back(mb(), mr());
                }
                else
                {
                    segments.emplace_back(mt(), mr());
                    segments.emplace_back(mb(), ml());
                }
                break;
              case 0xB /* 0b1011 */: segments.emplace_back(mt(), mr()); break;
              case 0xC /* 0b1100 */: segments.emplace_back(mr(), ml()); break;
              case 0xD /* 0b1101 */: segments.emplace_back(mr(), mb()); break;
              case 0xE /* 0b1110 */: segments.emplace_back(mb(), ml()); break;
              case 0xF /* 0b1111 */: break;
              assert_otherwise;
            }
        }

        static float get_pixel_value(
            const Image&                image,
            const CanvasProperties&     props,
            const size_t                x,
            const size_t                y)
        {
            assert(x < props.m_canvas_width);
            assert(y < props.m_canvas_height);

            Color4f c;
            image.get_pixel(x, y, c);

            return luminance(c.rgb());
        }

        void rasterize_isoline_segment(
            Image&                      image,
            const CanvasProperties&     props,
            const Segment&              seg) const
        {
            const Color4f SegmentColor(1.0f, 1.0f, 1.0f, 0.8f);     // segment color in linear RGB
            const size_t SubpixelGridSize = 4;
            const float RcpSubpixelCount = 1.0f / square(SubpixelGridSize);
            const float HalfSubpixel = 0.5f / SubpixelGridSize;
            const float Eps = 1.0e-6f;

            const float seg_radius = 0.5f * m_line_thickness;
            const float seg_square_radius = square(seg_radius);

            // Compute bounding box of segment, accounting for its thickness.
            AABB2f bbox;
            bbox.invalidate();
            bbox.insert(seg.m_a);
            bbox.insert(seg.m_b);
            bbox.grow(Vector2f(seg_radius + Eps));

            // Compute bounding box of segment in pixels.
            int ixmin = truncate<int>(fast_floor(bbox.min.x));
            int ixmax = truncate<int>(fast_floor(bbox.max.x));      // used to be fast_ceil() but this is probably correct
            int iymin = truncate<int>(fast_floor(bbox.min.y));
            int iymax = truncate<int>(fast_floor(bbox.max.y));
            assert(ixmin <= ixmax);
            assert(iymin <= iymax);

            // Clamp bounding box to the image.
            const int iw = static_cast<int>(props.m_canvas_width);
            const int ih = static_cast<int>(props.m_canvas_height);
            if (ixmin < 0) ixmin = 0;
            if (iymin < 0) iymin = 0;
            if (ixmax > iw - 1) ixmax = iw - 1;
            if (iymax > ih - 1) iymax = ih - 1;

            // Iterate over pixels.
            for (size_t py = iymin; py <= iymax; ++py)
            {
                const float fy = static_cast<float>(py);

                for (size_t px = ixmin; px <= ixmax; ++px)
                {
                    const float fx = static_cast<float>(px);

                    // Number of subpixels covered by the segment.
                    size_t hits = 0;

                    // Iterate over subpixels.
                    for (size_t sy = 0; sy < SubpixelGridSize; ++sy)
                    {
                        Vector2f p;
                        p.y =
                            fit<size_t, float>(
                                sy, 0, SubpixelGridSize - 1,
                                fy + HalfSubpixel, fy + 1.0f - HalfSubpixel);

                        for (size_t sx = 0; sx < SubpixelGridSize; ++sx)
                        {
                            p.x =
                                fit<size_t, float>(
                                    sx, 0, SubpixelGridSize - 1,
                                    fx + HalfSubpixel, fx + 1.0f - HalfSubpixel);

                            const float d2 = square_distance_point_segment(p, seg.m_a, seg.m_b);

                            if (d2 <= seg_square_radius)
                                ++hits;
                        }
                    }

                    if (hits == 0)
                        continue;

                    const float alpha = static_cast<float>(hits) * RcpSubpixelCount;

                    // Retrieve background color.
                    Color4f background_premult;
                    image.get_pixel(px, py, background_premult);

                    // Compute pixel color.
                    Color4f color_premult = SegmentColor;
                    color_premult.a *= alpha;
                    color_premult.premultiply_in_place();

                    // Composite pixel color over background.
                    color_premult += background_premult * (1.0f - color_premult.a);

                    // Write final color to image.
                    image.set_pixel(px, py, color_premult);
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
                    .insert("Turbo", "turbo")
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

    metadata.push_back(
        Dictionary()
            .insert("name", "legend_bar_ticks")
            .insert("label", "Legend Bar Ticks")
            .insert("type", "integer")
            .insert("min",
                Dictionary()
                    .insert("value", "2")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "64")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "8")
            .insert("visible_if",
                Dictionary()
                    .insert("add_legend_bar", "true")));

    metadata.push_back(
        Dictionary()
            .insert("name", "render_isolines")
            .insert("label", "Render Isolines")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "false")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "line_thickness")
            .insert("label", "Line Thickness")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.5")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "5.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("render_isolines", "true")));

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
