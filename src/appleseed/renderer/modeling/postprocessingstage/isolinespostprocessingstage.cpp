
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
#include "isolinespostprocessingstage.h"

// appleseed.renderer headers.
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/aabb.h"
#include "foundation/math/distance.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    const char* Model = "isolines_post_processing_stage";

    class IsolinesPostProcessingStage
      : public PostProcessingStage
    {
      public:
        IsolinesPostProcessingStage(
            const char*         name,
            const ParamArray&   params)
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

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            const OnFrameBeginMessageContext context("post-processing stage", this);

            m_low_isovalue = m_params.get_required<float>("low_isovalue", 0.0f, context);
            m_high_isovalue = m_params.get_required<float>("high_isovalue", 1.0f, context);
            m_levels = m_params.get_optional<size_t>("levels", 8, context);
            m_line_thickness = m_params.get_optional<float>("line_thickness", 1.0f, context);

            return true;
        }

        void execute(Frame& frame) const override
        {
            Image& image = frame.image();
            const CanvasProperties& props = image.properties();

            if (props.m_canvas_width <= 1 || props.m_canvas_height <= 1)
                return;

            SegmentVector segments;

            for (size_t level = 0; level < m_levels; ++level)
            {
                const float isovalue =
                    fit<size_t, float>(level, 0, m_levels - 1, m_low_isovalue, m_high_isovalue);

                for (size_t y = 0; y < props.m_canvas_height - 1; ++y)
                {
                    for (size_t x = 0; x < props.m_canvas_width - 1; ++x)
                        find_contour_segment(image, props, x, y, isovalue, segments);
                }
            }

            for (const Segment& seg : segments)
                rasterize(image, props, seg);
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

        typedef vector<Segment> SegmentVector;

        float   m_low_isovalue;
        float   m_high_isovalue;
        size_t  m_levels;
        float   m_line_thickness;

        static void find_contour_segment(
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
            const size_t b00 = f00 > isovalue ? 1 << 3 : 0;
            const size_t b10 = f10 > isovalue ? 1 << 2 : 0;
            const size_t b11 = f11 > isovalue ? 1 << 1 : 0;
            const size_t b01 = f01 > isovalue ? 1 << 0 : 0;
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
              case 0x1 /* 0b0001 */: segments.push_back(Segment(ml(), mb())); break;
              case 0x2 /* 0b0010 */: segments.push_back(Segment(mb(), mr())); break;
              case 0x3 /* 0b0011 */: segments.push_back(Segment(ml(), mr())); break;
              case 0x4 /* 0b0100 */: segments.push_back(Segment(mr(), mt())); break;
              case 0x5 /* 0b0101 */:
                  if (f00 + f10 + f01 + f11 < 4.0f * isovalue)
                  {
                      segments.push_back(Segment(mr(), mt()));
                      segments.push_back(Segment(ml(), mb()));
                  }
                  else
                  {
                      segments.push_back(Segment(ml(), mt()));
                      segments.push_back(Segment(mr(), mb()));
                  }
                  break;
              case 0x6 /* 0b0110 */: segments.push_back(Segment(mb(), mt())); break;
              case 0x7 /* 0b0111 */: segments.push_back(Segment(ml(), mt())); break;
              case 0x8 /* 0b1000 */: segments.push_back(Segment(mt(), ml())); break;
              case 0x9 /* 0b1001 */: segments.push_back(Segment(mt(), mb())); break;
              case 0xA /* 0b1010 */:
                  if (f00 + f10 + f01 + f11 < 4.0f * isovalue)
                  {
                      segments.push_back(Segment(mt(), ml()));
                      segments.push_back(Segment(mb(), mr()));
                  }
                  else
                  {
                      segments.push_back(Segment(mt(), mr()));
                      segments.push_back(Segment(mb(), ml()));
                  }
                  break;
              case 0xB /* 0b1011 */: segments.push_back(Segment(mt(), mr())); break;
              case 0xC /* 0b1100 */: segments.push_back(Segment(mr(), ml())); break;
              case 0xD /* 0b1101 */: segments.push_back(Segment(mr(), mb())); break;
              case 0xE /* 0b1110 */: segments.push_back(Segment(mb(), ml())); break;
              case 0xF /* 0b1111 */: break;
              assert_otherwise;
            }
        }

        void rasterize(
            Image&                      image,
            const CanvasProperties&     props,
            const Segment&              seg) const
        {
            const Color4f SegColorSRGB(1.0f, 1.0f, 1.0f, 0.8f);     // in sRGB color space
            const size_t SubpixelGridSize = 4;
            const float HalfSubpixel = 0.5f / SubpixelGridSize;
            const float Eps = 1.0e-6f;

            const float seg_radius = 0.5f * m_line_thickness;
            const float seg_square_radius = square(seg_radius);

            // Convert segment color to sRGB, premultipied alpha.
            Color4f seg_color_linear_rgb(
                srgb_to_linear_rgb(SegColorSRGB.rgb()) * SegColorSRGB.a,
                SegColorSRGB.a);

            // Compute bounding box of segment, accounting for its thickness.
            AABB2f bbox;
            bbox.invalidate();
            bbox.insert(seg.m_a);
            bbox.insert(seg.m_b);
            bbox.grow(Vector2f(seg_radius + Eps));

            // Compute bounding box of segment in pixels.
            int ixmin = truncate<int>(fast_floor(bbox.min.x));
            int ixmax = truncate<int>(fast_floor(bbox.max.x));    // used to be fast_ceil() but this is probably correct
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

                    // Retrieve background color.
                    Color4f background;
                    image.get_pixel(px, py, background);

                    // Compute pixel color.
                    Color4f pixel = seg_color_linear_rgb;
                    pixel *= static_cast<float>(hits) / square(SubpixelGridSize);

                    // Composite pixel color over background.
                    pixel += (1.0f - pixel.a) * background;

                    // Write final color to image.
                    image.set_pixel(px, py, pixel);
                }
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
    };
}


//
// IsolinesPostProcessingStageFactory class implementation.
//

void IsolinesPostProcessingStageFactory::release()
{
    delete this;
}

const char* IsolinesPostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary IsolinesPostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Isolines");
}

DictionaryArray IsolinesPostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "low_isovalue")
            .insert("label", "Low Isovalue")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "required")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "high_isovalue")
            .insert("label", "High Isovalue")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "levels")
            .insert("label", "Levels")
            .insert("type", "integer")
            .insert("min",
                Dictionary()
                    .insert("value", "1")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "64")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "8"));

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
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<PostProcessingStage> IsolinesPostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new IsolinesPostProcessingStage(name, params));
}

}   // namespace renderer
