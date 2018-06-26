
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
#include "renderstamppostprocessingstage.h"

// appleseed.renderer headers.
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/postprocessingstage/postprocessingstage.h"

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/drawing.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/text/textrenderer.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/system.h"
#include "foundation/resources/logo/appleseed-seeds-16.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    const char* Model = "render_stamp_post_processing_stage";

    const string DefaultFormatString = "appleseed {lib-version} | Time: {render-time}";

    class RenderStampPostProcessingStage
      : public PostProcessingStage
    {
      public:
        RenderStampPostProcessingStage(
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

            m_format_string = m_params.get_optional("format_string", DefaultFormatString, context);

            return true;
        }

        void execute(Frame& frame) const override
        {
            // Render stamp settings.
            const float FontSize = 14.0f;
            const Color4f FontColor(0.8f, 0.8f, 0.8f, 1.0f);
            const Color4f BackgroundColor(0.0f, 0.0f, 0.0f, 0.9f);
            const float MarginH = 6.0f;
            const float MarginV = 4.0f;

            // Retrieve additional render info from the frame.
            const ParamArray& render_info = frame.render_info();
            const double render_time = render_info.get_optional<double>("render_time", 0.0);

            // Compute the final string.
            string text = m_format_string;
            text = replace(text, "{lib-name}", Appleseed::get_lib_name());
            text = replace(text, "{lib-version}", Appleseed::get_lib_version());
            text = replace(text, "{lib-variant}", Appleseed::get_lib_variant());
            text = replace(text, "{lib-config}", Appleseed::get_lib_configuration());
            text = replace(text, "{lib-build-date}", Appleseed::get_lib_compilation_date());
            text = replace(text, "{lib-build-time}", Appleseed::get_lib_compilation_time());
            text = replace(text, "{render-time}", pretty_time(render_time, 1).c_str());
            text = replace(text, "{peak-memory}", pretty_size(System::get_peak_process_virtual_memory_size()).c_str());

            // Compute the height in pixels of the string.
            const CanvasProperties& props = frame.image().properties();
            const float image_height = static_cast<float>(props.m_canvas_height);
            const float text_height = TextRenderer::compute_string_height(FontSize, text.c_str());
            const float origin_y = image_height - text_height - MarginV;

            // Draw the background rectangle.
            Drawing::draw_filled_rect(
                frame.image(),
                Vector2i(
                    0,
                    truncate<int>(origin_y - MarginV)),
                Vector2i(
                    static_cast<int>(props.m_canvas_width - 1),
                    static_cast<int>(props.m_canvas_height - 1)),
                BackgroundColor);

            // Draw the string into the image.
            TextRenderer::draw_string(
                frame.image(),
                ColorSpaceLinearRGB,
                TextRenderer::Font::UbuntuL,
                FontSize,
                FontColor,
                MarginH + appleseed_seeds_16_width + MarginH,
                origin_y,
                text.c_str());

            // Blit the appleseed logo.
            Drawing::blit_bitmap(
                frame.image(),
                Vector2i(
                    static_cast<int>(MarginH),
                    static_cast<int>(props.m_canvas_height - appleseed_seeds_16_height - MarginV)),
                appleseed_seeds_16,
                appleseed_seeds_16_width,
                appleseed_seeds_16_height,
                PixelFormatUInt8,
                Color4f(1.0f, 1.0f, 1.0f, 0.8f));
        }

      private:
        string m_format_string;
    };
}


//
// RenderStampPostProcessingStageFactory class implementation.
//

void RenderStampPostProcessingStageFactory::release()
{
    delete this;
}

const char* RenderStampPostProcessingStageFactory::get_model() const
{
    return Model;
}

Dictionary RenderStampPostProcessingStageFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Render Stamp");
}

DictionaryArray RenderStampPostProcessingStageFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "format_string")
            .insert("label", "Format String")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", DefaultFormatString));

    return metadata;
}

auto_release_ptr<PostProcessingStage> RenderStampPostProcessingStageFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PostProcessingStage>(
        new RenderStampPostProcessingStage(name, params));
}

}   // namespace renderer
