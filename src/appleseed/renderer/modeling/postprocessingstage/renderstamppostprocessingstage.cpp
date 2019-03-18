
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
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/text/textrenderer.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/system.h"
#include "foundation/resources/logo/appleseed-seeds-256.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/string.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/imagebuf.h"
#include "OpenImageIO/imagebufalgo.h"
#include "OpenImageIO/typedesc.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <memory>
#include <string>

using namespace foundation;
using namespace OIIO;
using namespace std;

namespace renderer
{

namespace
{
    const char* Model = "render_stamp_post_processing_stage";

    const string DefaultFormatString = "appleseed {lib-version} | Time: {render-time}";
    const float DefaultScaleFactor = 1.0f;
    const float MinScaleFactor = 0.1f;
    const float MaxScaleFactor = 20.0f;

    class RenderStampPostProcessingStage
      : public PostProcessingStage
    {
      public:
        RenderStampPostProcessingStage(
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

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            const OnFrameBeginMessageContext context("post-processing stage", this);

            m_format_string = m_params.get_optional("format_string", DefaultFormatString, context);
            m_scale_factor = m_params.get_optional("scale_factor", DefaultScaleFactor, context);
            m_icon.reset(ImageSpec(appleseed_seeds_256_width, appleseed_seeds_256_height, 4, TypeDesc::TypeFloat));
            m_icon.set_pixels(ROI(0, appleseed_seeds_256_width, 0, appleseed_seeds_256_height), TypeDesc::TypeFloat, appleseed_seeds_256);

            return true;
        }

        void execute(Frame& frame) const override
        {
            // Render stamp settings.
            const auto Font = TextRenderer::Font::UbuntuL;
            const float font_height = 14.0f * m_scale_factor;
            const Color4f FontColor(srgb_to_linear_rgb(Color3f(0.9f, 0.9f, 0.9f)), 1.0f);   // linear RGB
            const Color4f BackgroundColor(0.0f, 0.0f, 0.0f, 0.95f);                         // linear RGB
            const Color4f LogoTint(1.0f, 1.0f, 1.0f, 0.8f);                                 // linear RGB
            const float margin_h = 6.0f * m_scale_factor;
            const float margin_v = 4.0f * m_scale_factor;

            // Retrieve additional render info from the frame.
            const ParamArray& render_info = frame.render_info();
            const double render_time = render_info.get_optional<double>("render_time", 0.0);

            // Compute the final string.
            string text = m_format_string;
            text = replace(text, "{lib-name}", Appleseed::get_lib_name());
            text = replace(text, "{lib-version}", Appleseed::get_lib_version());
            text = replace(text, "{lib-cpu-features}", Appleseed::get_lib_cpu_features());
            text = replace(text, "{lib-config}", Appleseed::get_lib_configuration());
            text = replace(text, "{lib-build-date}", Appleseed::get_lib_compilation_date());
            text = replace(text, "{lib-build-time}", Appleseed::get_lib_compilation_time());
            text = replace(text, "{render-time}", pretty_time(render_time, 1).c_str());
            text = replace(text, "{peak-memory}", pretty_size(System::get_peak_process_virtual_memory_size()).c_str());

            // Compute the height in pixels of the string.
            const CanvasProperties& props = frame.image().properties();
            const float image_height = static_cast<float>(props.m_canvas_height);
            const float text_height = TextRenderer::compute_string_height(font_height, text.c_str());
            const float origin_y = image_height - text_height - margin_v;

            // Draw the background rectangle.
            Drawing::draw_filled_rect(
                frame.image(),
                Vector2i(
                    0,
                    truncate<int>(origin_y - margin_v)),
                Vector2i(
                    static_cast<int>(props.m_canvas_width - 1),
                    static_cast<int>(props.m_canvas_height - 1)),
                BackgroundColor);

            // Calculate final icon size and scale the icon
            const ImageSpec icon_spec = m_icon.spec();
            const float stamp_height = text_height + 2 * margin_v;
            const float icon_height = 0.68f * stamp_height;
            const float aspect = static_cast<float>(icon_spec.width) / static_cast<float>(icon_spec.height);
            const ROI roi(
                0,
                round<int>(icon_height * aspect),
                0,
                round<int>(icon_height),
                0,
                1,
                0,
                m_icon.nchannels());
                
            ImageBuf unpremult_icon, scaled_unpremult_icon, scaled_premult_icon;
            ImageBufAlgo::unpremult(unpremult_icon, m_icon, ROI::All());
            const float filter_width = fit(m_scale_factor, MinScaleFactor, MaxScaleFactor, 2.75f, 4.5f);
            ImageBufAlgo::resize(scaled_unpremult_icon, unpremult_icon, "mitchell", filter_width, roi);
            ImageBufAlgo::premult(scaled_premult_icon, scaled_unpremult_icon, ROI::All());
            unique_ptr<float[]> pixels(new float[roi.width() * roi.height() * roi.nchannels()]);
            scaled_premult_icon.get_pixels(ROI::All(), TypeDesc::TypeFloat, pixels.get());

            // Blit the appleseed logo.
            Drawing::blit_bitmap(
                frame.image(),
                Vector2i(
                    static_cast<int>(margin_h),
                    static_cast<int>(props.m_canvas_height - icon_height - margin_v)),
                pixels.get(),
                roi.width(),
                roi.height(),
                PixelFormatFloat,
                LogoTint);

            // Draw the string into the image.
            TextRenderer::draw_string(
                frame.image(),
                Font,
                font_height,
                FontColor,
                margin_h + roi.width() + margin_h,
                origin_y,
                text.c_str());
        }

      private:
        string   m_format_string;
        float    m_scale_factor;
        ImageBuf m_icon;
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

    metadata.push_back(
        Dictionary()
            .insert("name", "scale_factor")
            .insert("label", "Scale Factor")
            .insert("type", "numeric")
            .insert("min",
                    Dictionary()
                        .insert("value", "0.1")
                        .insert("type", "hard"))
            .insert("max",
                    Dictionary()
                        .insert("value", "20.0")
                        .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0"));

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
