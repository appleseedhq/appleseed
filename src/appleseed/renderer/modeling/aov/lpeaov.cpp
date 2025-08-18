
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Joey Chen, The appleseedhq Organization
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
#include "lpeaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/color/colorspace.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/oslversion.h"
#include "foundation/platform/_endoslheaders.h"

// Standard headers.
#include <vector>

using namespace foundation;
using namespace std;
using OIIO::ustring;

namespace renderer
{

namespace
{
    //
    // Structure to pass information to OSLAOVWrapper.
    //
    struct PixelInfo
    {
        size_t      px;
        size_t      py;
        size_t      sample_count;
    };

    //
    // A wrapper class for the OSL AOV.
    //

    class OSLAOVWrapper
      : public OSL::Aov
    {
      public:
        void set_image_ptr(Image* image_ptr)
        {
            m_aov_image_ptr = image_ptr;
        }

        void write(void *flush_data, OSL::Color3 &color, float alpha, bool has_color, bool has_alpha) override
        {
            // Flush_data will point to a PixelInfo struct.
            auto pixel_info_ptr = (PixelInfo*)(flush_data);
            m_aov_image_ptr->set_pixel(pixel_info_ptr->px, pixel_info_ptr->py, Color3f(color.x, color.y, color.z));
        }

      private:
        Image*                          m_aov_image_ptr;
    };

    const char* LPEAOVModel = "lpeaov";
}


//
// LPEAOV class implementation.
//

struct LPEAOV::Impl
{
    string rule;
    OSLAOVWrapper osl_aov;
};

LPEAOV::LPEAOV(const ParamArray& params)
  : UnfilteredAOV("oslmatte", params)
  , impl(new Impl())
{
    set_name(params.get_required<string>("name").c_str());
    impl->rule = params.get_required<string>("rule");
}

LPEAOV::~LPEAOV()
{
    delete impl;
}

const char* LPEAOV::get_model() const
{
    return LPEAOVModel;
}

const char* LPEAOV::get_rule_string() const
{
    return impl->rule.c_str();
}

OSL::Aov* LPEAOV::get_wrapped_aov() const
{
    return &impl->osl_aov;
}

auto_release_ptr<AOVAccumulator> LPEAOV::create_accumulator() const
{
    return auto_release_ptr<AOVAccumulator>();
}

void LPEAOV::create_image(const size_t canvas_width, const size_t canvas_height, const size_t tile_width, const size_t tile_height, ImageStack &aov_images)
{
    UnfilteredAOV::create_image(canvas_width, canvas_height, tile_width, tile_height, aov_images);
    impl->osl_aov.set_image_ptr(m_image);
}


//
// LPEAOVFactory class implementation.
//

void LPEAOVFactory::release()
{
    delete this;
}

const char* LPEAOVFactory::get_model() const
{
    return LPEAOVModel;
}

Dictionary LPEAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "LPE");
}

DictionaryArray LPEAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> LPEAOVFactory::create(
    const ParamArray&   params) const
{
    return auto_release_ptr<AOV>(new LPEAOV(params));
}

}   // namespace renderer
