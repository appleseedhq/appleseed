
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "normalaov.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Normal AOV accumulator.
    //

    class NormalAOVAccumulator
      : public UnfilteredAOVAccumulator
    {
      public:
        explicit NormalAOVAccumulator(Image& image)
          : UnfilteredAOVAccumulator(image)
        {
        }

        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            const AOVComponents&        aov_components,
            ShadingResult&              shading_result) override
        {
            const Vector2i& pi = pixel_context.get_pixel_coords();

            // Ignore samples outside the tile.
            if (!m_cropped_tile_bbox.contains(pi))
                return;

            float* out =
                reinterpret_cast<float*>(
                    m_tile->pixel(
                        pi.x - m_tile_origin_x,
                        pi.y - m_tile_origin_y));

            if (shading_point.hit_surface())
            {
                const Vector3d& n = shading_point.get_shading_normal();
                out[0] = static_cast<float>(n[0]) * 0.5f + 0.5f;
                out[1] = static_cast<float>(n[1]) * 0.5f + 0.5f;
                out[2] = static_cast<float>(n[2]) * 0.5f + 0.5f;
            }
            else
            {
                out[0] = 0.5f;
                out[1] = 0.5f;
                out[2] = 0.5f;
            }
        }
    };


    //
    // Normal AOV.
    //

    const char* NormalAOVModel = "normal_aov";

    class NormalAOV
      : public UnfilteredAOV
    {
      public:
        explicit NormalAOV(const ParamArray& params)
          : UnfilteredAOV("normal", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return NormalAOVModel;
        }

        void clear_image() override
        {
            m_image->clear(Color3f(0.5f));
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new NormalAOVAccumulator(get_image()));
        }
    };
}


//
// NormalAOVFactory class implementation.
//

void NormalAOVFactory::release()
{
    delete this;
}

const char* NormalAOVFactory::get_model() const
{
    return NormalAOVModel;
}

Dictionary NormalAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Normal");
}

DictionaryArray NormalAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> NormalAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new NormalAOV(params));
}

}   // namespace renderer
