
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
#include "positionaov.h"

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
    // Position AOV accumulator.
    //

    class PositionAOVAccumulator
      : public UnfilteredAOVAccumulator
    {
      public:
        explicit PositionAOVAccumulator(Image& image)
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
                const Vector3d& p = shading_point.get_point();
                out[0] = static_cast<float>(p[0]);
                out[1] = static_cast<float>(p[1]);
                out[2] = static_cast<float>(p[2]);
            }
            else
            {
                out[0] = 0.0f;
                out[1] = 0.0f;
                out[2] = 0.0f;
            }
        }
    };


    //
    // Position AOV.
    //

    const char* PositionAOVModel = "position_aov";

    class PositionAOV
      : public UnfilteredAOV
    {
      public:
        explicit PositionAOV(const ParamArray& params)
          : UnfilteredAOV("position", params)
        {
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return PositionAOVModel;
        }

        void clear_image() override
        {
            m_image->clear(Color3f(0.0f));
        }

      private:
        auto_release_ptr<AOVAccumulator> create_accumulator() const override
        {
            return auto_release_ptr<AOVAccumulator>(
                new PositionAOVAccumulator(get_image()));
        }
    };
}


//
// PositionAOVFactory class implementation.
//

void PositionAOVFactory::release()
{
    delete this;
}

const char* PositionAOVFactory::get_model() const
{
    return PositionAOVModel;
}

Dictionary PositionAOVFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", get_model())
            .insert("label", "Position");
}

DictionaryArray PositionAOVFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<AOV> PositionAOVFactory::create(const ParamArray& params) const
{
    return auto_release_ptr<AOV>(new PositionAOV(params));
}

}   // namespace renderer
