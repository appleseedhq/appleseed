
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "debugsamplerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingresult.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/vector.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace renderer  { class PixelContext; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Debug sample renderer.
    //

    class DebugSampleRenderer
      : public ISampleRenderer
    {
      public:
        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
        }

        void render_sample(
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const Vector2d&             image_point,
            AOVAccumulatorContainer&    aov_accumulators,
            ShadingResult&              shading_result) override
        {
            const Vector2d v = Vector2d(0.5) - image_point;
            const double d = norm(v) * 2.0;
            const double M = 0.01;
            const float c =
                std::abs(0.5 - image_point.x) < M / 2 ||
                std::abs(0.5 - image_point.y) < M / 2 ||
                std::abs(1.0 - M / 2 - d) < M ? 0.0f : 1.0f;
            shading_result.m_main = Color4f(c, c, c, 1.0f);
        }

        StatisticsVector get_statistics() const override
        {
            return StatisticsVector();
        }
    };
}


//
// DebugSampleRendererFactory class implementation.
//

void DebugSampleRendererFactory::release()
{
    delete this;
}

ISampleRenderer* DebugSampleRendererFactory::create(const size_t thread_index)
{
    return new DebugSampleRenderer();
}

}   // namespace renderer
