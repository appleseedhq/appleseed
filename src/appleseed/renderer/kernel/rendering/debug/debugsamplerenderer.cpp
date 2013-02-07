
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingresult.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cmath>

using namespace foundation;
using namespace std;

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
        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void render_sample(
            SamplingContext&    sampling_context,
            const Vector2d&     image_point,
            ShadingResult&      shading_result) OVERRIDE
        {
            const Vector2d v = Vector2d(0.5) - image_point;
            const double d = norm(v) * 2.0;
            const double M = 0.01;
            const float c =
                abs(0.5 - image_point.x) < M / 2 ||
                abs(0.5 - image_point.y) < M / 2 ||
                abs(1.0 - M / 2 - d) < M ? 0.0f : 1.0f;
            shading_result.set_to_linear_rgb(Color3f(c));
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
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

ISampleRenderer* DebugSampleRendererFactory::create()
{
    return new DebugSampleRenderer();
}

}   // namespace renderer
