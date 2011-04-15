
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// appleseed.renderer headers.
#include "renderer/utility/testutils.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(CompareImages)
{
    struct ExceptionNonMatchingImageSizes
      : public Exception
    {
    };

    struct IPixelOp
    {
        virtual Color3f operator()(const Color3f& lhs, const Color3f& rhs) const = 0;
    };

    struct ColorSubstractPixelOp : public IPixelOp
    {
        virtual Color3f operator()(const Color3f& lhs, const Color3f& rhs) const
        {
            Color3f result;

            for (size_t i = 0; i < 3; ++i)
                result[i] = abs(lhs[i] - rhs[i]);

            return result;
        }
    };

    struct LuminanceSubstractPixelOp : public IPixelOp
    {
        virtual Color3f operator()(const Color3f& lhs, const Color3f& rhs) const
        {
            const float result = luminance(lhs) - luminance(rhs);
            return
                result > 0.0f
                    ? Color3f(result, 0.0f, 0.0f)       // red = positive
                    : Color3f(0.0f, 0.0f, -result);     // blue = negative
        }
    };

    struct SignDifferencePixelOp : public IPixelOp
    {
        const float m_threshold;

        SignDifferencePixelOp(const float threshold = 0.0f)
          : m_threshold(threshold)
        {
        }

        virtual Color3f operator()(const Color3f& lhs, const Color3f& rhs) const
        {
            const float diff = luminance(lhs) - luminance(rhs);

            return
                diff > m_threshold ? Color3f(1.0f, 0.0f, 0.0f) :    // red = positive
                diff < -m_threshold ? Color3f(0.0f, 0.0f, 1.0f) :   // blue = negative
                Color3f(0.0f);                                      // black = insignificant
        }
    };

    struct ColorDividePixelOp : public IPixelOp
    {
        virtual Color3f operator()(const Color3f& lhs, const Color3f& rhs) const
        {
            Color3f result;

            for (size_t i = 0; i < 3; ++i)
                result[i] = lhs[i] == rhs[i] ? 1.0f : lhs[i] / rhs[i];

            return result;
        }
    };

    struct LuminanceDividePixelOp : public IPixelOp
    {
        virtual Color3f operator()(const Color3f& lhs, const Color3f& rhs) const
        {
            const float lhs_lum = luminance(lhs);
            const float rhs_lum = luminance(rhs);
            const float result = lhs_lum == rhs_lum ? 1.0f : lhs_lum / rhs_lum;
            return Color3f(result);
        }
    };

    auto_ptr<Image> compare(const Image& lhs, const Image& rhs, const IPixelOp& op)
    {
        const CanvasProperties& lhs_props = lhs.properties();
        const CanvasProperties& rhs_props = rhs.properties();

        if (lhs_props.m_canvas_width != rhs_props.m_canvas_width ||
            lhs_props.m_canvas_height != rhs_props.m_canvas_height)
            throw ExceptionNonMatchingImageSizes();

        auto_ptr<Image> output(new Image(lhs));

        for (size_t y = 0; y < lhs_props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < lhs_props.m_canvas_width; ++x)
            {
                Color3f lhs_color;
                lhs.get_pixel(x, y, lhs_color);
                
                Color3f rhs_color;
                rhs.get_pixel(x, y, rhs_color);

                const Color3f result = op(lhs_color, rhs_color);

                output->set_pixel(x, y, saturate(result));
            }
        }

        return output;
    }

#if 0
    TEST_CASE(Compare)
    {
        const size_t Width = 512;
        const size_t Height = 512;

        auto_ptr<Image> left_image = load_raw_image("first.raw", Width, Height);
        auto_ptr<Image> right_image = load_raw_image("second.raw", Width, Height);

        ASSERT_TRUE(left_image.get());
        ASSERT_TRUE(right_image.get());

        // ColorSubstractPixelOp op;
        // LuminanceSubstractPixelOp op;
        SignDifferencePixelOp op;
        // ColorDividePixelOp op;
        // LuminanceDividePixelOp op;

        auto_ptr<Image> result = compare(*left_image.get(), *right_image.get(), op);

        GenericImageFileWriter writer;
        writer.write("result.png", *result.get());
    }
#endif
}
