
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
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_Frame_Frame)
{
    struct Fixture
    {
        Image                   m_ref_image;
        auto_release_ptr<Frame> m_frame;

        Fixture()
          : m_ref_image(32, 32, 32, 32, 4, PixelFormatFloat)
        {
            ParamArray params;
            params.insert("resolution", "32 32");
            params.insert("tile_size", "32 32");
            params.insert("pixel_format", "float");
            params.insert("color_space", "linear_rgb");
            m_frame = FrameFactory::create("frame", params);
        }
    };

    TEST_CASE_F(ComputeRMSDeviation_GivenBlankLinearRGBFrameAndBlankRefImage_ReturnsZero, Fixture)
    {
        const double rmsd = m_frame->compute_rms_deviation(m_ref_image);

        EXPECT_EQ(0.0, rmsd);
    }

    TEST_CASE_F(ComputeRMSDeviation_GivenWhiteLinearRGBFrameAndWhiteRefImage_ReturnsZero, Fixture)
    {
        m_frame->tile(0, 0).clear(Color4f(1.0f));
        m_ref_image.tile(0, 0).clear(Color4f(1.0f));

        const double rmsd = m_frame->compute_rms_deviation(m_ref_image);

        EXPECT_EQ(0.0, rmsd);
    }

    TEST_CASE_F(ComputeRMSDeviation_GivenWhiteLinearRGBFrameAndBlackRefImage_ReturnsOne, Fixture)
    {
        m_frame->tile(0, 0).clear(Color4f(1.0f));
        m_ref_image.tile(0, 0).clear(Color4f(0.0f));

        const double rmsd = m_frame->compute_rms_deviation(m_ref_image);

        EXPECT_EQ(1.0, rmsd);
    }
}
