
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

// appleseed.qtcommon headers.
#include "utility/interop.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Qt headers.
#include <QColor>

using namespace appleseed::qtcommon;
using namespace foundation;

void force_linking_qtcommon_utility_interop_tests() {}

TEST_SUITE(QtCommon_Utility_InterOp)
{
    TEST_CASE(UnitColorComponentToInt_GivenMinusOne_ReturnsZero)
    {
        const int result = unit_color_component_to_int(-1.0);
        EXPECT_EQ(0, result);
    }

    TEST_CASE(UnitColorComponentToInt_GivenZero_ReturnsZero)
    {
        const int result = unit_color_component_to_int(0.0);
        EXPECT_EQ(0, result);
    }

    TEST_CASE(UnitColorComponentToInt_GivenOne_Returns255)
    {
        const int result = unit_color_component_to_int(1.0);
        EXPECT_EQ(255, result);
    }

    TEST_CASE(UnitColorComponentToInt_GivenTwo_Returns255)
    {
        const int result = unit_color_component_to_int(2.0);
        EXPECT_EQ(255, result);
    }

    TEST_CASE(ColorToQColor_GivenColor4d_ReturnsQColor)
    {
        const QColor result = color_to_qcolor(Color4d(0.25, 0.5, 0.75, 1.0));
        EXPECT_EQ(QColor(64, 128, 192, 255), result);
    }

    TEST_CASE(QColorToColor_GivenQColor_ReturnsColor4d)
    {
        const Color4d result = qcolor_to_color<Color4d>(QColor(64, 128, 192, 255));
        EXPECT_EQ(Color4d(64.0/255, 128.0/255, 192.0/255, 255.0/255), result);
    }
}
