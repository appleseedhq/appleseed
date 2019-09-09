
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

// appleseed.renderer headers.
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/scalarsource.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace renderer;

TEST_SUITE(Renderer_Modeling_Input_InputArray)
{
    TEST_CASE(Find_GivenNameOfExistingInput_ReturnsInputIterator)
    {
        InputArray inputs;
        inputs.declare("x", InputFormatFloat);

        const InputArray::const_iterator i = inputs.find("x");

        EXPECT_EQ("x", std::string(i.name()));
    }

    TEST_CASE(Find_GivenNameOfNonExistingInput_ReturnsEndIterator)
    {
        InputArray inputs;
        inputs.declare("x", InputFormatFloat);

        const InputArray::const_iterator i = inputs.find("y");

        EXPECT_TRUE(i == inputs.end());
    }

    TEST_CASE(Source_GivenNameOfNonExistingInput_ReturnsZero)
    {
        InputArray inputs;
        inputs.declare("x", InputFormatFloat);

        const Source* source = inputs.source("y");

        EXPECT_EQ(0, source);
    }

    TEST_CASE(Source_GivenNameOfUnboundExistingInput_ReturnsZero)
    {
        InputArray inputs;
        inputs.declare("x", InputFormatFloat);

        const Source* source = inputs.source("x");

        EXPECT_EQ(0, source);
    }

    TEST_CASE(Source_GivenNameOfBoundExistingInput_ReturnsBoundSource)
    {
        InputArray inputs;
        inputs.declare("x", InputFormatFloat);

        Source* expected_source = new ScalarSource(1.0);
        inputs.find("x").bind(expected_source);

        const Source* source = inputs.source("x");

        EXPECT_EQ(expected_source, source);
    }
}
