
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

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstdlib>

using namespace foundation;

BENCHMARK_SUITE(Foundation_String_String)
{
    template <typename T>
    struct Fixture
    {
        T m_dummy;

        Fixture() : m_dummy(0) {}
    };

    BENCHMARK_CASE_F(Strtol, Fixture<long>)
    {
        m_dummy += strtol("123456", 0, 10);
    }

    BENCHMARK_CASE_F(FastStrtolBase10, Fixture<long>)
    {
        m_dummy += fast_strtol_base10("123456", 0);
    }

    BENCHMARK_CASE_F(Strtod_FixedNotation, Fixture<double>)
    {
        m_dummy += strtod("12345.67891", 0);
    }

    BENCHMARK_CASE_F(Strtod_ScientificNotation, Fixture<double>)
    {
        m_dummy += strtod("123.4567e-4", 0);
    }

    BENCHMARK_CASE_F(FastStrtod_FixedNotation, Fixture<double>)
    {
        m_dummy += fast_strtod("12345.67891", 0);
    }

    BENCHMARK_CASE_F(FastStrtod_ScientificNotation, Fixture<double>)
    {
        m_dummy += fast_strtod("123.4567e-4", 0);
    }
}
