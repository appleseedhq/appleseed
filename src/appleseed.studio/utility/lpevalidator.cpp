
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Gray Olson, The appleseedhq Organization
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

#include "lpevalidator.h"

using namespace OSL;

namespace appleseed {
namespace studio {

// LpeValidator implementation

LpeValidator::LpeValidator()
{}

QValidator::State LpeValidator::validate(QString& input, int& pos) const
{
    AccumAutomata automata;

    if (automata.addRule(input.toUtf8().data(), 0, false) != NULL)
    {
        return QValidator::State::Acceptable;
    }
    else
    {
        return QValidator::State::Intermediate;
    }
}

// LPSortAov implementation

void LPSortAov::write(
    void*    flush_data,
    Color3&  color,
    float    alpha,
    bool     has_color,
    bool     has_alpha)
{
    size_t lp_idx = (size_t)flush_data;
    if (has_color && color.x > 0)
        // only add if there is a positive color in x
        m_received.push_back(lp_idx);
}

std::vector<size_t> LPSortAov::get_received() const
{
    return m_received;
}

} // namespace studio
} // namespace appleseed
