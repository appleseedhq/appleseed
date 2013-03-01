
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "filterfactory.h"

using namespace foundation;
using namespace std;

namespace renderer
{

auto_ptr<FilterFactory::FilterType> FilterFactory::create(const string& name, const double radius)
{
    auto_ptr<FilterType> filter;

    if (name == "box")
        filter.reset(new BoxFilter2<double>(radius, radius));
    else if (name == "triangle")
        filter.reset(new TriangleFilter2<double>(radius, radius));
    else if (name == "gaussian")
        filter.reset(new GaussianFilter2<double>(radius, radius, 8.0));
    else if (name == "mitchell")
        filter.reset(new MitchellFilter2<double>(radius, radius, 1.0/3, 1.0/3));
    else if (name == "bspline")
        filter.reset(new MitchellFilter2<double>(radius, radius, 1.0, 0.0));
    else if (name == "catmull")
        filter.reset(new MitchellFilter2<double>(radius, radius, 0.0, 0.5));
    else if (name == "lanczos")
        filter.reset(new LanczosFilter2<double>(radius, radius, 3.0));

    return filter;
}

}   // namespace renderer
