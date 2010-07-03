
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_GLOBAL_UTILITY_H
#define APPLESEED_RENDERER_GLOBAL_UTILITY_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

namespace renderer
{

//
// Compute the bounding box of a set of objects.  The objects must implement
// a get_parent_bbox() method that return their bounding box in parent space.
//

template <typename BBox, typename Iterator>
BBox compute_parent_bbox(const Iterator begin, const Iterator end)
{
    BBox bbox;
    bbox.invalidate();

    for (Iterator i = begin; i != end; ++i)
        bbox.insert(i->get_parent_bbox());

    return bbox;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_GLOBAL_UTILITY_H
