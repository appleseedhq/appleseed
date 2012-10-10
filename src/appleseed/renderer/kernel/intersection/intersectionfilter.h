
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONFILTER_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONFILTER_H

// appleseed.renderer headers.
#include "renderer/kernel/intersection/trianglekey.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class ObjectInstance; }
namespace renderer  { class Source; }
namespace renderer  { class TextureCache; }

namespace renderer
{

class IntersectionFilter
  : public foundation::NonCopyable
{
  public:
    IntersectionFilter(
        const ObjectInstance&   object_instance,
        TextureCache&           texture_cache);

    ~IntersectionFilter();

    bool keep() const;

    bool accept(
        const TriangleKey&      triangle_key,
        const double            u,
        const double            v) const;

    size_t get_memory_size() const;

  private:
    struct Bitmap
    {
        size_t                          m_width;
        size_t                          m_height;
        float                           m_max_x;
        float                           m_max_y;
        double                          m_transparency;
        std::vector<foundation::uint8>  m_bits;
    };

    std::vector<Bitmap*>                m_alpha_masks;
    std::vector<foundation::Vector2f>   m_uv;

    static Bitmap* create_alpha_mask(
        const Source*           alpha_map,
        TextureCache&           texture_cache);

    void copy_uv_coordinates(
        const ObjectInstance&   object_instance);
};


//
// IntersectionFilter class implementation.
//

inline bool IntersectionFilter::accept(
    const TriangleKey&          triangle_key,
    const double                u,
    const double                v) const
{
    assert(triangle_key.get_region_index() == 0);

    const Bitmap* alpha_mask = m_alpha_masks[triangle_key.get_triangle_pa()];

    if (alpha_mask == 0)
        return true;

    const size_t triangle_index = triangle_key.get_triangle_index();

    const float fu = static_cast<float>(u);
    const float fv = static_cast<float>(v);

    const foundation::Vector2f uv =
          m_uv[triangle_index * 3 + 0] * (1.0f - fu - fv)
        + m_uv[triangle_index * 3 + 1] * fu
        + m_uv[triangle_index * 3 + 2] * fv;

    const float fx = foundation::clamp(uv[0] * alpha_mask->m_width, 0.0f, alpha_mask->m_max_x);
    const float fy = foundation::clamp(uv[1] * alpha_mask->m_height, 0.0f, alpha_mask->m_max_y);
    const size_t ix = foundation::truncate<size_t>(fx);
    const size_t iy = foundation::truncate<size_t>(fy);

    return alpha_mask->m_bits[iy * alpha_mask->m_width + ix] > 0;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONFILTER_H
