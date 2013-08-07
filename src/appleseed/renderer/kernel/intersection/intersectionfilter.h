
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
#include <cstring>
#include <vector>

// Forward declarations.
namespace renderer  { class MaterialArray; }
namespace renderer  { class Object; }
namespace renderer  { class Source; }
namespace renderer  { class TextureCache; }

namespace renderer
{

class IntersectionFilter
  : public foundation::NonCopyable
{
  public:
    IntersectionFilter(
        Object&                 object,
        const MaterialArray&    materials,
        TextureCache&           texture_cache);

    ~IntersectionFilter();

    bool has_alpha_masks() const;

    bool accept(
        const TriangleKey&      triangle_key,
        const double            u,
        const double            v) const;

  private:
    struct Bitmap
    {
        size_t                  m_width;
        size_t                  m_height;
        size_t                  m_block_width;
        size_t                  m_size;
        float                   m_max_x;
        float                   m_max_y;
        double                  m_transparency;
        foundation::uint8*      m_bits;

        Bitmap(
            const size_t        width,
            const size_t        height)
          : m_width(width)
          , m_height(height)
          , m_block_width((width + 7) / 8)
          , m_size(m_block_width * m_height)
          , m_max_x(static_cast<float>(width) - 1.0f)
          , m_max_y(static_cast<float>(height) - 1.0f)
          , m_bits(new foundation::uint8[m_size])
        {
            std::memset(m_bits, 0, m_size);
        }

        ~Bitmap()
        {
            delete [] m_bits;
        }

        size_t get_memory_size() const
        {
            return sizeof(*this) + m_size;
        }

        void set(
            const size_t            x,
            const size_t            y,
            const foundation::uint8 b)
        {
            m_bits[y * m_block_width + x / 8] |= b << (x & 7);
        }

        foundation::uint8 get(
            const size_t            x,
            const size_t            y) const
        {
            return m_bits[y * m_block_width + x / 8] >> (x & 7);
        }
    };

    std::vector<Bitmap*>                m_alpha_masks;
    std::vector<foundation::Vector2f>   m_uv;

    static Bitmap* create_alpha_mask(
        const Source*           alpha_map,
        TextureCache&           texture_cache);

    size_t get_masks_memory_size() const;
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

    return alpha_mask->get(ix, iy) != 0;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONFILTER_H
