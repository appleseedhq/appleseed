
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/bitmask.h"

// Standard headers.
#include <cassert>
#include <cstddef>
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
    class AlphaMask
      : public foundation::NonCopyable
    {
      public:
        AlphaMask(
            const size_t        width,
            const size_t        height)
          : m_max_x(static_cast<float>(width) - 1.0f)
          , m_max_y(static_cast<float>(height) - 1.0f)
          , m_bitmask(width, height)
        {
        }

        void set_opaque(
            const size_t        x,
            const size_t        y,
            const bool          opaque)
        {
            m_bitmask.set(x, y, opaque);
        }

        bool is_opaque(const foundation::Vector2f& uv) const
        {
            const float fx = foundation::clamp(uv[0] * m_bitmask.get_width(), 0.0f, m_max_x);
            const float fy = foundation::clamp(uv[1] * m_bitmask.get_height(), 0.0f, m_max_y);

            const size_t ix = foundation::truncate<size_t>(fx);
            const size_t iy = foundation::truncate<size_t>(fy);

            return m_bitmask.is_set(ix, iy);
        }

        size_t get_memory_size() const
        {
            return m_bitmask.get_memory_size();
        }

      private:
        const float             m_max_x;
        const float             m_max_y;
        foundation::BitMask2    m_bitmask;
    };

    std::vector<AlphaMask*>             m_alpha_masks;
    std::vector<foundation::Vector2f>   m_uv;

    static AlphaMask* create_alpha_mask(
        const Source*           alpha_map,
        TextureCache&           texture_cache,
        double&                 transparency);

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

    const AlphaMask* alpha_mask = m_alpha_masks[triangle_key.get_triangle_pa()];

    if (alpha_mask == 0)
        return true;

    // Don't use the alpha mask if the UV coordinates are indefinite.
    // This can happen in rare circumstances, when hitting degenerate
    // or nearly degenerate geometry. Since we cannot guarantee to
    // catch all instances of degenerate geometry before rendering, and
    // because using the alpha mask in this case would lead to a crash,
    // we decide in this case to simply revert to the normal code path.
    if (u != u || v != v)
        return true;

    const size_t triangle_index = triangle_key.get_triangle_index();

    const float fu = static_cast<float>(u);
    const float fv = static_cast<float>(v);

    const foundation::Vector2f uv =
          m_uv[triangle_index * 3 + 0] * (1.0f - fu - fv)
        + m_uv[triangle_index * 3 + 1] * fu
        + m_uv[triangle_index * 3 + 2] * fv;

    return alpha_mask->is_opaque(uv);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTIONFILTER_H
