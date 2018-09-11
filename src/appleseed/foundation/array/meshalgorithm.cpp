
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/array/applyvisitor.h"
#include "foundation/array/array.h"
#include "foundation/array/arrayview.h"
#include "foundation/array/exception.h"
#include "foundation/array/meshalgorithm.h"

// Standard headers.
#include <algorithm>

using namespace std;

namespace foundation
{
namespace
{
    struct ComputeBBoxVisitor
    {
        AABB3f m_bbox;

        ComputeBBoxVisitor()
        {
            m_bbox.invalidate();
        }

        explicit ComputeBBoxVisitor(const AABB3f& bbox)
        : m_bbox(bbox)
        {
        }

        void operator()(const ArrayView<Vector3f>& view)
        {
            for (const Vector3f& p : view)
                m_bbox.insert(p);
        }

        template <typename T>
        void operator()(const ArrayView<T>& view)
        {
            throw BadArrayTypeException();
        }
    };

    class FaceSidesInfoVisitor
    {
    public:
        explicit FaceSidesInfoVisitor(FaceSidesInfo& stats)
        : m_stats(stats)
        {
        }

        void operator()(const ArrayView<uint8>& view)
        {
            collect_stats(view);
        }

        void operator()(const ArrayView<uint16>& view)
        {
            collect_stats(view);
        }

        void operator()(const ArrayView<uint32>& view)
        {
            collect_stats(view);
        }

        template <typename T>
        void operator()(const ArrayView<T>& view)
        {
            throw BadArrayTypeException();
        }

    private:
        FaceSidesInfo& m_stats;

        template <typename T>
        void collect_stats(const ArrayView<T>& view)
        {
            for (const T n : view)
            {
                m_stats.m_face_count++;

                    if (n <  3) m_stats.m_invalid_count++;
                else if (n == 3) m_stats.m_triangle_count++;
                else if (n == 4) m_stats.m_quad_count++;
                else             m_stats.m_ngon_count++;

                m_stats.m_max_face_sides = max(
                    m_stats.m_max_face_sides,
                    static_cast<size_t>(n));
            }
        }
    };
}

AABB3f compute_bounding_box(const Array& vertices)
{
    ComputeBBoxVisitor v;
    apply_visitor(vertices, v);
    return v.m_bbox;
}

AABB3f compute_bounding_box(const Array& vertices, const AABB3f& initial_bbox)
{
    ComputeBBoxVisitor v(initial_bbox);
    apply_visitor(vertices, v);
    return v.m_bbox;
}

FaceSidesInfo get_face_sides_info(const Array& verts_per_face)
{
    FaceSidesInfo info;
    apply_visitor(verts_per_face, FaceSidesInfoVisitor(info));
    return info;
}

}       // namespace foundation
