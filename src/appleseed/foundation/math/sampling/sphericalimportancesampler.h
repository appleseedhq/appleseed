
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_SAMPLING_SPHERICALIMPORTANCESAMPLER_H
#define APPLESEED_FOUNDATION_MATH_SAMPLING_SPHERICALIMPORTANCESAMPLER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/area.h"
#include "foundation/math/cdf.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/sphericaltriangle.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/vpythonfile.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <map>
#include <vector>

namespace foundation
{

template <typename T>
class SphericalImportanceSampler
  : public foundation::NonCopyable
{
  public:
    explicit SphericalImportanceSampler(const size_t subdivisions);

    Vector<T, 3> sample(const Vector<T, 3>& s) const;

    bool dump_as_obj(const char* filepath) const;
    void dump_to_vpython_file(VPythonFile& file) const;

  private:
    struct Tri
    {
        size_t m_v0, m_v1, m_v2;

        Tri() {}

        Tri(const size_t v0, const size_t v1, const size_t v2)
          : m_v0(v0)
          , m_v1(v1)
          , m_v2(v2)
        {
        }
    };

    typedef std::map<uint64, size_t> PointCache;

    std::vector<Vector<T, 3> >  m_verts;
    std::vector<Tri>            m_tris;
    CDF<size_t, T>              m_cdf;

    void create_regular_icosahedron();
    void subdivide(const size_t subdivisions);
    size_t get_or_create_middle_point(PointCache& point_cache, size_t v0, size_t v1);
    void build_cdf();
};


//
// SphericalImportanceSampler class implementation.
//
// References:
//
//   http://en.wikipedia.org/wiki/Regular_icosahedron#Cartesian_coordinates
//   http://blog.andreaskahler.com/2009/06/creating-icosphere-mesh-in-code.html
//   http://www.graphics.cornell.edu/pubs/1995/Arv95c.pdf
//

template <typename T>
SphericalImportanceSampler<T>::SphericalImportanceSampler(const size_t subdivisions)
{
    create_regular_icosahedron();
    subdivide(subdivisions);
    build_cdf();
}

template <typename T>
Vector<T, 3> SphericalImportanceSampler<T>::sample(const Vector<T, 3>& s) const
{
    const size_t tri_index = m_cdf.sample(s[0]).first;
    const Tri& tri = m_tris[tri_index];

    return
        sample_spherical_triangle_uniform(
            m_verts[tri.m_v0],
            m_verts[tri.m_v1],
            m_verts[tri.m_v2],
            Vector<T, 2>(s[1], s[2]));
}

template <typename T>
bool SphericalImportanceSampler<T>::dump_as_obj(const char* filepath) const
{
    std::FILE* file = std::fopen(filepath, "wt");

    if (file == 0)
        return false;

    for (size_t i = 0; i < m_verts.size(); ++i)
    {
        const Vector<T, 3>& vert = m_verts[i];
        std::fprintf(file, "v %.15f %.15f %.15f\n", vert.x, vert.y, vert.z);
    }

    for (size_t i = 0; i < m_tris.size(); ++i)
    {
        const Tri& tri = m_tris[i];
        std::fprintf(
            file,
            "f " FMT_SIZE_T " " FMT_SIZE_T " " FMT_SIZE_T "\n",
            tri.m_v0 + 1, tri.m_v1 + 1, tri.m_v2 + 1);
    }

    std::fclose(file);

    return true;
}

template <typename T>
void SphericalImportanceSampler<T>::dump_to_vpython_file(VPythonFile& file) const
{
    for (size_t i = 0; i < m_tris.size(); ++i)
    {
        const Tri& tri = m_tris[i];
        const Vector3d points[] =
        {
            Vector3d(m_verts[tri.m_v0]),
            Vector3d(m_verts[tri.m_v1]),
            Vector3d(m_verts[tri.m_v2])
        };

        file.draw_triangle(points[0], points[1], points[2]);
        file.draw_polyline(3, points);
    }
}

template <typename T>
void SphericalImportanceSampler<T>::create_regular_icosahedron()
{
    m_verts.reserve(12);
    m_tris.reserve(20);

    m_verts.push_back(normalize(Vector<T, 3>(T(-1.0),  GoldenRatio<T>(), T(0.0))));
    m_verts.push_back(normalize(Vector<T, 3>(T( 1.0),  GoldenRatio<T>(), T(0.0))));
    m_verts.push_back(normalize(Vector<T, 3>(T(-1.0), -GoldenRatio<T>(), T(0.0))));
    m_verts.push_back(normalize(Vector<T, 3>(T( 1.0), -GoldenRatio<T>(), T(0.0))));

    m_verts.push_back(normalize(Vector<T, 3>(T(0.0), T(-1.0),  GoldenRatio<T>())));
    m_verts.push_back(normalize(Vector<T, 3>(T(0.0), T( 1.0),  GoldenRatio<T>())));
    m_verts.push_back(normalize(Vector<T, 3>(T(0.0), T(-1.0), -GoldenRatio<T>())));
    m_verts.push_back(normalize(Vector<T, 3>(T(0.0), T( 1.0), -GoldenRatio<T>())));

    m_verts.push_back(normalize(Vector<T, 3>( GoldenRatio<T>(), T(0.0), T(-1.0))));
    m_verts.push_back(normalize(Vector<T, 3>( GoldenRatio<T>(), T(0.0), T( 1.0))));
    m_verts.push_back(normalize(Vector<T, 3>(-GoldenRatio<T>(), T(0.0), T(-1.0))));
    m_verts.push_back(normalize(Vector<T, 3>(-GoldenRatio<T>(), T(0.0), T( 1.0))));

    // 5 faces around point 0.
    m_tris.push_back(Tri(0, 11, 5));
    m_tris.push_back(Tri(0, 5, 1));
    m_tris.push_back(Tri(0, 1, 7));
    m_tris.push_back(Tri(0, 7, 10));
    m_tris.push_back(Tri(0, 10, 11));

    // 5 adjacent faces.
    m_tris.push_back(Tri(1, 5, 9));
    m_tris.push_back(Tri(5, 11, 4));
    m_tris.push_back(Tri(11, 10, 2));
    m_tris.push_back(Tri(10, 7, 6));
    m_tris.push_back(Tri(7, 1, 8));

    // 5 faces around point 3.
    m_tris.push_back(Tri(3, 9, 4));
    m_tris.push_back(Tri(3, 4, 2));
    m_tris.push_back(Tri(3, 2, 6));
    m_tris.push_back(Tri(3, 6, 8));
    m_tris.push_back(Tri(3, 8, 9));

    // 5 adjacent faces.
    m_tris.push_back(Tri(4, 9, 5));
    m_tris.push_back(Tri(2, 4, 11));
    m_tris.push_back(Tri(6, 2, 10));
    m_tris.push_back(Tri(8, 6, 7));
    m_tris.push_back(Tri(9, 8, 1));
}

template <typename T>
void SphericalImportanceSampler<T>::subdivide(const size_t subdivisions)
{
    PointCache point_cache;

    for (size_t s = 0; s < subdivisions; ++s)
    {
        const size_t tri_count = m_tris.size();

        std::vector<Tri> new_tris;
        new_tris.reserve(4 * tri_count);

        for (size_t t = 0; t < tri_count; ++t)
        {
            const Tri& tri = m_tris[t];

            const size_t a = get_or_create_middle_point(point_cache, tri.m_v0, tri.m_v1);
            const size_t b = get_or_create_middle_point(point_cache, tri.m_v1, tri.m_v2);
            const size_t c = get_or_create_middle_point(point_cache, tri.m_v2, tri.m_v0);

            new_tris.push_back(Tri(tri.m_v0, a, c));
            new_tris.push_back(Tri(tri.m_v1, b, a));
            new_tris.push_back(Tri(tri.m_v2, c, b));
            new_tris.push_back(Tri(a, b, c));
        }

        m_tris.swap(new_tris);
    }
}

template <typename T>
size_t SphericalImportanceSampler<T>::get_or_create_middle_point(PointCache& point_cache, size_t v0, size_t v1)
{
    if (v0 > v1)
        std::swap(v0, v1);

    const uint64 key = (static_cast<uint64>(v0) << 32) | v1;

    const PointCache::const_iterator it = point_cache.find(key);

    if (it != point_cache.end())
        return it->second;

    const Vector<T, 3>& p0 = m_verts[v0];
    const Vector<T, 3>& p1 = m_verts[v1];
    const Vector<T, 3> pm = normalize(p0 + p1);

    const size_t vm = m_verts.size();
    point_cache[key] = vm;
    m_verts.push_back(pm);

    return vm;
}

template <typename T>
void SphericalImportanceSampler<T>::build_cdf()
{
    const size_t tri_count = m_tris.size();
    m_cdf.reserve(tri_count);

    for (size_t i = 0; i < tri_count; ++i)
    {
        const Tri& tri = m_tris[i];

        const Vector<T, 3>& p0 = m_verts[tri.m_v0];
        const Vector<T, 3>& p1 = m_verts[tri.m_v1];
        const Vector<T, 3>& p2 = m_verts[tri.m_v2];

        // Compute the arc lengths of the sides of the spherical triangle.
        T a, b, c;
        compute_spherical_triangle_edge_lengths(p0, p1, p2, a, b, c);

        // Compute the interior angles of the spherical triangle.
        T alpha, beta, gamma;
        compute_spherical_triangle_interior_angles(a, b, c, alpha, beta, gamma);

        // Compute the area of the spherical triangle.
        const T area = compute_spherical_triangle_area(alpha, beta, gamma);

        m_cdf.insert(i, area);
    }

    m_cdf.prepare();
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_SAMPLING_SPHERICALIMPORTANCESAMPLER_H
