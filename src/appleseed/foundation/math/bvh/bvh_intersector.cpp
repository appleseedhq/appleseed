
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "bvh_intersector.h"

// appleseed.foundation headers.
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/math/fp.h"
#include "foundation/platform/sse.h"
#endif

namespace foundation {
namespace bvh {

namespace impl
{
#ifdef APPLESEED_FOUNDATION_USE_SSE

    int intersect_bvh_nodes(
        const Ray3d&        ray,
        const RayInfo3d&    ray_info,
        const AABB3f&       left_bbox,
        const AABB3f&       right_bbox,
        double              tmin[2],
        double              tmax[2])
    {
        const sse2d mposinf = set1pd(FP<double>::pos_inf());
        const sse2d mneginf = set1pd(FP<double>::neg_inf());

        // X slabs.

        ALIGN_SSE_VARIABLE const float bbx[4] =
        {
            left_bbox.min.x,
            right_bbox.min.x,
            left_bbox.max.x,
            right_bbox.max.x
        };

        const sse4f mbbx4f = loadps(bbx);
        const sse2d mbbminx2d = _mm_cvtps_pd(mbbx4f);
        const sse2d mbbmaxx2d = _mm_cvtps_pd(shuffleps(mbbx4f, mbbx4f, _MM_SHUFFLE(0, 0, 3, 2)));

        const sse2d mrox = set1pd(ray.m_org.x);
        const sse2d mrrcpdx = set1pd(ray_info.m_rcp_dir.x);
        const sse2d mx1 = mulpd(mrrcpdx, subpd(mbbminx2d, mrox));
        const sse2d mx2 = mulpd(mrrcpdx, subpd(mbbmaxx2d, mrox));

        sse2d mtmax = maxpd(minpd(mx1, mposinf), minpd(mx2, mposinf));
        sse2d mtmin = minpd(maxpd(mx1, mneginf), maxpd(mx2, mneginf));

        // Y slabs.

        ALIGN_SSE_VARIABLE const float bby[4] =
        {
            left_bbox.min.y,
            right_bbox.min.y,
            left_bbox.max.y,
            right_bbox.max.y
        };

        const sse4f mbby4f = loadps(bby);
        const sse2d mbbminy2d = _mm_cvtps_pd(mbby4f);
        const sse2d mbbmaxy2d = _mm_cvtps_pd(shuffleps(mbby4f, mbby4f, _MM_SHUFFLE(0, 0, 3, 2)));

        const sse2d mroy = set1pd(ray.m_org.y);
        const sse2d mrrcpdy = set1pd(ray_info.m_rcp_dir.y);
        const sse2d my1 = mulpd(mrrcpdy, subpd(mbbminy2d, mroy));
        const sse2d my2 = mulpd(mrrcpdy, subpd(mbbmaxy2d, mroy));

        mtmax = minpd(mtmax, maxpd(minpd(my1, mposinf), minpd(my2, mposinf)));
        mtmin = maxpd(mtmin, minpd(maxpd(my1, mneginf), maxpd(my2, mneginf)));

        // Z slabs.

        ALIGN_SSE_VARIABLE const float bbz[4] =
        {
            left_bbox.min.z,
            right_bbox.min.z,
            left_bbox.max.z,
            right_bbox.max.z
        };

        const sse4f mbbz4f = loadps(bbz);
        const sse2d mbbminz2d = _mm_cvtps_pd(mbbz4f);
        const sse2d mbbmaxz2d = _mm_cvtps_pd(shuffleps(mbbz4f, mbbz4f, _MM_SHUFFLE(0, 0, 3, 2)));

        const sse2d mroz = set1pd(ray.m_org.z);
        const sse2d mrrcpdz = set1pd(ray_info.m_rcp_dir.z);
        const sse2d mz1 = mulpd(mrrcpdz, subpd(mbbminz2d, mroz));
        const sse2d mz2 = mulpd(mrrcpdz, subpd(mbbmaxz2d, mroz));

        mtmax = minpd(mtmax, maxpd(minpd(mz1, mposinf), minpd(mz2, mposinf)));
        mtmin = maxpd(mtmin, minpd(maxpd(mz1, mneginf), maxpd(mz2, mneginf)));

        // Conclusion.

        const sse2d mraytmin = set1pd(ray.m_tmin);
        const sse2d mraytmax = set1pd(ray.m_tmax);

        const int hits =
            movemaskpd(
                orpd(
                    cmpgtpd(mtmin, mtmax),
                    orpd(
                        cmpltpd(mtmax, mraytmin),
                        cmpgepd(mtmin, mraytmax)))) ^ 3;

        if (hits)
        {
            storepd(tmin, maxpd(mraytmin, mtmin));
            storepd(tmax, minpd(mraytmax, mtmax));
        }

        return hits;
    }

#else

    int intersect_bvh_nodes(
        const Ray3d&        ray,
        const RayInfo3d&    ray_info,
        const AABB3f&       left_bbox,
        const AABB3f&       right_bbox,
        double              tmin[2],
        double              tmax[2])
    {
        int result = 0;

        if (intersect(ray, ray_info, AABB3d(left_bbox), tmin[0], tmax[0]))
            result |= 1;

        if (intersect(ray, ray_info, AABB3d(right_bbox), tmin[1], tmax[1]))
            result |= 2;

        return result;
    }

#endif  // APPLESEED_FOUNDATION_USE_SSE
}

}   // namespace bvh
}   // namespace foundation
